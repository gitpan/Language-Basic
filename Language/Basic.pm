package Language::Basic;
# by Amir Karger (See below for copyright/license/etc.)

=pod

=head1 NAME

Language::Basic - Perl Module to interpret BASIC

=head1 SYNOPSIS

    use Language::Basic;

    my $Program = new Language::Basic::Program;
    $Program->input("program.bas"); # Read the lines from a file
    $Program->parse; # Parse the lines
    $Program->implement; # Run the program

The program basic.pl that comes with the module basically does the above.

=head1 DESCRIPTION

This module lets you run any BASIC programs you may have lying around, or
may inspire you to write new ones!

The aspects of the language that are supported are described below. Note
that I was pretty much aiming for Applesoft BASIC (tm) ca. 1985, not some
modern BASIC with real subroutines.


=cut

use strict;
require 5.004; # I use 'foreach my'
use IO::File;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK);

require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(
);

# Stolen from `man perlmod`
$VERSION = do { my @r = (q$Revision: 1.32 $ =~ /\d+/g); sprintf "%d."."%02d" x $#r, @r }; # must be all one line, for MakeMaker

# Sub-packages
use Language::Basic::Common;
use Language::Basic::Expression;
use Language::Basic::Function;
use Language::Basic::Statement;
use Language::Basic::Variable;

# sub-packages
{
package Language::Basic::Program;
package Language::Basic::Line;
}

######################################################################

=head2 Class Language::Basic::Program

This class handles a whole program. A Program is just a bunch of Lines,
each of which has one or more Statements on it. Running the program
involves moving through the lines, usually in numerical order, and
implementing each line.

Methods:

=over 4

=cut

{
package Language::Basic::Program;
# Parsing_If: some things change when you're parsing a THEN or ELSE,
# e.g., "30" means "GOTO 30"
use vars qw($Parsing_If);

# The programs (sorted) line numbers.
my @Line_Numbers;
# Counter and Next_Counter are indices in @Line_Numbers of the current
# and next line to be executed.
my ($Next_Counter, $Counter);
# The subroutine stack. In BASIC, it's just a list of line numbers we came from.
my @Stack;
# The data holder (stuff from DATA statements, read by READ statements)
my @Data;
# Functions whose perl-equivalent subs we need to print out at the end
# of the program
my %Needed_Subs;
# Indenting for outputted Perl
my $Output_Indent = 2; # eight spaces by default

sub new {
    my ($class, $infile) = @_;

    # Most of the time, we aren't parsing IF statements
    $Parsing_If = 0;

    #Initialize the intrinsic functions
    &Language::Basic::Function::Intrinsic::initialize();

    my $in = {
	# Keys are line numbers, values are LB::Line objects
	"lines" => {},
    };
    bless $in, $class;
} # end sub Language::Basic::Program::new

=item input

This method reads in a program from a file. It doesn't do any parsing,
except for taking the line number out of the line.

=cut

# Read the program in from file arg0
# TODO decide on the exact splitup between input/parse
# Should input literally just read the text?
# Rather, why not read the lines in from the file in the main program. (or
# have a LB::input_file sub that can do it too). Call LB::Program::parse with
# a reference to an array of lines of text (need not be chomped, etc.)
# Then in theory you could parse just one line, etc.
sub input {
    my ($self, $filename) = @_;
    my $fh = new IO::File $filename;
    die "Error opening $filename: $!\n" unless defined $fh;
    my $old_num = -1;

    while (<$fh>) {
        next if /^\s*$/; # empty lines
	chomp;

	# Line Number
	# no \n on die so perl gives chunk #
	s/^\s*(\d+)\s+// 
	    or die "Missing line number! Last line number read was $old_num.\n";
	my $line_num = $1;

	# Make sure lines are in numerical order. If they're not, it's most
	# likely a bug.
	# TODO if we ever create a real interpreter, we have to get rid of
	# this restriction
	if ($line_num <= $old_num) {
	    die "Line $line_num: lines must be in increasing order.\n";
	}

	# Create an LB::Line with what's left of the line
	$self->{"lines"}->{$line_num} = new Language::Basic::Line $_;
	$old_num = $line_num;
    }
    close ($fh);

    @Line_Numbers = sort {$a <=> $b} keys %{$self->{"lines"}};

} # end sub Language::Basic::Program::input

=item parse

This method parses the program, which just involves looping over the lines
in the program and parsing each line.

=cut

sub parse {
# This function takes lines of the program and "orders" them,
# then parses each one.
    my $self = shift;
    $Counter = 0;

    # Loop through the lines in the program
    while ($Counter <= $#Line_Numbers) {
        my $line = $self->{"lines"}->{$Line_Numbers[$Counter]};
	$line->parse;
	$Counter++;
    }
} # end sub Language::Basic::Program::parse

=item implement

This method actually runs the program. That is, it starts on the first line,
and implements lines one at a time, following line numbers in numerical
order unless a GOTO, NEXT, etc. sends it somewhere else. It stops when it
hits an END statement or "falls off" the end of the program.

=cut

sub implement {
    my $self = shift;
    # Start on the first line of the program
    $Counter = 0;
    # By default, increment lines normally
    undef $Next_Counter;

    # Loop over lines while there are lines
    # END statement sets Counter to -1. Falling off end of program also ends.
    while ($Counter>=0 && $Counter <= $#Line_Numbers) {
	# TODO create a "trace" command that prints out line numbers
	# for debugging
	#print "Line $Line_Numbers[$Counter]\n";
        my $line = $self->{"lines"}->{$Line_Numbers[$Counter]};
	$line->implement;
	&goto_next_line;
    }
} # end sub Language::Basic::Program::implement

# What line are we currently on?
sub line_number {
    return $Line_Numbers[$Counter];
}

# "Call a subroutine", i.e. push the current line number onto the calling stack
sub push_stack {
    my $num = &line_number;
    push @Stack, $num;
}

# "Return from a subroutine", i.e. pop the last calling line number 
# from the calling stack
sub pop_stack {
    return pop @Stack;
}

# Add a piece of data (i.e. parsing a DATA statement)
sub add_data {
    my $thing = shift;
    push @Data, $thing;
}

# Get a piece of data (i.e. implementing a READ statement)
sub get_data {
    my $thing = shift @Data or 
        Exit_Error("More items READ than input in DATA!");
    return $thing;
}

# Go to the next line. If Next_Counter is set, go there. Otherwise, go to
# the next line in numerical order.
sub goto_next_line {
    if (defined $Next_Counter) {
        $Counter = $Next_Counter;
    } else {
        $Counter++;
    }

    # By default, next time we just increment the line number
    undef $Next_Counter;
}

# Which line do we move to after doing the current line?
sub set_next_line {
    my $next_line = shift;

    # Special case: END statement causes this
    unless (defined $next_line) { $Next_Counter = -1; return; }

    # We could do a binary search here, but not worth it for tens
    # or hundreds of lines
    foreach (0..$#Line_Numbers) {
	$Next_Counter = $_, last if $Line_Numbers[$_] == $next_line;
    }
    Exit_Error("Can't find line $next_line!") unless defined $Next_Counter;
} # end sub Language::Basic::Program::set_next_line

# Just like set_next_line, except go the line *after* that line.
# E.g., RETURN from a GOSUB, you want to return to the GOSUB line
# but start execution after that line. Same with FOR.
sub set_after_next_line {
    my $next_line = shift;
    &set_next_line($next_line);
    $Next_Counter++;
}

sub output_perl {
# This function takes lines of the program and outputs each as perl
    my $self = shift;
    my $sep = '#' x 78;
    $Counter = 0;

    # Beginning of the program
    print '#!/usr/bin/perl -w';
    print "\n#Translated from BASIC by basic2pl\n\n";

    if (@Data) {
	print "$sep\n# Setup\n#\n";
	print "# Read data\n";
        print "while (<DATA>) {chomp; push \@Data, \$_}\n\n";
    }

    # Loop through the lines in the program
    print "$sep\n# Main program\n#\n";
    while ($Counter <= $#Line_Numbers) {
	my $line_num = $Line_Numbers[$Counter];
        my $line = $self->{"lines"}->{$line_num};
	my $label = "L$Line_Numbers[$Counter]:";
	my $spaces_per_indent = 4;
	my $out = $label . $line->output_perl;
	# Print labels all the way against the left edge of the line,
	# then indent the rest of the line.
	# Split with -1 so final \n's don't get ignored
	foreach (split (/\n/, $out, -1)) {
	    # Change indenting for next time?
	    $Output_Indent += 1, next if $_ eq "INDENT";
	    $Output_Indent -= 1, next if $_ eq "UNINDENT";
	    warn "weird indenting $Output_Indent\n" if $Output_Indent < 2;

	    # If we didn't hit an indent-changing command, print the
	    # label (if any) and the actual string
	    $label = (s/^A?L\d+:// ? $& : "");
	    # minus for left justify
	    my $indent = -$Output_Indent * $spaces_per_indent; 
	    printf("%*s", $indent, $label);

	    # print the actual string
	    print $_;
	    print "\n"; # the \n we lost from split, or the last \n
	}
	$Counter++;
    }

    print "\n$sep\n# Subroutine Definitions\n#\n" if %Needed_Subs;
    # Print out required subroutines
    foreach (sort keys %Needed_Subs) {
        print (join(" ", "sub", $_, $Needed_Subs{$_}));
	print " # end sub $_\n\n";
    }

    # If there were any DATA statements...
    if (@Data) {
        print "\n\n$sep\n# Data\n#\n__DATA__\n";
	print join("\n", map {$_->output_perl} @Data);
	print "\n";
    }
} # end sub Language::Basic::Program::output_perl

sub need_sub {
    my ($func_name, $func_desc) = @_;
    return if exists $Needed_Subs{$func_name};
    $Needed_Subs{$func_name} = $func_desc;
}

} # end package Language::Basic::Program

######################################################################
# package Language::Basic::Line
# A line in a BASIC program

{
package Language::Basic::Line;

# Make a new LB::Line with the text given (don't parse it yet)
sub new {
    my $class = shift;
    my $text = shift;
    my $in = {
	# literal text of the line (not including line number)
	"text" => $text,
	# Pointer to first LB::Statement on the line
	"first_statement" => 0,
    };
    bless $in, $class;
} # end sub Language::Basic::Line::new

# Parse a line and the statements within it
# First the lines are broken into tokens (and whitespace is removed,
# except in strings)
# Then the statement is parsed, depending on what sort of command it is.
sub parse {
    my $line = shift;
    my @tokens = $line->lex;

    # Create the new Statement and figure out what kind of statement it is.
    # $statement will be an object of a subclass LB::Statement::*)
    my $statement = new Language::Basic::Statement \@tokens;
    $line->{"first_statement"} = $statement;

    # Parse the statement, depending on what subclass it is.
    $statement->parse;
}

# Return a list of tokens
# TODO if we're smart about reading expressions, we won't need whitespace
# around, e.g., "TO" in FOR statements.

sub lex {
    my $line = shift;
    my $text = $line->{"text"};
    # Separate string constants from everything else
    my @pieces = split(/(".*?")/, $text);
    my @tokens = map {
	if (/^"/) {
	    $_;
	} else {
	    # Get rid of whitespace, and upcase everything but strings
	    map {uc} split;
	}
    } @pieces;

    #print ("Tokens are '",join("', '",@tokens),"'\n");
    return @tokens;
}

# Implement one line of the program
sub implement {
    my $self = shift;
    #print "$Language::Basic::Program::Counter ", $self->{"text"},"\n";
    my $Curr_Statement = $self->{"first_statement"};

    # If there's more than one statement on the line, we have to do them
    # all. Statement::implement will return 0 if there aren't any more.
    while ($Curr_Statement) {
	#print "Statement class ",ref($Curr_Statement),"\n";
	# Hooray for OO; just call "implement" on everything!
	# implement should return non-zero if there's another statement
	# on this line.
	$Curr_Statement = $Curr_Statement->implement;
    }
} # end sub Language::Basic::Line::implement

sub output_perl {
    my $self = shift;
    my $statement = $self->{"first_statement"};
    # TODO loop over statements in the line?

    # Output the statement
    my $out = $statement->output_perl;
    return $out;
} # end sub Language::Basic::Line::output_perl

} # end package Language::Basic::Line


# end package Language::Basic
1;

__END__
# More Docs

=head1 BASIC LANGUAGE REFERENCE

This is a (hopefully current) description of what Language::Basic supports.
For each command, I give an example use of that command, and possible
a comment or two about it.

Also see the Syntax file included in the distribution, which describes
the exact syntax for each statement, expressions, variable names, etc.

=head2 Commands

=over 4

=item DATA

DATA 1,2,"HI". These will be read sequentially by READ statements. Note
that currently all string constants must be quoted.

=item DEF

DEF FNA(X)= INT(X + .5). 

=item DIM

DIM A(20), B(10,10). Arrays default to size 10 (or actually 11 since they
start at zero.)

=item END

END.

=item FOR

FOR I = 1 TO 10 STEP 3. STEP defaults to 1 if not given, and may be negative.
(For loops are always implemented at least once.)

=item GOTO

GOTO 30. Note that GOTO 30+(X*3) is also supported.

=item GOSUB

GOSUB 10+X. Gosub is just like GOTO, except that when the program gets to
a RETURN statement, it will come back to the statement just after the GOSUB.

=item IF

IF X > Y THEN 30 ELSE X = X + 1. ELSE is not required. In a THEN or ELSE,
a lone number means GOTO that number (also known as an implied GOTO).
AND/OR/NOT still aren't supported for conditional expressions.

=item INPUT

INPUT A$, B$. Also allowed is INPUT "FOO"; BAR. This prints "FOO?" instead of
just "?" as the input prompt.

=item LET

LET X=4. The word "LET" isn't required; i.e. X=4 is just like LET X=4.

=item NEXT

NEXT I. Increment I by STEP, test against its limit, go back to the FOR
statement if it's not over its limit.

=item ON

ON X-3 GOSUB 10,20. This is equivalent to: 
 IF X-3 = 1 THEN GOSUB 10
 IF X-3 = 2 THEN GOSUB 20
ON ... GOTO is also allowed.

=item PRINT

PRINT FOO; BAR$, 6*BLAH. semicolon means no space (or one space after printing
numbers!), comma is like a 14-character tab (or \n past column 56).
Print \n after the last expression unless there's a semicolon after it.

=item READ

READ A, B(I), C$. Reads data from DATA statements into variables

=item REM

REM WHATEVER. Anything after the REM is ignored.

=head2 Intrinsic functions

The following functions are currently supported:

Numeric Functions: INT (like Perl's int), RND (rand), ASC (ord), 
LEN (length), VAL (turn a string into a number; in Perl you just + 0 :))

RND just calls Perl's rand; you can't seed it or anything.

String functions: CHR$, MID$

=back

=head2 Overall Coding Issues

=over 4

=item * 

Hopefully your code doesn't have many bugs, because there isn't
much error checking.

=item *

Everything except string constants is converted to upper case, so 'a' and 'A'
are the same variable. (But note that the string "Yes" <> "YES", at least
for now.)

=item *

Spaces are (currently) required around various pieces of the program, like
THEN, ELSE, GOTO. That is, GOTO20 won't work. This may or may not change
in the future.

=back

=head1 AUTHOR

Amir Karger (akarger@cpan.org)

=head1 COPYRIGHT

Copyright (c) Amir Karger 2000

=head1 LICENSE

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 HISTORY

BASIC stands for Beginner's All-purpose Symbolic Instruction Code. Since it
was considered pretty hot stuff in the early 80's, it's the first language that
I and a lot of folks my age learned, so it holds a special place in my heart.
Which is the only reason I spent so many hours writing an interpreter for it
long after it was superseded by real interpreted languages that had subroutines
and didn't rely quite so much on GOTO.

I originally wrote this interpreter in C, as the final project for my first
C programming class in college. Its name at that point was COMPLEX, which
stood for "C-Oriented Major Project which did not use LEX".

When I learned Perl, I felt like its string handling capabilities would be
much better for an interpreter, so eventually I ported and expanded it. 
(Incidentally, I was right. I had surpassed the original program's
functionality in less than a week, and I was able to run wumpus in 2.)

A big goal for the Perl port is to support enough of the language that I can
run wumpus, another legacy from my childhood.  The interpreter's name could be
changed from COMPLEX to "Perl Eclectic Retro interpreter which did not use
Parse::LEX", or PERPLEX, but I settled for Language::Basic instead.

=head1 SEE ALSO

perl(1), wump(6)

=cut
