package Language::Basic;
# by Amir Karger (See below for copyright/license/etc.)

=pod

=head1 NAME

Language::Basic - Perl Module to interpret BASIC

=head1 SYNOPSIS

    use Language::Basic;

    my $Program = new Language::Basic::Program;
    $Program->input("program.bas"); # Read lines from a file
    $Program->parse; # Parse the Program
    $Program->implement; # Run the Program
    $Program->output_perl; # output Program as a Perl program

    $Program->line("20 PRINT X"); # add one line to existing Program

Featured scripts:

=over 4

=item basic.pl

Runs BASIC programs from the command line.

=item termbasic.pl

Term::Readline program. Input one line of BASIC at a time, then run the
program.

=item basic2pl.pl

Outputs a Perl program that does the same thing as the input BASIC program.

=back

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
$VERSION = do { my @r = (q$Revision: 1.35 $ =~ /\d+/g); sprintf "%d."."%02d" x $#r, @r }; # must be all one line, for MakeMaker

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
use Language::Basic::Common;

# Fields:
# lines		Keys are line numbers, values are LB::Line objects
# curr_line	Current line number in the program
# next_line	The line we're going to implement after this one
# first_line	First line in the program
# stack		The subroutine stack. In BASIC, it's just a list of line 
# 		numbers we came from.
# data		The data holder (stuff from DATA statements, read by READ)
# parsed	Has this Program been parsed since the last time 
#		new lines were added?
# needed_subs	Functions whose perl-equivalent subs we need to print out 
#		at the end of the program. (Keys are names of subs, values
#		are sub descriptions.)
# column	Current column of the screen the program is printing to
sub new {
    my ($class, $infile) = @_;

    #Initialize the intrinsic functions
    &Language::Basic::Function::Intrinsic::initialize();

    my $in = {
	"lines" => {},
        "cur_line" => undef,
        "next_line" => undef,
        "first_line" => undef,
        'stack' => [],
        'data' => [],
        'parsed' => 0,
	"needed_subs" => {},
	"column" => 0,
    };
    bless $in, $class;
} # end sub Language::Basic::Program::new

=item Current_Program

Returns the program currently being parsed/implemented/whatever

=item Set_Current_Program

Sets arg0 to be the Current Program

=cut

my $_Current_Program;
sub Current_Program {
    return $_Current_Program;
}
sub Set_Current_Program {
    my $self = shift or die "LBP::Set_Current_Program must have argument!\n";
    $_Current_Program = $self;
}

=item input

This method reads in a program from a file. It doesn't do any parsing,
except for taking the line number out of the line.

=cut

sub input {
    my ($self, $filename) = @_;
    $self->Set_Current_Program;
    my $fh = new IO::File $filename;
    die "Error opening $filename: $!\n" unless defined $fh;
    my $old_num = -1;

    while (<$fh>) {
        next if /^\s*$/; # empty lines
	chomp;

	# Line Number
	my $line_num = $self->_add_line($_);
	defined $line_num
	    or die "Missing line number " .
		($old_num > 0 ? "after line $old_num\n" : "on first line\n");

	# In input files, we make sure lines are in numerical order. 
	# If they're not, it's most likely a bug.
	# Same is not true for a Term::Readline interpreter
	if ($line_num <= $old_num) {
	    die "Line $line_num: lines in file must be in increasing order.\n";
	}

	$old_num = $line_num;
    }
    close ($fh);

    # order the lines
    $self->_fix_lines;
    $self->{'parsed'} = 0;

} # end sub Language::Basic::Program::input

=item line

This method takes a line of BASIC (arg1, already chomped), forms a new LB::Line
with it, and adds it to the Program (arg0). It doesn't do any parsing,
except for taking the line number out of the line.

=cut

sub line {
    my $self = shift;
    $self->Set_Current_Program;
    my $line = shift; # sans \n

    defined $self->_add_line($line) or die "Missing line number in line()!\n";
    $self->_fix_lines;
    $self->{'parsed'} = 0;
} # end sub Language::Basic::Program::line

sub _add_line {
    # takes the line (sans \n), returns the line number read or undef if there
    # is none.
    # You must call _fix_lines between _add_line and returning to the
    # user's program!

    my $self = shift;
    my $line = shift;

    # Line Number
    $line =~ s/^\s*(\d+)\s+// or return;
    my $line_num = $1;

    # Create an LB::Line with what's left of the line
    $self->{'lines'}{$line_num} = new Language::Basic::Line $line;

    return $line_num;
} # end sub Language::Basic::Program::_add_line

# fix the ordering of the lines in the program
sub _fix_lines {
    my $self = shift;

    my @line_numbers = sort {$a <=> $b} keys %{$self->{"lines"}};

    for (my $i = 0; $i < @line_numbers - 1; $i++) { # process all but last
	my $line = $self->{'lines'}{ $line_numbers[$i] };
	$line->set_next( $line_numbers[ $i+1 ] );
    } 

    $self->{'lines'}{ $line_numbers[-1] }->set_next( undef );

    $self->{'first_line'} = $line_numbers[0];
} # end sub Language::Basic::Program::_fix_lines 

=item parse

This method parses the program, which just involves looping over the lines
in the program and parsing each line.

=cut

sub parse {
# This function takes lines of the program and "orders" them,
# then parses each one.
    my $self = shift;
    $self->Set_Current_Program;

    return if $self->{'parsed'};

    $self->{'curr_line'} = $self->{'first_line'};

    # Loop through the lines in the program
    while (defined $self->{'curr_line'}) {
        my $line = $self->{"lines"}->{ $self->{'curr_line'} };
	
	$line->parse;
	$self->{'curr_line'} = $line->get_next;
    }

    $self->{'parsed'} = 1;
} # end sub Language::Basic::Program::parse

=item implement

This method actually runs the program. That is, it starts on the first line,
and implements lines one at a time, following line numbers in numerical
order unless a GOTO, NEXT, etc. sends it somewhere else. It stops when it
hits an END statement or "falls off" the end of the program.

=cut

sub implement {
    my $self = shift;
    $self->Set_Current_Program;
    # In case you're lazy & call implement w/out parsing first
    $self->parse unless $self->{'parsed'};

    # Zero stack, etc., start at beginning of program
    $self->start;

    # Loop over lines while there are lines
    # curr_line will be undef for END statements or when you "fall off" the
    # end of the program
    while (defined $self->{'curr_line'}) {
	# TODO create a "trace" command that prints out line numbers
	# for debugging
	#print "Line ",$self->{"curr_line"},"\n";
        my $line = $self->{"lines"}->{ $self->{'curr_line'} };
	
	# By default, go on to next line in numerical order after this one
	# (self->{'next_line'} may be changed while implementing the line!)
	$self->{'next_line'} = $line->get_next;

	# Implement the line!
	$line->implement;
	
	# Advance!
	$self->{'curr_line'} = $self->{'next_line'};
    }

} # end sub Language::Basic::Program::implement

=item start

This method erases program stack and moves line pointer to beginning of program

It should be called any time we start going through the program.
(Either implement or output_perl.)

=cut

# Don't erase "data". It's set during parsing.
sub start {
    my $self = shift;
    $self->{"stack"} = [];
    $self->{"column"} = 0;

    # Start on the first line of the program
    $self->{'curr_line'} = $self->{'first_line'};
} # end sub Language::Basic::Program::start

=item line_number

What line are we currently on?

=cut

sub line_number { return shift->{"curr_line"}; }

=item push_stack

"Call a subroutine", i.e. push the current line number onto the Program's
calling stack

=cut

sub push_stack {
    my $self = shift;
    my $num = $self->line_number;
    push @{ $self->{'stack'} }, $num;
}

# "Return from a subroutine", i.e. pop the last calling line number 
# from the calling stack
sub pop_stack {
    my $self = shift;
    return pop @{ $self->{'stack'} };
}

=item add_data

Add a piece of data to the Program's data storage, to be accessed
later. (i.e. parsing a DATA statement)

=cut

sub add_data {
    my $self = shift;
    my $thing = shift;
    push @{ $self->{'data'} }, $thing;
}

=item get_data

Get a piece of data that was stored earlier. (i.e. implementing a READ
statement)

=cut

sub get_data {
    my $self = shift;
    @{ $self->{'data'} } or Exit_Error("More items READ than input in DATA!");
    my $thing = shift @{ $self->{'data'} }; 
    return $thing;
}

=item set_next_line

Which line number do we move to after doing the current line?

=cut

sub set_next_line {
    my $self = shift;
    my $next_line = shift;
    $self->{'next_line'} = $next_line;
    
    Exit_Error("Can't find line $next_line!") 
	if defined $next_line and not defined $self->{'lines'}{ $next_line };
} # end sub Language::Basic::Program::set_next_line

=item set_after_next_line

Just like set_next_line, except go the line *after* that line.
E.g., RETURN from a GOSUB, you want to return to the GOSUB line
but start execution after that line. Same with FOR.

=cut

sub set_after_next_line {
    my $self = shift;
    my $next_line = shift;
    my $next_obj  = $self->{'lines'}{ $next_line };
    $next_line = $next_obj->get_next if defined $next_obj;
    $self->set_next_line($next_line);
} # end sub Language::Basic::Program::set_after_next_line

sub output_perl {
# This function takes lines of the program and outputs each as perl
    my $self = shift;
    $self->Set_Current_Program;
    # In case you're lazy & call implement w/out parsing first
    $self->parse unless $self->{'parsed'};

    my $sep = '#' x 78;
    # TODO these variables should be changeable by switches to basic2pl!
    my $spaces_per_indent = 4;
    # Indenting for outputted Perl
    my $Output_Indent = 2; # eight spaces by default

    # Beginning of the program
    print '#!/usr/bin/perl -w';
    print "\n#Translated from BASIC by basic2pl\n\n";

    if (@{$self->{"data"}}) {
	print "$sep\n# Setup\n#\n";
	print "# Read data\n";
        print "while (<DATA>) {chomp; push \@Data, \$_}\n\n";
    }

    # Zero program stack, etc., start at beginning of program
    $self->start;

    # Loop through the lines in the program
    print "$sep\n# Main program\n#\n";
    while (defined $self->{'curr_line'}) {
	my $line_num = $self->{"curr_line"};
	#warn "Line $line_num\n";
        my $line = $self->{"lines"}->{ $self->{'curr_line'} };
	my $label = "L$line_num:";

	# What's the line?
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

	# Go through lines in order
	$self->{'curr_line'} = $line->get_next;
    }

    my $n = $self->{"needed_subs"};
    print "\n$sep\n# Subroutine Definitions\n#\n" if %$n;
    # Print out required subroutines
    foreach (sort keys %$n) {
        print (join(" ", "sub", $_, $n->{$_}));
	print " # end sub $_\n\n";
    }

    # If there were any DATA statements...
    if (@{$self->{"data"}}) {
        print "\n\n$sep\n# Data\n#\n__DATA__\n";
	print join("\n", map {$_->output_perl} @{$self->{"data"}});
	print "\n";
    }
} # end sub Language::Basic::Program::output_perl

=item need_sub

Tells the Program that it needs to use the sub named arg0 (whose definition
is in arg1). This is used for outputting a Perl translation of a BASIC
program, so that you only write "sub mid_str {...}" if MID$ is used in
the BASIC program.

=back

=cut

sub need_sub {
    my $self = shift;
    my $n = $self->{"needed_subs"};
    my ($func_name, $func_desc) = @_;
    return if exists $n->{$func_name};
    $n->{$func_name} = $func_desc;
} # end sub Language::Basic::Program::need_sub

} # end package Language::Basic::Program

######################################################################

=head2 Class Language::Basic::Line

This class handles one line of a BASIC program, which has one or more
Statements on it.

Methods:

=over 4

=cut

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
	# number of next line (accessed with set/get_next)
	'next_line' => undef,
    };
    bless $in, $class;
} # end sub Language::Basic::Line::new

=item get_next

Returns the next line number in the Program

=item set_next

Sets the next line number in the Program to be arg1.

=cut

sub get_next { return shift->{'next_line'}; }

sub set_next {
    my $self = shift;
    my $next = shift;

    $self->{'next_line'} = $next;
} # end sub Language::Basic::Line::set_next

=item parse

This method breaks the line up into tokens (and removes whitespace, except in
strings), then parses the first statement on the line. (Eventually, it
will parse all statements on the line, but colons aren't currently supported.)

=cut

sub parse {
    my $self = shift;
    my @tokens = $self->lex;

    # Create the new Statement and figure out what kind of statement it is.
    # $statement will be an object of a subclass LB::Statement::*)
    my $statement = new Language::Basic::Statement \@tokens;
    $self->{"first_statement"} = $statement;

    # TODO parse multiple statements on one line!
    # Parse the statement, depending on what subclass it is.
    $statement->parse;
}

=item lex

This method breaks the line up into tokens. Currently that means it
separates by whitespace (which is discarded) and equals signs. (The = split
isn't necessary but it's convenient.) Anything in double quotation marks is its
own token, and no splitting is done within it.

=cut

# TODO if we're smart about reading expressions, we won't need whitespace
# around, e.g., "TO" in FOR statements.
sub lex {
    my $self = shift;
    my $text = $self->{"text"};
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

=item implement

This method actually executes the line. That is, it starts with the first
statement on the line, and performs it. If there's another statement to
implement on this line (e.g., an IF/THEN, or multiple statements
separated by colons (when that's supported)) then LB::Statement::implement
returns that Statement, which in turn is implemented.

=back

=cut

sub implement {
    my $self = shift;
    #print "$Language::Basic::Program::Counter ", $self->{"text"},"\n";
    my $curr_statement = $self->{"first_statement"};

    # If there's more than one statement on the line, we have to do them
    # all. Statement::implement will return 0 if there aren't any more.
    while ($curr_statement) {
	#print "Statement class ",ref($curr_statement),"\n";
	# Hooray for OO; just call "implement" on everything!
	# implement should return non-zero if there's another statement
	# on this line.
	$curr_statement = $curr_statement->implement;
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

=item *

When you use basic.pl (&LB::Program::input), the lines in the input file must
be in numerical order. When using termbasic.pl (&LB::Program::line), this
rule doesn't apply.

=back


=head1 BUGS

This is an alpha release and likely contains many bugs; these are merely
the known ones.

If you use multiple B<Language::Basic::Program> objects in a Perl program,
functions and variables can leak over from one to another.

It is possible to get some Perl warnings; for example, if you input a string
into a numerical variable and then do something with it.

B<PRINT> and so forth all go to the select-ed output handle; there really 
ought to be a way to set for a B<Program> the output handle.

There needs to be better and consistent error handling, and a more
extensive test suite.

=head1 AUTHOR

Amir Karger (akarger@cpan.org)

David Glasser gave ideas and feedback, hunted down bugs, and sent in a major
patch to help the LB guts.

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
