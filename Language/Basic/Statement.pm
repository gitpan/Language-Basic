package Language::Basic::Statement;

# Part of Language::Basic by Amir Karger (See Basic.pm for details)

=pod

=head1 NAME

Language::Basic::Statement - Module to handle parsing and implementing single
BASIC statements. 

=head1 STATEMENT OVERVIEW

See L<Language::Basic> for the overview of how the Language::Basic module
works. This pod page is more technical.

Take a program like:

 5 LET A = 2

 10 IF A >= 3 THEN GOTO 20 ELSE PRINT "IT'S SMALLER"

Line 5 has just one statement. Line 10 actually contains three. The first
is an IF statement, but the results of the THEN and the ELSE are entire
statements.

Each type of statement in BASIC has an associated LB::Statement class.
For example, there's LB::Statement::Let and LB::Statement::If. (But no
LB::Statement::Then! Instead the "then" field of the LB::Statement::If
object will point to another statement. In the above program, it would
point to a LB::Statement::Goto.)

Parsing a line of BASIC starts with removing the line number. After that,
LB::Statement::new is called with a ref to the text on the line.

LBS::new simply creates an LBS object. However, it then calls LBS::refine,
which looks at the first word of the command and blesses the object to
the correct subclass.

Each LBS subclass then has a parse routine, which is called with a ref to
text, and an implement routine. The parse routine goes through the text and
digests it (a ref is passed so that the next statement knows where to start
parsing, e.g., in an IF/THEN), and sets various fields in the object.
The implement routine uses the fields to implement the BASIC command.

=cut

use strict;
use Language::Basic::Common;

# sub-packages
{
package Language::Basic::Statement::Data;
package Language::Basic::Statement::Def;
package Language::Basic::Statement::Dim;
package Language::Basic::Statement::End;
package Language::Basic::Statement::For;
package Language::Basic::Statement::Gosub;
package Language::Basic::Statement::Goto;
package Language::Basic::Statement::If;
package Language::Basic::Statement::Input;
package Language::Basic::Statement::Let;
package Language::Basic::Statement::Next;
package Language::Basic::Statement::On;
package Language::Basic::Statement::Print;
package Language::Basic::Statement::Read;
package Language::Basic::Statement::Rem;
package Language::Basic::Statement::Return;
}

# Valid FORTRAN statements
my @Keywords = qw (DATA DEF DIM END FOR GOSUB GOTO IF INPUT 
    LET NEXT ON PRINT READ REM RETURN);

# Note: This sub first blesses itself to be class LB::Statement, but then
# class LB::Statement::refine, which blesses the object to a subclass
# depending on what sort of statement it is. The refined object is returned.
#
# Fields:
#     tokens - array of tokens (chars, words, etc.) in this statement
#          Note that this is a temporary array. It gets eaten during parsing.
#     next_statement - pointer to next Statment on this Line. (or 0)
#
#     lvalue - an LB::Expression::Lvalue object, which represents an
#          expression like X or AR(3+Q), which can be on the left hand
#          side of an assignment statement
#     expression - an LB::Expression:: subclass (e.g., Arithmetic or
#          Conditional.) Sometimes there are multiple expressions.
sub new {
    my $class = shift;
    my $tokref = shift;
    my $self = {
	"tokens" => $tokref,
	"next_statement" => 0,
    };

    bless $self, $class;
    $self->refine;
} # end sub Language::Basic::Statement::new

# Refine LB::Statement to the correct subclass
# I.e., Read the command this statement starts with, and bless the
# Statement to be a new subclass
sub refine {
    my $self = shift;
    my $tokref = $self->{"tokens"};

    # First word is a command, or a variable (implied LET statment)
    my $text = ${$tokref}[0];
    my $command;
    if (grep {$_ =~ /^\Q$text\E$/} @Keywords) {
	# Remove the command from the token list
        shift @{$tokref};
	$command = $text;
    } elsif ($text =~ /^[A-Z]\w*\$?/) {
        $command = "LET";
    # If we're in a THEN or ELSE, a line number means GOTO that line
    } elsif ($Language::Basic::Program::Parsing_If && $text =~ /^\d+$/) {
        $command = "GOTO";
    } else {
        Exit_Error("Syntax Error: No Keyword or Variable found!");
    }
    my $subclass = "Language::Basic::Statement::" . ucfirst(lc($command));
    #print "New $subclass Statement\n";

    bless $self, $subclass;
} # end sub Language::Basic::Statement::refine

# By default, parsing does nothing. Useful, e.g., for REM
sub parse { }

# By default, implementing does nothing. Useful, e.g., for REM
sub implement {
    return shift->{"next_statement"};
}

# Concatenate tokens until you get to a splitting char.
# Input: LB::Statement object, and "regexp" ("=" or "[,;]" to split on.
# Output: Two strings. First is the catted tokens until the matching string,
# second is the matching string.
# (Note that object's token array gets eaten.)
sub cat_until_match {
    my ($self, $matcher) = @_;
    my $tokref = $self->{"tokens"};

    # String constants always come in their own token.
    my $total = "";
    my $match = undef;
    while (defined (my $tok = shift @$tokref)) {
	#print "tok is '$tok'\n";

	# Watch out for ;, in string constants!
        if ($tok !~ /^"/) {
	    # Copy up through a comma
	    # TODO this won't work once we have multidimensional arrays!
	    if ($tok =~ /$matcher/) {
		($tok, $match) = ($`, $&);
		unshift @$tokref, $' if length($'); # anything left? Try next time
	    }
	}

	# Add what we read to what we already have
	$total .= $tok;

	#print "total is '$total'\n";
	return ($total, $match) if defined $match;
    } # end while

    return ($total, undef); # didn't find match
} # end sub Language::Basic::Statement::cat_until_match

######################################################################
# package Language::Basic::Statement::Data
# A DATA statement in a BASIC program.
{
package Language::Basic::Statement::Data;
@Language::Basic::Statement::Data::ISA = qw(Language::Basic::Statement);

sub parse {
    my $self = shift;
    my $tokref = $self->{"tokens"};

    # The rest of the line is things to dim and how big to dim them
    my $rest = join("", @$tokref);
    do {
	my $exp = new Language::Basic::Expression::Constant \$rest;
	&Language::Basic::Program::add_data($exp);
    } while $rest =~ s/,//;
} # end sub Language::Basic::Statement::Data::parse

# no sub implement nec.
} # end package Language::Basic::Statement::Data

######################################################################
# package Language::Basic::Statement::Def
# A DEF statement in a BASIC program.
{
package Language::Basic::Statement::Def;
@Language::Basic::Statement::Def::ISA = qw(Language::Basic::Statement);
use Language::Basic::Common;

sub parse {
    my $self = shift;
    my $tokref = $self->{"tokens"};

    my $rest = join("", @$tokref);
    # Function name (and args) is stuff up to equals
    $rest =~ s/^(.*?)=// or Exit_Error("DEF missing '='!");
    my $fdesc = $1;
    Exit_Error("Function names must start with 'FN'!") unless $fdesc =~ /^FN/;

    # The rest of the line is a function definition
    my $exp = new Language::Basic::Expression::Arithmetic \$rest;

    # Second argument forces 'new' to create a new Function, instead
    # of returning undef since the function doesn't exist
    my $func = new Language::Basic::Expression::Function(\$fdesc, $exp)
	    or Exit_Error("Incorrect DEF!");
    $self->{"function"} = $func;
} # end sub Language::Basic::Statement::Def::parse

# No sub implement: definition happens at compile time
} # end package Language::Basic::Statement::Def

######################################################################
# package Language::Basic::Statement::Dim
# A DIM statement in a BASIC program.
{
package Language::Basic::Statement::Dim;
@Language::Basic::Statement::Dim::ISA = qw(Language::Basic::Statement);

sub parse {
    my $self = shift;
    my $tokref = $self->{"tokens"};

    # The rest of the line is things to dim and how big to dim them
    my $rest = join("", @$tokref);
    do {
	my $exp = new Language::Basic::Expression::Lvalue \$rest;
	push @{$self->{"arrays"}}, $exp;
    } while $rest =~ s/,//;
} # end sub Language::Basic::Statement::Dim::parse

sub implement {
    my $self = shift;
    foreach (@{$self->{"arrays"}}) {
	# The Lvalue's Array
        my $array = $_->{"varptr"};
	my @indices = $_->{"arglist"}->evaluate;
	$array->dimension(@indices);
    }

    return $self->{"next_statement"};
} # end sub Language::Basic::Statement::Dim::implement

} # end package Language::Basic::Statement::Dim

######################################################################
# package Language::Basic::Statement::End
# An END statement in a BASIC program.
{
package Language::Basic::Statement::End;
@Language::Basic::Statement::End::ISA = qw(Language::Basic::Statement);
use Language::Basic::Common;

sub implement {
    # TODO Exit more gracefully?
    &Language::Basic::Program::set_next_line(undef);
    return 0; # don't do any more statements, either
} # end sub Language::Basic::Statement::End::implement

} # end package Language::Basic::Statement::End

######################################################################
# package Language::Basic::Statement::For
# A FOR statement in a BASIC program.
{
package Language::Basic::Statement::For;
@Language::Basic::Statement::For::ISA = qw(Language::Basic::Statement);
use Language::Basic::Common;

sub parse {
    my $self = shift;
    my $tokref = $self->{"tokens"};

    # Until the token "TO", we're copying a variable (or array cell)
    # an equals, and the variable's initialization
    my ($text, $tok) = $self->cat_until_match('^TO$');
    Exit_Error("FOR missing 'TO'!") unless defined $tok;

    # Read variable name
    my $lvalue = new Language::Basic::Expression::Lvalue \$text
	    or Exit_Error("Missing variable in FOR!");
    # No strings allowed, at least for now
    if ($lvalue->isa("Language::Basic::Expression::Lvalue::String")) {
        Exit_Error("FOR statements can't use strings!");
    }
    $self->{"lvalue"} = $lvalue;
    $text =~ s/^=// or Exit_Error("FOR missing '='!");

    # Read initialization value
    $self->{"start"} = 
        new Language::Basic::Expression::Arithmetic::Numeric \$text
	or Exit_Error("Missing/Bad initialization expression in FOR!");

    # Until the token "step" OR the end of the line, we're copying an
    # expression, namely the variable's increment
    ($text, $tok) = $self->cat_until_match('^STEP$');
    $self->{"limit"} = 
        new Language::Basic::Expression::Arithmetic::Numeric \$text
	or Exit_Error("Missing/Bad limit expression in FOR!");

    # If there's anything left, it had better be a step...
    # Otherwise, step = 1
    my $step = 1; # default value of step
    if (defined $tok) {
	$step = join("", @$tokref);
    }
    $self->{"step"} = 
        new Language::Basic::Expression::Arithmetic::Numeric \$step
	or Exit_Error("Missing/Bad step expression in FOR!");
} # end sub Language::Basic::Statement::For::parse

sub implement {
    my $self = shift;
    my $lvalue = $self->{"lvalue"};
    my $var = $lvalue->variable;
    $var->set($self->{"start"}->evaluate);
    # TODO should really have set_limit, set_step subs. OR put this info in
    # the For object!
    $var->{"limit"} = $self->{"limit"}->evaluate;
    $var->{"step"} = $self->{"step"}->evaluate;
    $var->{"goto_line"} = &Language::Basic::Program::line_number();
    # TODO Should check whether we're higher than the limit here before
    # doing the loop once. (Although it *is* accurate BASIC.)

    return $self->{"next_statement"};
} # end sub Language::Basic::Statement::For::implement

} # end package Language::Basic::Statement::For

######################################################################
# package Language::Basic::Statement::Gosub
# A GOSUB statement in a BASIC program.
{
package Language::Basic::Statement::Gosub;
@Language::Basic::Statement::Gosub::ISA = qw(Language::Basic::Statement);
use Language::Basic::Common;

sub parse {
    my $self = shift;
    my $tokref = $self->{"tokens"};

    # The rest of the line is an expression for the line to go to
    my $rest = join("", @$tokref);
    $self->{"expression"} = new Language::Basic::Expression::Arithmetic \$rest;
} # end sub Language::Basic::Statement::Gosub::parse

sub implement {
    my $self = shift;
    my $goto = $self->{"expression"}->evaluate;
    if ($goto !~ /^\d+$/) {Exit_Error("Bad GOSUB: $goto")}
    # Push the current line number onto the subroutine stack;
    &Language::Basic::Program::push_stack;
    # Then GOTO the new line
    &Language::Basic::Program::set_next_line($goto);

    return 0; # no more statements on this line, since we GOTO'ed a new line
} # end sub Language::Basic::Statement::Gosub::implement

} # end package Language::Basic::Statement::Gosub

######################################################################
# package Language::Basic::Statement::Goto
# A GOTO statement in a BASIC program.
{
package Language::Basic::Statement::Goto;
@Language::Basic::Statement::Goto::ISA = qw(Language::Basic::Statement);
use Language::Basic::Common;

sub parse {
    my $self = shift;
    my $tokref = $self->{"tokens"};

    # The rest of the line is an expression for the line to go to
    my $rest = join("", @$tokref);
    $self->{"expression"} = new Language::Basic::Expression::Arithmetic \$rest;
} # end sub Language::Basic::Statement::Goto::parse

# Note that this sub allows "GOTO X+17/3", not just "GOTO 20"
sub implement {
    my $self = shift;
    my $goto = $self->{"expression"}->evaluate;
    if ($goto !~ /^\d+$/) {Exit_Error("Bad GOTO: $goto")}
    &Language::Basic::Program::set_next_line($goto);

    return 0; # no more statements on this line, since we GOTO'ed a new line
} # end sub Language::Basic::Statement::Goto::implement

} # end package Language::Basic::Statement::Goto

######################################################################
# package Language::Basic::Statement::If
# An IF statement in a BASIC program.
{
package Language::Basic::Statement::If;
@Language::Basic::Statement::If::ISA = qw(Language::Basic::Statement);
use Language::Basic::Common;

sub parse {
    my $self = shift;
    my $tokref = $self->{"tokens"};

    # Until the token "then", we're copying a conditional expression
    my ($exp, $tok) = $self->cat_until_match('^THEN$');
    Exit_Error("IF MISSING 'THEN'!") unless defined $tok;
    $self->{"condition"} = new Language::Basic::Expression::Conditional \$exp;

    # "THEN 20" means THEN GOTO 20. This variable lets the parsing routines
    # know that.
    $Language::Basic::Program::Parsing_If = 1;

    # Until the token "else" OR the end of the line, is a new statement
    # expression, namely the variable's increment
    my $t1 = [];
    while (defined ($tok = shift @$tokref)) {
	last if $tok eq "ELSE";
        push @$t1, $tok;
    }
    my $then = new Language::Basic::Statement $t1;
    $then->parse;
    $self->{"then_s"} = $then;

    # If there's anything left, it's the else
    my $t2 = [];
    while (defined ($tok = shift @$tokref)) {
	last if $tok eq "ELSE";
        push @$t2, $tok;
    }
    if (@$t2) {
	# Use up all the leftover tokens
	my $else = new Language::Basic::Statement $t2;
	$else->parse ($tokref);
	$self->{"else_s"} = $else;
    } else {
        $self->{"else_s"} = 0;
    }

    # Go back to usual parsing. I.e., a statement containing just a number
    # is an error, not an implied GOTO
    $Language::Basic::Program::Parsing_If = 0;
} # end sub Language::Basic::Statement::If::parse

sub implement {
    my $self = shift;

    if ($self->{"condition"}->evaluate) {
        return $self->{"then_s"};
    } else {
	# This may be zero, in which case, code will just continue to next line
        return $self->{"else_s"};
    }
} # end sub Language::Basic::Statement::If::implement

} # end package Language::Basic::Statement::If

######################################################################
# package Language::Basic::Statement::Input
# An INPUT statement in a BASIC program.
{
package Language::Basic::Statement::Input;
@Language::Basic::Statement::Input::ISA = qw(Language::Basic::Statement);
use Language::Basic::Common;

sub parse {
    my $self = shift;
    my $tokref = $self->{"tokens"};

    my $rest = join("",@$tokref);

    # Handle INPUT "FOO"; BAR, BLAH
    # TODO I should really just try to call LBE::Constant::String and not
    # do anything if it returns undef. But currently that warns that what
    # we're trying to input isn't a quoted string if there's not quotation
    # mark.
    if ($rest =~ /^"/) {
        my $prompt = new Language::Basic::Expression::Constant::String \$rest;
        $self->{"to_print"} = $prompt;
	$rest =~ s/^;// or Exit_Error("Expected ; after INPUT prompt!");
    }

    # The rest of the inputs will be separated by commas
    do {
	my $exp = new Language::Basic::Expression::Lvalue \$rest
	    or Exit_Error("Incorrect INPUT!");
	push @{$self->{"lvalues"}}, $exp;
    } while $rest =~ s/^,//;
} # end sub Language::Basic::Statement::Input::parse

sub implement {
    my $self = shift;
    TRY_AGAIN:
    my $to_print = (exists $self->{"to_print"} ? 
        $self->{"to_print"}->evaluate :
	"");
    print "$to_print? ";
    # Use "EXTRA IGNORED?" to let user know they need to quote commas?
    my $in = <>;
    chomp($in);
    # TODO read Constants (String or Numeric) followed by commas if nec.
    # TODO type checking: make sure a string is a string
    # (this might be done by a different part of the program)
    my @ins = split(/\s*,\s*/, $in);
    foreach (@{$self->{"lvalues"}}) {
	my $var = $_->variable; # LB::Variable object
	# TODO Print "??" if they don't input enough. 
	my $value = shift @ins;
	if (!defined $value) {
	    print "Not enough inputs! Try whole statement again...\n";
	    # Can't have a BASIC interpreter without a GOTO!
	    goto TRY_AGAIN;
	}
	$var->set($value);
    }

    return $self->{"next_statement"};
} # end sub Language::Basic::Statement::Input::implement

} # end package Language::Basic::Statement::Input

######################################################################
# package Language::Basic::Statement::Let
# A LET statement in a BASIC program.
{
package Language::Basic::Statement::Let;
@Language::Basic::Statement::Let::ISA = qw(Language::Basic::Statement);
use Language::Basic::Common;

sub parse {
    my $self = shift;
    my $tokref = $self->{"tokens"};

    # Read thing to set and the "=" after it
    my $rest = join("",@$tokref);
    my $lvalue = new Language::Basic::Expression::Lvalue \$rest
	    or Exit_Error("Missing variable in LET!");
    $self->{"lvalue"} = $lvalue;
    $rest =~ s/^=// or Exit_Error("LET missing '='!");

    # The rest of the line is an expression to set the variable equal to
    $self->{"expression"} = new Language::Basic::Expression::Arithmetic \$rest
	    or Exit_Error("Missing right side expression in LET!");
} # end sub Language::Basic::Statement::Let::parse

sub implement {
    my $self = shift;
    my $lvalue = $self->{"lvalue"};
    my $var = $lvalue->variable;
    my $value = $self->{"expression"}->evaluate;
    $var->set($value);

    return $self->{"next_statement"};
} # end sub Language::Basic::Statement::Let::implement

} # end package Language::Basic::Statement::Let

######################################################################
# package Language::Basic::Statement::Next
# A NEXT statement in a BASIC program.
{
package Language::Basic::Statement::Next;
@Language::Basic::Statement::Next::ISA = qw(Language::Basic::Statement);
use Language::Basic::Common;

sub parse {
    my $self = shift;
    my $tokref = $self->{"tokens"};

    my $desc = shift @$tokref;
    my $lvalue = new Language::Basic::Expression::Lvalue \$desc
	    or Exit_Error("Incorrect FOR!");
    # No strings allowed, at least for now
    if (ref($lvalue->variable) =~ /String/) {
        Exit_Error("FOR statements can't use strings!");
    }
    $self->{"lvalue"} = $lvalue;
} # end sub Language::Basic::Statement::Next::parse

# Note: goto_line gets set to zero when we exit the loop. That way, if we
# GOTO into the middle of a loop, we'll get an error.
sub implement {
    my $self = shift;
    my $lvalue = $self->{"lvalue"};
    my $var = $lvalue->variable;
    my $value = $var->value;
    my ($limit,$step,$goto) = 
        map {$var->{$_}} qw (limit step goto_line);
    Exit_Error("NEXT without FOR!") unless $goto;

    # Increment
    $value += $step;
    $var->set($value);
    #print "next: '$value' '$limit' '$step' '$goto'\n";

    #test
    my $done = ($step > 0 ?  $value > $limit : $value < $limit);
    if ($done) {
	$var->{"goto_line"} = 0;
	return $self->{"next_statement"};
    } else {
	# Go to the line *after* the line the FOR started on
        &Language::Basic::Program::set_after_next_line($goto);
	return 0; # no more statements on this line, since we GOTO'ed a new line
    }
} # end sub Language::Basic::Statement::Next::implement

} # end package Language::Basic::Statement::Next

######################################################################
# package Language::Basic::Statement::On
# An ON statement in a BASIC program.
{
package Language::Basic::Statement::On;
@Language::Basic::Statement::On::ISA = qw(Language::Basic::Statement);
use Language::Basic::Common;

sub parse {
    my $self = shift;
    my $tokref = $self->{"tokens"};

    # Until the token "GOSUB/GOTO", we're copying an arithmetic expression
    my ($exp, $type) = $self->cat_until_match('^GO(SUB|TO)$');
    $self->{"type"} = $type or Exit_Error("ON missing GOSUB/GOTO!");
    $self->{"expression"} = new Language::Basic::Expression::Arithmetic \$exp;

    my $rest = join("", @$tokref);
    # The rest of the inputs will be separated by commas
    do {
	my $exp = new Language::Basic::Expression::Arithmetic::Numeric \$rest
	    or Exit_Error("Incorrect Expression in ON ... $type!");
	push @{$self->{"gotos"}}, $exp;
    } while $rest =~ s/^,//;
} # end sub Language::Basic::Statement::On::parse

sub implement {
    my $self = shift;
    my $type = $self->{"type"};
    my $value = $self->{"expression"}->evaluate;
    if ($value !~ /^\d+$/ || $value > @{$self->{"gotos"}}) {
        Exit_Error("Bad value in ON: $value")
    }

    my $goto = ${$self->{"gotos"}}[$value-1]->evaluate;
    if ($goto !~ /^\d+$/) {Exit_Error("Bad GOTO in ON: $goto")}
    &Language::Basic::Program::set_next_line($goto);

    # And if it's a GOSUB, push the program stack so we can get back
    &Language::Basic::Program::push_stack if $type eq "GOSUB";

    return 0; # no more statements on this line, since we GOTO'ed a new line
} # end sub Language::Basic::Statement::On::implement

} # end package Language::Basic::Statement::On

######################################################################
# package Language::Basic::Statement::Print
# A PRINT statement in a BASIC program.
{
package Language::Basic::Statement::Print;
@Language::Basic::Statement::Print::ISA = qw(Language::Basic::Statement);
use Language::Basic::Common;

my $Column; # column on the screen we are about to print to

sub parse {
    my $self = shift;
    my $tokref = $self->{"tokens"};
    push @$tokref, '""' unless @$tokref; # empty print statement

    my $rest = join("",@$tokref);
    my $endchar;
    do {
	my $exp = new Language::Basic::Expression::Arithmetic \$rest;
	if ($rest =~ s/[,;]//) {
	    $endchar = $&;
	} else {
	    $endchar = "";
	}
	push @{$self->{"to_print"}}, [$exp , $endchar];

    } while $endchar && $rest;

} # end sub Language::Basic::Statement::Print::parse

sub implement {
    # TODO More than one expression to print! Use an array of LB::Expressions
    my $self = shift;
    foreach my $thing (@{$self->{"to_print"}}) {
	my ($exp, $endchar) = @$thing;
	my $string = $exp->evaluate;
	print $string;
	$Column += length($string);
	if ($endchar eq ",") {
	    # Paraphrased from a BASIC manual:
	    # If the printhead (!) is at char 56 or more after the expression,
	    # print \n, else print spaces until the printhead is at the
	    # beginning of the next 14-character field
	    if ($Column >= 56) {
	        print "\n";
		$Column = 0;
	    } else {
		my $c = 14 - $Column % 14;
		print (" " x $c);
		$Column += $c;
	    }
	} elsif ($endchar eq ";") {
	    # In BASIC, you always print a space after numbers, but not
	    # after strings. That seems a bit dumb, but that's how it is.
	    if (ref($exp) =~ /::Numeric$/) {
	        print " ";
		$Column++;
	    }
	} else {
	    print "\n";
	    $Column = 0;
	}
    } # end foreach loop over expressions to print

    return $self->{"next_statement"};
} # end sub Language::Basic::Statement::Print::implement

} # end package Language::Basic::Statement::Print

######################################################################
# package Language::Basic::Statement::Read
# A READ statement in a BASIC program.
{
package Language::Basic::Statement::Read;
@Language::Basic::Statement::Read::ISA = qw(Language::Basic::Statement);

sub parse {
    my $self = shift;
    my $tokref = $self->{"tokens"};

    # The rest of the line is things to dim and how big to dim them
    my $rest = join("", @$tokref);
    do {
	my $exp = new Language::Basic::Expression::Lvalue \$rest;
	push @{$self->{"lvalues"}}, $exp;
    } while $rest =~ s/,//;
} # end sub Language::Basic::Statement::Read::parse

sub implement {
    my $self = shift;
    foreach (@{$self->{"lvalues"}}) {
        my $var = $_->variable;
	my $data = &Language::Basic::Program::get_data();
	# Data will just be a LBE::Constant, but we still have to &evaluate it
	my $value = $data->evaluate;
	$var->set($value);
    }

    return $self->{"next_statement"};
} # end sub Language::Basic::Statement::Read::implement

} # end package Language::Basic::Statement::Read

######################################################################
# package Language::Basic::Statement::Rem
# A REM statement in a BASIC program.
{
package Language::Basic::Statement::Rem;
@Language::Basic::Statement::Rem::ISA = qw(Language::Basic::Statement);
} # end package Language::Basic::Statement::Rem

######################################################################
# package Language::Basic::Statement::Return
# A RETURN statement in a BASIC program.
{
package Language::Basic::Statement::Return;
@Language::Basic::Statement::Return::ISA = qw(Language::Basic::Statement);
use Language::Basic::Common;

# No need to have a sub parse

sub implement {
    my $gosub = &Language::Basic::Program::pop_stack or
        Exit_Error("RETURN without GOSUB");
    # Start at the line *after* the GOSUB line
    &Language::Basic::Program::set_after_next_line($gosub);

    return 0; # no more statements on this line, since we GOTO'ed a new line
} # end sub Language::Basic::Statement::Return::implement

} # end package Language::Basic::Statement::Return

1; # end of package Language::Basic::Statement
