package Language::Basic::Expression;
# Part of Language::Basic by Amir Karger (See Basic.pm for details)

=pod

=head1 NAME

Language::Basic::Expression - Package to handle string, numeric, and
conditional expressions. 

=head1 SYNOPSIS

See L<Language::Basic> for the overview of how the Language::Basic module
works. This pod page is more technical.

    # Given an LB::Token::Group, create an expression AND parse it
    my $exp = new LB::Expression::Arithmetic $token_group;
    # What's the value of the expression?
    print $exp->evaluate;
    # Perl equivalent of the BASIC expression
    print $exp->output_perl;

Expressions are basically the building blocks of Statements, in that every
BASIC statement is made up of keywords (like GOTO, TO, STEP) and expressions.
So expressions includes not just the standard arithmetic expressions
(like 1 + 2), but also lvalues (scalar variables or arrays), functions,
and constants. See the Syntax file included with the Language::Basic
distribution for details on the way expressions are built.

=head1 DESCRIPTION

Each subclass has (at least) three methods: 

=over 4

=item parse 

takes a Token::Group and eats one or more Tokens from it (or returns undef if
the string's syntax doesn't match that subclass), setting various fields
in the object.

=item evaluate

actually calculates the value of the expression. For a string
or numeric constant or variable, that just means taking the stored value
of that object. For other Expressions, you actually need to do math.

=item output_perl

Gives a string with the Perl equivalent to a BASIC expression. "1+2" is
converted to "1+2", but "A" becomes "$a", "A$" becomes "$a_str", and
function calls may be even more complicated.

=back

The Expression subclasses closely follow the BASIC grammar. Most subclasses
can have their own String and Numeric subclasses in turn. This allows us
to make sure that you don't try to multiply strings together, etc.

Expression subclasses:

=over 4

=cut

use strict;
use Language::Basic::Common;

# sub-packages
{
package Language::Basic::Expression::Conditional;
package Language::Basic::Expression::Arithmetic;
package Language::Basic::Expression::Multiplicative;
package Language::Basic::Expression::Unary;
package Language::Basic::Expression::Lvalue;
package Language::Basic::Expression::Arglist;
package Language::Basic::Expression::Function;
package Language::Basic::Expression::Constant;
}

#TODO pod for each class!

# new blesses an empty object to the class you're in, then parses
# the tokens (whose reference is given in arg1) to populate the object
# with different fields, depending on the class.
# The parse subroutine may well call other 'new' subs and return a whole
# hierarchy of LB::Expressions. Note that this sub is inherited by
# all the LB::Expression::* subclasses.
sub new {
    # important to use shift here, since parse passes any remaining @_
    my ($class, $token_group) = (shift, shift);
    my $self = {};
    bless $self, $class;
    # parse the expression, returns subclass (or undef for a problem)
    # Also pass any extra arguments that new got
    $self->parse($token_group, @_) or return undef; 
    return $self;
} # end sub Language::Basic::Expression::new

# An arithmetic expression is a LBE::Arithmetic::String if it's made up
# of LBE::Multiplicative::String expressions and so on. We never mix
# expression types (except within an argument list for an array or function.)
# This sub therefore blesses an object to its String or Numeric subclass
# depending on the type of the sub-expression (and returns the newly
# blessed object.)
# Arg0 is the thing to bless, arg1 is the subexp
sub string_or_numeric {
    my $self = shift;
    my $class = ref($self);
    # If we already are blessed, don't rebless!
    return $self if ref($self) =~ /::(String|Numeric)$/;

    my $subexp = shift;
    ref($subexp) =~ /::(String|Numeric)$/ or 
        die "Error refining $class to ",ref($subexp),"\n";
    my $type = $1;
    #print "self, class, type is 1 $self 2 $class 3 $type\n";
    # Note: "$class::$type" breaks!
    my $subclass = $class . "::$type";
    bless $self, $subclass;
}

######################################################################

=item Arithmetic

An arithmetic expression is a set of multiplicative expressions connected by
plus or minus signs. (String expressions can only be connected by plus,
which is the BASIC concatenation operator.)

=cut

{
package Language::Basic::Expression::Arithmetic;
@Language::Basic::Expression::Arithmetic::ISA = qw(Language::Basic::Expression);

sub parse {
# The while loop is necessary in case we have an expression like 1+2+3 
# It will effectively evaluate the +, - operators left to right 
    my $self = shift;
    my $token_group = shift;
    my (@adds, @ops);

    my $add = new Language::Basic::Expression::Multiplicative $token_group;
    push @adds, $add;
    while (defined (my $tok = 
	    $token_group->eat_if_class("Arithmetic_Operator"))) {
	push @ops, $tok->text;
	$add = new Language::Basic::Expression::Multiplicative $token_group;
	push @adds, $add;
    } # end while

    $self->{"expressions"} = \@adds;
    $self->{"operations"} = \@ops;
    # Bless to LBEA::String or Numeric
    $self->string_or_numeric($add);
} # end sub Language::Basic::Expression::Arithmetic::parse

package Language::Basic::Expression::Arithmetic::String;
@Language::Basic::Expression::Arithmetic::String::ISA = qw(Language::Basic::Expression::Arithmetic);

sub evaluate {
    my $self = shift;
    my @exps = @{$self->{"expressions"}};
    # Ops ought to be all pluses, since that's all BASIC can do.
    my @ops = @{$self->{"operations"}};

    my $add = (shift @exps)->evaluate;
    while (my $op = shift @ops) {
	my $add2 = (shift @exps)->evaluate;
	if ($op eq '+') {
	    $add .= $add2;
	} else {
	    die "Unknown op in LBE::Arithmetic::String::evaluate!\n";
	}
    } # end while
    return($add);
} # end sub Language::Basic::Expression::Arithmetic::String::evaluate

sub output_perl {
    my $self = shift;
    my @exps = @{$self->{"expressions"}};
    my @ops = @{$self->{"operations"}};

    my $ret = (shift @exps)->output_perl;
    while (my $op = shift @ops) {
	if ($op eq "+") {
	    my $exp = (shift @exps)->output_perl;
	    $ret .= " . " . $exp;
	} else {
	    die "Unknown op in LBE::Arithmetic::String::output_perl!\n";
	}
    } # end while
    return($ret);
} # end sub Language::Basic::Expression::Arithmetic::String::output_perl

package Language::Basic::Expression::Arithmetic::Numeric;
@Language::Basic::Expression::Arithmetic::Numeric::ISA = qw(Language::Basic::Expression::Arithmetic);

sub evaluate {
    my $self = shift;
    my @exps = @{$self->{"expressions"}};
    my @ops = @{$self->{"operations"}};

    my $add = (shift @exps)->evaluate;
    while (my $op = shift @ops) {
	my $add2 = (shift @exps)->evaluate;
	#print "eval_add: $add $op $add2\n";
	if ($op eq '+') {
	    $add = $add + $add2;
	} else { # minus
	    $add = $add - $add2;
	}
    } # end while
    return($add);
} # end sub Language::Basic::Expression::Arithmetic::Numeric::evaluate

sub output_perl {
    my $self = shift;
    my @exps = @{$self->{"expressions"}};
    my @ops = @{$self->{"operations"}};

    my $ret = (shift @exps)->output_perl;
    while (my $op = shift @ops) {
	my $exp = (shift @exps)->output_perl;
	$ret .= $op . $exp;
    } # end while
    return($ret);
} # end sub Language::Basic::Expression::Arithmetic::Numeric::output_perl

} # end package Language::Basic::Expression::Arithmetic

=item Multiplicative

a set of unary expressions connected by '*' or '/'.  A String multiplicative
expression is just a String unary expression.

=cut

{
package Language::Basic::Expression::Multiplicative;
@Language::Basic::Expression::Multiplicative::ISA = qw(Language::Basic::Expression);

sub parse {
# Note that String unary expressions can't multiply or divide, but it doesn't
# really matter
    my $self = shift;
    my $token_group = shift;
    my (@mults, @ops);

    my $mult = new Language::Basic::Expression::Unary $token_group;
    push @mults, $mult;
    while (defined (my $tok = 
	    $token_group->eat_if_class("Multiplicative_Operator"))) {
	push @ops, $tok->text;
	$mult = new Language::Basic::Expression::Unary $token_group;
	push @mults, $mult;
    } # end while

    $self->{"expressions"} = \@mults;
    $self->{"operations"} = \@ops;

    # Bless to LBEM::String or Numeric
    $self->string_or_numeric($mult);
} # end sub Language::Basic::Expression::Multiplicative::parse

sub evaluate {
    my $self = shift;
    my @exps = @{$self->{"expressions"}};
    my @ops = @{$self->{"operations"}};

    my $mult = (shift @exps)->evaluate;
    while (my $op = shift @ops) {
	my $mult2 = (shift @exps)->evaluate;
	#print "eval_mult: $mult $op $mult2\n";
	if ($op eq '*') {
	    $mult = $mult * $mult2;
	} else {
	    $mult = $mult / $mult2;
	}
    } # end while
    return($mult);
} # end sub Language::Basic::Expression::Multiplicative::evaluate

sub output_perl {
    my $self = shift;
    my @exps = @{$self->{"expressions"}};
    my @ops = @{$self->{"operations"}};

    my $ret = (shift @exps)->output_perl;
    while (my $op = shift @ops) {
	my $exp = (shift @exps)->output_perl;
	$ret .= $op . $exp;
    } # end while
    return($ret);
} # end sub Language::Basic::Expression::Multiplicative::output_perl

# Sub packages
package Language::Basic::Expression::Multiplicative::Numeric;
@Language::Basic::Expression::Multiplicative::Numeric::ISA = qw(Language::Basic::Expression::Multiplicative);
package Language::Basic::Expression::Multiplicative::String;
@Language::Basic::Expression::Multiplicative::String::ISA = qw(Language::Basic::Expression::Multiplicative);
} # end package Language::Basic::Expression::Multiplicative

=item Unary

a variable, a function, a string or numeric constant, or an arithmetic
expression in parentheses, potentially with a unary minus sign.

=cut

{
package Language::Basic::Expression::Unary;
@Language::Basic::Expression::Unary::ISA = qw(Language::Basic::Expression);
use Language::Basic::Common;

sub parse {
    my $self = shift;
    my $token_group = shift;

    my $unary;
    my $try;

    # unary minus in the expression?
    $self->{"minus"} = defined($token_group->eat_if_string("-"));

    # if a parentheses, (recursively) parse what's inside
    if (defined($token_group->eat_if_class("Left_Paren"))) {
	$unary = new Language::Basic::Expression::Arithmetic $token_group;
	# Skip End Paren
	defined($token_group->eat_if_class("Right_Paren")) or
	     Exit_Error("Expected ')' to match '('!");

    # OR it's a function
    # NOTE that LBEF::new had better not eat the word if it returns undef!
    } elsif (defined ($try = 
	    new Language::Basic::Expression::Function $token_group)) {
	$unary = $try;

    # OR it's a String or Numeric variable
    } elsif (defined ($try = 
	    new Language::Basic::Expression::Lvalue $token_group)) {
	$unary = $try;

    # OR it's a String or Numeric constant
    } elsif (defined ($try = 
	    new Language::Basic::Expression::Constant $token_group)) {
	$unary = $try;

    # Or die
    } else {
	my $tok = $token_group->lookahead or
	    Exit_Error("Found nothing when expected Unary Expression!");
        Exit_Error("Unknown unary expression starts with '", $tok->text,"'");
    }

    $self->{"expression"} = $unary;
    #print "unary ref is ",ref($unary),"\n";

    # Bless to LBEU::String or Numeric
    $self->string_or_numeric($unary);
} # end eval_unary

sub evaluate {
    my $self = shift;
    my $exp = $self->{"expression"};

    my $value = $exp->evaluate;
    $value = -$value if $self->{"minus"};
    return($value);
} # end sub Language::Basic::Expression::Unary::evaluate

sub output_perl {
    my $self = shift;
    my $ret = $self->{"minus"} ?  "-" : "";
    my $exp = $self->{"expression"};
    my $out = $exp->output_perl;
    if ($exp->isa("Language::Basic::Expression::Arithmetic")) {
        $out = "(" . $out . ")";
    }
    $ret .= $out;
    return($ret);
} # end sub Language::Basic::Expression::Unary::output_perl

# Sub packages
package Language::Basic::Expression::Unary::Numeric;
@Language::Basic::Expression::Unary::Numeric::ISA = qw(Language::Basic::Expression::Unary);
package Language::Basic::Expression::Unary::String;
@Language::Basic::Expression::Unary::String::ISA = qw(Language::Basic::Expression::Unary);
} # end package Language::Basic::Expression::Unary

######################################################################

=item Constant

a string or numeric constant, like "17" or 32.4

=cut

{
package Language::Basic::Expression::Constant;
@Language::Basic::Expression::Constant::ISA = qw(Language::Basic::Expression);

# Returns a LBE::Constant::* subclass or undef
# TODO this totally sucks. Because of the complicatedness of the new/parse
# scheme, we have to cannibalize the returned LBEC::* object and
# call string_or_numeric, rather than just returning that object.
# Maybe LBE::new should say "$self = $self->parse". That would require
# having all the other parse subs return self, but that's doable.
sub parse {
    my $self = shift;
    my $token_group = shift;
    my ($const, $try);
    if (defined ($try = 
	    new Language::Basic::Expression::Constant::Numeric $token_group)) {
	$const = $try;
    } elsif (defined ($try = 
	    new Language::Basic::Expression::Constant::String $token_group)) {
	$const = $try;
    } else {
        return undef;
    }

    $self->{"value"} = $const->{"value"};
    $self->string_or_numeric($const);
} # end Language::Basic::Expression::Constant::parse

sub evaluate {return shift->{"expression"}->evaluate; }

package Language::Basic::Expression::Constant::Numeric;
@Language::Basic::Expression::Constant::Numeric::ISA = qw(Language::Basic::Expression::Constant);

sub parse {
    my $self = shift;
    my $token_group = shift;
    if (defined (my $tok = 
            $token_group->eat_if_class("Numeric_Constant"))) {
	$self->{"value"} = $tok->text + 0;
    } else {
        return undef;
    }
    # Otherwise, if value is empty string, returning it confuses LBE::new
    return $self;
} # end sub Language::Basic::Expression::Constant::Numeric::parse

sub evaluate { return shift->{"value"} }

sub output_perl {return shift->{"value"}}

package Language::Basic::Expression::Constant::String;
@Language::Basic::Expression::Constant::String::ISA = qw(Language::Basic::Expression::Constant);

sub parse {
    my $self = shift;
    my $token_group = shift;
    if (defined (my $tok = 
            $token_group->eat_if_class("String_Constant"))) {
	(my $text = $tok->text) =~ s/^"(.*?)"/$1/;
	$self->{"value"} = $text;
    } else {
	# TODO handle unquoted string for Input, Data statements
	warn "Currently only understand quoted strings for String Constant";
        return undef;
    }
    # Otherwise, if value is empty string, returning it confuses LBE::new
    return $self;
} # end sub Language::Basic::Expression::Constant::String::parse

sub evaluate { return shift->{"value"} }

# Don't return in single quotes, because single quotes may be in a BASIC
# string constant. Instead use quotemeta. But don't really use quotemeta,
# because it quotes too much.
sub output_perl {
    my $self = shift;
    my $str = $self->{"value"};
    $str =~ s/([\$%@*&])/\\$1/g; # poor man's quotemeta
    return '"' . $str . '"';
} # end sub Language::Basic::Expression::Constant::String::output_perl

} # end package Language::Basic::Expression::Constant

######################################################################

=item Lvalue

a settable expression: a variable, X, or one cell in an array, A(17,Q). The
"variable" method returns the actual LB::Variable::Scalar object referenced by
this Lvalue.

=cut

{
package Language::Basic::Expression::Lvalue;
@Language::Basic::Expression::Lvalue::ISA = qw(Language::Basic::Expression);
use Language::Basic::Common;

# Sub-packages
{
package Language::Basic::Expression::Lvalue::Numeric;
@Language::Basic::Expression::Lvalue::Numeric::ISA = qw(Language::Basic::Expression::Lvalue);
package Language::Basic::Expression::Lvalue::String;
@Language::Basic::Expression::Lvalue::String::ISA = qw(Language::Basic::Expression::Lvalue);
}

# Fields:
#    varptr - ref to the LB::Variable (::Array or ::Scalar) object. Note
#        that it does NOT ref a particular cell in an LBV::Array object!
#    arglist - a set of Arithmetic Expressions describing which exact cell
#        in an LBV::Array to get. undef for a LBV::Scalar
sub parse {
    my $self = shift;
    my $token_group = shift;
    defined (my $tok = 
	    $token_group->eat_if_class("Identifier")) or
	    return undef;
    my $name = $tok->text;

    # read ( Arglist ) if it exists
    # By default, though, it's a scalar, and has no ()
    $self->{"arglist"} = undef;
    if (defined (my $arglist = 
	    new Language::Basic::Expression::Arglist $token_group)) {
	$self->{"arglist"} = $arglist;
    }

    # Look up the variable by name in the (Array or Scalar) variable storage.
    # (Also, create the Variable if it doesn't yet exist.)
    my $var = &Language::Basic::Variable::lookup($name, $self->{"arglist"});
    $self->{"varptr"} = $var;
    $self->{"name"} = $name;

    # Is it a string or numeric lvalue?
    $self->string_or_numeric($var);
} # end sub Language::Basic::Expression::Lvalue::parse

sub evaluate { 
    my $self = shift;
    # This automatically gets the correct array cell if necessary
    my $var = $self->variable;
    my $value = $var->value;
    return $value;
} # end sub Language::Basic::Expression::Lvalue::evaluate

# returns a variable, e.g. for setting in a Let or changing in a Next
# Note that it always returns a LB::Variable::Scalar object. If the
# variable in this expression is an Array, it returns one cell from the array.
sub variable { 
    my $self = shift;
    my $var = $self->{"varptr"};
    # if Arglist exists, evaluate each arith. exp. in it and get that cell
    # from the Array
    if (defined (my $arglist = $self->{"arglist"})) {
	my @args = $arglist->evaluate;
	$var = $var->get_cell(@args);
    }

    return $var;
} # end sub Language::Basic::Expression::Lvalue::variable

sub output_perl {
    my $self = shift;
    my $name = $self->{"name"};
    $name =~ s/\$$/_str/; # make name perl-like
    my $ret = '$' . lc($name);
    if (defined $self->{"arglist"}) {
	my $args = join("][", ($self->{"arglist"}->output_perl));
	$ret .= "[" . $args . "]";
    }
    return $ret;
} # end sub Language::Basic::Expression::Lvalue::output_perl

} # end package Language::Basic::Expression::Lvalue

######################################################################

=item Function

Either an Intrinsic or a User-Defined function.

=cut

#
# Fields:
#    function - ref to the LB::Function (::Intrinsic or ::Defined) used
#        by this expression
#    arglist - a set of Arithmetic Expressions describing the arguments
#        to pass to the function
{
package Language::Basic::Expression::Function;
@Language::Basic::Expression::Function::ISA = qw(Language::Basic::Expression);
use Language::Basic::Common;

# Sub-packages
{
package Language::Basic::Expression::Function::Numeric;
@Language::Basic::Expression::Function::Numeric::ISA = qw(Language::Basic::Expression::Function);
package Language::Basic::Expression::Function::String;
@Language::Basic::Expression::Function::String::ISA = qw(Language::Basic::Expression::Function);
}

# Arg0, Arg1 are the object and a ref to the string being parsed, as usual.
# Arg2, if it exists, says we're in a DEF statement, so that if the
# function doesn't exist, we should create it rather than returning undef.
sub parse {
    my $self = shift;
    my $token_group = shift;
    # Don't eat it if it's not a true function name (could be an lvalue)
    my $tok = $token_group->lookahead;
    return undef unless $tok->isa("Language::Basic::Token::Identifier");
    my $name = $tok->text;
    my $defining = (defined (my $exp = shift));

    # Look up the function name
    # If the function doesn't exist, the word is a variable or something...
    # Alternatively, if there was a second argument to parse, then we're
    # in a DEF statement & should create the function.
    my $func;
    if ($defining) {
	# TODO should this check be somewhere else, so that we can
	# give a more descriptive error message in Statement::Def::parse?
	return undef unless $name =~ /^FN/;
        $func = new Language::Basic::Function::Defined $name;
    } else {
	$func = &Language::Basic::Function::lookup($name) or return undef;
    }
    $self->{"function"} = $func;

    #Now that we know it's a function, eat the token
    $token_group->eat;

    # read ( Arglist )
    # TODO Actually, whether or not we're defining, we should just read
    # an LBE::Arglist here. If $defining, define() can make sure all args
    # are actually Lvalues containing Scalar Variables. However, this
    # requires that Arglist has Lvalues, rather than Arith. Exp.'s
    # containing (ME's containing...) Lvalues.
    if ($defining) {
	# Empty parens aren't allowed! (and \s* has been removed by lexing)
	defined($token_group->eat_if_class("Left_Paren")) or
	    Exit_Error("Function must take at least one argument.");
	my @args;
	do {
	    my $arg = new Language::Basic::Expression::Lvalue $token_group;
	    push @args, $arg;
	} while (defined $token_group->eat_if_string(","));
	defined($token_group->eat_if_class("Right_Paren")) or
	     Exit_Error("Expected ')' to match '('!");

        # Declare the number & type of args in the subroutine
	$func->declare (\@args);

    } else {
	my $arglist = new Language::Basic::Expression::Arglist $token_group
	    or Exit_Error("Function without arglist!");
	# check if the number or type of args is wrong.
	$func->check_args($arglist);
	$self->{"arglist"} = $arglist;
    }

    # Is it a string or numeric Function?
    $self->string_or_numeric($func);
} # end sub Language::Basic::Expression::Function::parse

sub evaluate { 
    my $self = shift;
    my $func = $self->{"function"};
    my $arglist = $self->{"arglist"};
    # Note we tested number & type of args in parse
    my @args = $arglist->evaluate;
    my $value = $func->evaluate(@args);
    return $value;
} # end sub Language::Basic::Expression::Function::evaluate

sub output_perl {
    my $self = shift;
    # Function name
    my $func = $self->{"function"};
    my $ret = $func->output_perl;
    # If it's either a user-defined function or a BASIC intrinsic (that
    # doesn't have a Perl equivalent), add a &
    if ($ret =~ /(fun|bas)$/) {$ret = '&' . $ret}

    # Function args
    $ret .= "(";
    
    my @args = $self->{"arglist"}->output_perl;
    $ret .= join(", ", @args);
    $ret .= ")";
    return $ret;
}

} # end package Language::Basic::Expression::Function

=item Arglist

a list of arguments to an array or function

=cut

{
package Language::Basic::Expression::Arglist;
@Language::Basic::Expression::Arglist::ISA = qw(Language::Basic::Expression);
use Language::Basic::Common;

sub parse {
    my $self = shift;
    my $token_group = shift;

    # Has to start with paren
    defined($token_group->eat_if_class("Left_Paren")) or
	return undef;
    # Eat args
    my @args = ();
    do {
	my $arg = new Language::Basic::Expression::Arithmetic $token_group;
	# TODO test that arg is a Scalar!
	push @args, $arg;
    } while (defined($token_group->eat_if_string(",")));

    # Has to end with paren
    defined($token_group->eat_if_class("Right_Paren")) or
	Exit_Error("Arglist without ')' at end!");
    unless (@args) {Exit_Error("Empty argument list ().")}

    $self->{"arguments"} = \@args;
} # end sub Language::Basic::Expression::Arglist::parse

# Returns a LIST of values
sub evaluate {
    my $self = shift;
    my @values = map {$_->evaluate} @{$self->{"arguments"}};
    return @values;
} # end sub Language::Basic::Expression::Arglist::evaluate

# Note this returns an ARRAY of args. Messes up the output_perl paradigm,but
# functions & arrays need to do different things to the args.
sub output_perl {
    my $self = shift;
    return map {$_->output_perl} @{$self->{"arguments"}};
} # end sub Language::Basic::Expression::Arglist::output_perl
} # end package Language::Basic::Expression::Arglist

######################################################################

=item Arglist

A conditional expression, like "A>B+C". Note that in BASIC, (unlike Perl or 
C) you can't change a conditional expression into an integer or string.

=cut

{
package Language::Basic::Expression::Conditional;
@Language::Basic::Expression::Conditional::ISA = qw(Language::Basic::Expression);
use Language::Basic::Common;

sub parse {
    my ($self, $token_group) = @_;
    my $e1 = new Language::Basic::Expression::Arithmetic $token_group or
        Exit_Error("Unexpected text at beginning of Cond. Exp.");

    defined (my $tok = $token_group->eat_if_class("Relational_Operator")) 
	or Exit_Error("No Rel. Op. in Conditional Exp.");
    my $op = $tok->text;

    my $e2 = new Language::Basic::Expression::Arithmetic $token_group or
        Exit_Error("Unexpected text in Cond. Exp. after '$op'");
    $self->{"exp1"} = $e1;
    $self->{"exp2"} = $e2;

    # Convert BASIC ops to perlops
    my $num_op = {
        "="  => "==",
	">"  => ">",
	"<"  => "<",
	">=" => ">=",
	"<=" => "<=",
	"<>" => "!=",
    };
    my $string_op = {
        "="  => "eq",
	">"  => "gt",
	"<"  => "lt",
	">=" => "ge",
	"<=" => "le",
	"<>" => "ne",
    };
    my $trans = (ref($e1) =~ /String/ ? $string_op : $num_op);
    my $perlop = $trans->{$op} or Exit_Error("Unrecognized Rel. op. '$op'");
    $self->{"op"} = $perlop;
    
    # Is conditional String or Numeric? Figure out from its exps
    Exit_Error("Expressions in conditional must be the same type!") unless
        ref($e1) eq ref($e2);
    $self->string_or_numeric($e1);
} # end sub Language::Basic::Expression::new

sub evaluate {
    my $self = shift;

    # Evaluate the two sides of the conditional (each is an expression--
    # either they're both arithmetic or they're both string).
    my $exp1 = $self->{"exp1"};
    my $exp2 = $self->{"exp2"};
    my $e1 = $exp1->evaluate;
    my $e2 = $exp2->evaluate;

    my $perlop = $self->{"op"};
    # I'm vainly hoping that Perl eval will get the same result BASIC would
    # Need to use \Q in case we say IF A$ = "\", which should really compare
    # with \\.
    my $perlexp = join ("", "\"\Q$e1\E\" ", $perlop . " \"\Q$e2\E\"");
    my $value = eval $perlexp;
    #print "exp is '$perlexp', value is '$value'\n";
    
    return $value;
} # end sub Language::Basic::Expression::Conditional::evaluate

sub output_perl {
    my $self = shift;
    my $exp1 = $self->{"exp1"};
    my $exp2 = $self->{"exp2"};
    my $e1 = $exp1->output_perl;
    my $e2 = $exp2->output_perl;

    my $perlop = $self->{"op"};
    my $ret = join(" ",$e1, $perlop, $e2);

    return($ret);
} # end sub Language::Basic::Expression::Conditional::output_perl

# Sub packages
package Language::Basic::Expression::Conditional::Numeric;
@Language::Basic::Expression::Conditional::Numeric::ISA = qw(Language::Basic::Expression::Conditional);
package Language::Basic::Expression::Conditional::String;
@Language::Basic::Expression::Conditional::String::ISA = qw(Language::Basic::Expression::Conditional);
} # end package Language::Basic::Expression::Conditional

=pod

=back

=cut

1; # end package Language::Basic::Expression