package Language::Basic::Function;
# Part of Language::Basic by Amir Karger (See Basic.pm for details)

=pod

=head1 DESCRIPTION

Language::Basic::Function and its subclasses handle user-defined and
intrinsic Functions.

The define method defines the number and type of the function's arguments,
as well as what the function actually does.

The check_args method checks that the right number and type of function
arguments were input.

The evaluate method actually calculates the value of the function, given
certain arguments.

=cut

# Fields:
#     arg_types - a string. If a function takes a String and two Numeric
#         arguments, the string will be "SNN". Like in Perl, a semicolon
#         separates required from optional arguments

use strict;
use Language::Basic::Common;

# sub-packages
{
package Language::Basic::Function::Intrinsic;
package Language::Basic::Function::Defined;
}

# Lookup table for functions
my %Table;

# This sub puts the function in the lookup table
sub new {
    my ($class, $name) = @_;

    my $self = {} ;

    # Put this sub in lookup table
    $Table{$name} = $self;

    my $type = ($name =~ /\$$/) ? "String" : "Numeric";
    # Create a new subclass object, & return it
    my $subclass = $class . "::$type";
    bless $self, $subclass;
} # end sub Language::Basic::Function::new

# Lookup a function by name in the function table.
# This will (in theory) never be called before new has been called
# for function $name
sub lookup {
    my $name = shift;
    return $Table{$name};
} # end sub Language::Basic::Variable::lookup

# Check argument number and type
sub check_args {
    my ($self, $arglist) = @_;
    my @args = @{$arglist->{"arguments"}};
    # Test for several errors at once
    my $error = "";

    # Handle optional args
    my ($min_types, $max_types);
    my $types = $self->{"arg_types"};
    $min_types = length($types);
    $types =~ s/;// and $min_types = length($`);
    $max_types = length($types);

    $error .= ("Wrong number of arguments to function\n") 
        unless @args <= $max_types && @args >= $min_types;
    # Now check each argument type
    foreach my $type (split (//, $types)) {
	my $arg = shift @args or last; # may be optional args
	ref($arg) =~ /(String|Numeric)$/;
	my $atype = substr($&,0,1);
	if ($atype ne $type) {
	    $error .= $type eq "N" ?
		"String argument given, Numeric required.\n" :
		"Numeric argument given, String required.\n";
	}
    }
    chomp($error); # Exit_Error will add last \n back in.
    return $error;
} # end sub Language::Basic::Variable::check_args

######################################################################
# package Language::Basic::Function::Intrinsic
# Intrinsic BASIC functions
#
# Fields:
#     subroutine - a ref to a sub that implements the BASIC routine in Perl
#         (assuming the args are in @_)
{
package Language::Basic::Function::Intrinsic;
@Language::Basic::Function::Intrinsic::ISA = qw(Language::Basic::Function);
use Language::Basic::Common;

# Called at the beginning of the program to set the intrinsic functions up.
sub initialize {
    # Each value in the array is an arrayref.
    # Each arrayref has: name, string, subref.
    # The string is an N or S for each Numeric or String argument the
    # function takes.
    my @Init = (
	# Numeric functions...
	["ASC", "S", sub {ord(shift)} ],
	["INT", "N", sub {int(shift)} ],
	["LEN", "S", sub {length(shift)} ],
	["RND", "N", sub {rand()} ],
	["VAL", "S", sub {my $a=shift; return ($a =~ /[\d+-.]/ ? $a : 0)} ],

	# and String functions...
	['CHR$', "N", 
	    sub {
	        my $a=shift; 
		if ($a>127 || $a<0) {Exit_Error("Arg. to CHR\$ must be < 127")}
		chr($a);
	    } ],
	['MID$', "SN;N", 
	    sub {
	        my ($str, $index, $length) = @_;
		$index--; # BASIC strings index from 1!
		return (defined $length ?
		    substr($str, $index, $length) :
		    substr($str, $index) );
	    } ],
    );

    # Initialize intrinsic functions
    foreach (@Init) {
	my ($name, $arg_types, $subref) = @$_;
	my $func = new Language::Basic::Function::Intrinsic ($name);
	$func->define($arg_types, $subref);
    }
} # end sub Language::Basic::Function::Intrinsic::initialize

# This sub defines a function, i.e. says what it does with its arguments
sub define {
    # $subref is a sub ref which "translates" the BASIC function into Perl
    # arg_types is a string containing an N or S for each Numeric or String
    # argument the function takes
    my ($self, $arg_types, $subref) = @_;
    $self->{"subroutine"} = $subref;
    $self->{"arg_types"} = $arg_types;
} # end sub Language::Basic::Function::Intrinsic::define

sub evaluate {
    # Note that number & type of args has already been checked
    my ($self, @args) = @_;
    # Put this in an eval to find errors?
    return &{$self->{"subroutine"}} (@args);
} # end sub Language::Basic::Function::Intrinsic::evaluate

package Language::Basic::Function::Intrinsic::String;
@Language::Basic::Function::Intrinsic::String::ISA = qw(Language::Basic::Function::Intrinsic);
package Language::Basic::Function::Intrinsic::Numeric;
@Language::Basic::Function::Intrinsic::Numeric::ISA = qw(Language::Basic::Function::Intrinsic);
} # end package Language::Basic::Function::Intrinsic

######################################################################
# package Language::Basic::Function::Defined
# User-defined functions
#
# Fields:
#     variables - the function parameters. (LB::Variable::Scalar objects)
#     expression - an arithmetic expression. When the function parameters
#         are correctly set, evaluating this expression will yield the
#         value of the function
{
package Language::Basic::Function::Defined;
@Language::Basic::Function::Defined::ISA = qw(Language::Basic::Function);
use Language::Basic::Common;

# This sub defines a function, i.e. says what it does with its arguments
sub define {
    # $arglist is a ref to a list of LB::Variable::Scalars, which are the
    # arguments to the Function. (E.g., X in DEF FN(X))
    # $exp is an LB::Expression which, when evaluated on the arguments,
    # will implement the function
    my ($self, $arglistref, $exp) = @_;
    my $types; # Each arg is S (String) or N (Numeric)

    foreach my $arg (@$arglistref) {
        ref($arg) =~ /(String|Numeric)$/ or die "Error in LBF::Defined::define";
	$types .= substr($&,0,1);
    }
    $self->{"arg_types"} = $types;

    $self->{"variables"} = $arglistref;
    $self->{"expression"} = $exp;
} # end sub Language::Basic::Function::Defined::define

# Actually evaluate the function on its arguments
# Set each parameter (in "variables" field) to the value given in the
# arguments, then evaluate the expression.
# Just in case user has a function FN(X) and uses X elsehwere in the
# program, save the value of X just before we set X based on the argument.
# This is a poor man's version of variable scoping.
sub evaluate {
    # Note that number & type of args has already been checked
    my ($self, @args) = @_;

    my @save_vars;
    foreach my $var (@{$self->{"variables"}}) {
        my $arg = shift @args;
	push @save_vars, $var->value;
    }

    my $value = $self->{"expression"}->evaluate;

    # Now restore the values of the function parameters that we may have
    # changed.
    foreach my $var (@{$self->{"variables"}}) {
        my $save = shift @save_vars;
	$var->set($save);
    }

    return $value;
} # end sub Language::Basic::Function::Intrinsic::evaluate

package Language::Basic::Function::Defined::String;
@Language::Basic::Function::Defined::String::ISA = qw(Language::Basic::Function::Defined);
package Language::Basic::Function::Defined::Numeric;
@Language::Basic::Function::Defined::Numeric::ISA = qw(Language::Basic::Function::Defined);
} # end package Language::Basic::Function::Defined

1; # end package Language::Basic::Function
