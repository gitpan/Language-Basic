# Subs used by all the Language::Basic packages
package Language::Basic::Common;

use strict;
use vars qw(@ISA @EXPORT @EXPORT_OK);

BEGIN{
use Exporter ();
@ISA = qw(Exporter);
@EXPORT = qw(
    &Exit_Error
);
}

sub Exit_Error {
    my $err = shift;
    my $prog = &Language::Basic::Program::current_program;
    my $error_line = $prog->current_line_number;

    STDOUT->flush; # in case we're in the middle of a PRINT statement...
    warn "\nError in line $error_line: $err\n";
    exit (1);
}

1;
