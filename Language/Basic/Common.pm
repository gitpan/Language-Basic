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
    STDOUT->flush; # in case we're in the middle of a PRINT statement...
    warn "\nError in line ",&Language::Basic::Program::line_number,": $err\n";
    exit (1);
}

1;
