use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'Language::Basic',
    'VERSION_FROM' => 'Language/Basic.pm', # finds $VERSION
    'dist' => { COMPRESS => 'gzip', SUFFIX => 'gz'}, # use gzip, not compress
);
