# Test script for Language::Basic.
# Uses tools from testbasic.pl, which allow it to run under Test::Harness
# $code is a BASIC program, $expected is its expected output (including \n's!)
# Call &setup_test for each $code,$expected pair.
# Then call &perform_tests at the end.

# TODO there's a lot more to be tested here!

# Include subs
push @INC, "t";
do 'testbasic.pl';

my ($code, $expected); # one program & its expected outpt

# Use single quotes because of "$" et al.
$code =<<'ENDCODE';
    10 print 2;
    20 print 1+1;
    30 print (1+1);
    40 print -(-3*4 / (2+2*2));
    50 a = 2;
    60 print -(-a*(a+a+a) / (a+a*a));
    70 print "2" + "2";
ENDCODE
$expected = "2 2 2 2 2 22";
&setup_test($code, $expected);

&perform_tests;
