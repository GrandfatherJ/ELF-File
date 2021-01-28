use strict;
use warnings;
use Config;

use Test::More tests => 1;

my $ivSize;

BEGIN {
    
    use lib '../../..';    # For 'CPAN' folder layout
    use lib '../lib';      # For dev folder layout

    $ivSize = $Config{ivsize};

    if ($^O ne 'MSWin32') {
        plan(skip_all => "Windows only module. Tests irrelevant on $^O");
    } elsif (!Win32::API->new("kernel32", 'LoadLibrary', "P", 'I')->Call('msi'))
    {
        plan(skip_all =>
                "msi.dll required - Windows Installer must be installed");
    } else {
        plan(tests => 90);
    }

    use_ok("ELF::File");
}


my $object = ELF::Reader->new ();
isa_ok ($object, 'ELF::Reader');


