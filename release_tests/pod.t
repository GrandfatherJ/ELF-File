#!/usr/bin/perl
use lib '../lib';
use Test::More;
use Test::Pod;

my @paths = ('../lib/ELF');

print "Parsing following paths for POD testing:\n   ",
    join "\n   ", @paths;
print "\n";

my @files = grep {/\.pm$/i} all_pod_files(@paths);
all_pod_files_ok(@files);
