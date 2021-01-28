#!/usr/bin/perl
use Test::More;
use lib '../lib/';
#use Test::Pod::Coverage;
sub Pod::Coverage::TRACE_ALL () { 1 };
use Pod::Coverage;

my $test = Pod::Coverage->new(package => "ELF::File::SegmentEntry");
my @uncovered = $test->covered();
plan (tests => 1);
pod_coverage_ok(
    "ELF::File::SegmentEntry",
    {also_private => [ qr/^_/ ]},
    "ELF::File, with leading underscore functions as privates\n",
);
