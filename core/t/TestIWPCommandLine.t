=pod
 
=head1 DESCRIPTION
 
This module tests IWPCommandline.
(c) Corvideon Limited 2016
Seamus Brady <seamus@corvideon.ie>
 
=cut

package TestIWPCommandline;

use v5.22;
use strict;
use warnings;
use autodie;
use FindBin;
use lib "$FindBin::Bin/../lib";
use IWPCommandline;
use Test::More tests => 4;

my $iwpCLI = IWPCommandline->new();

# Test 1

ok( defined $iwpCLI, "IWPCommandline is defined.");

# Test 2

ok( $iwpCLI->isa("IWPCommandline"),  "IWPCommandline is the correct class.");

# Test 3

my %the_args = (
        "arg1" => "test1",
        "arg2" => "test2",
);

$iwpCLI->args(\%the_args);
my $arg_number =  scalar keys %the_args;

ok( $arg_number = $iwpCLI->args(),  "IWPCommandline contains the correct no. of args.");


# Test 4

ok( $iwpCLI->args()->{arg1} eq "test1",  "IWPCommandline loads the args correctly");

1;