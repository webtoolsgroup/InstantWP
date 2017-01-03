=pod
 
=head1 DESCRIPTION
 
This module tests IWPEnvironment.
(c) Corvideon Limited 2016
Seamus Brady <seamus@corvideon.ie>
 
=cut

package TestIWPEnvironment;

use v5.22;
use strict;
use warnings;
use autodie;
use FindBin;
use lib "$FindBin::Bin/../lib";
use IWPEnvironment;
use IWPConfig;
use Test::More tests => 10;

my $iwp_env = IWPEnvironment->new();

# Test 1

ok( defined $iwp_env, "IWPEnvironment is defined.");

# Test 2

ok( $iwp_env->isa("IWPEnvironment"),  "IWPEnvironment is the correct class.");

# Test 3

$iwp_env->system_root_dir("/IWP");

ok( $iwp_env->system_root_dir() eq "/IWP",  "IWPEnvironment contains the correct system path.");

# Test 4

ok( $iwp_env->config_file_path() eq "/IWP/config/iwp-osx.ini",  "IWPEnvironment contains the correct config path.");


# Test 5

my $iwp_config = IWPConfig->new();
$iwp_config->config_file_path("$FindBin::Bin/test.ini");

ok( $iwp_env->get_port_number($iwp_config, "Monitor") == 10099,  "IWPEnvironment gets the correct port number settings.");

# Test 6

ok( !$iwp_env->is_windows(),  "IWPEnvironment knows this is not Windows.");

# Test 7

ok( $iwp_env->is_osx(),  "IWPEnvironment knows this is OSX.");

# Test 8

ok( $iwp_env->platform_dir() eq "/IWP/platform/",  "IWPEnvironment knows where the plaform directory is.");

# Test 9

ok( $iwp_env->vm_dir() eq "/IWP/vm/",  "IWPEnvironment knows where the vm directory is.");

# Test 10

ok( $iwp_env->docs_dir() eq "/IWP/docs/",  "IWPEnvironment knows where the docs directory is.");
