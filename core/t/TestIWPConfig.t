=pod
 
=head1 DESCRIPTION
 
This module tests IWPConfig.
(c) Corvideon Limited 2016
Seamus Brady <seamus@corvideon.ie>
 
=cut

package TestIWPConfig;

use v5.22;
use strict;
use warnings;
use autodie;
use FindBin;
use lib "$FindBin::Bin/../lib";
use IWPConfig;
use Test::More tests => 6;
use Data::Dumper;

my $iwp_config = IWPConfig->new();

# Test 1

ok( defined $iwp_config, "IWPConfig is defined.");

# Test 2

ok( $iwp_config->isa("IWPConfig"),  "IWPConfig is the correct class.");

# Test 3

$iwp_config->config_file_path("$FindBin::Bin/test.ini");

ok( $iwp_config->config_file_path eq "$FindBin::Bin/test.ini",  "IWPConfig loads the file path.");

# Test 4

# say Dumper($iwp_config->get_config_hash());

ok( defined $iwp_config->get_config_hash(),  "IWPConfig loads the config data.");

# Test 5

# say $iwp_config->get_config_setting("General", "AppName");

ok( $iwp_config->get_config_setting("General", "AppName") eq 'InstantWP',  "IWPConfig loads the config setting.");

# Test 6

ok( $iwp_config->get_config_setting("vmports", "PortOffset") == 10000,  "IWPConfig loads the port offset correctly.");
1;