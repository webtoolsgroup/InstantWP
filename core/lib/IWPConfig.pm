=pod
 
=head1 DESCRIPTION
 
This module manages the configuration settings for IWP.
(c) Corvideon Limited 2016
Seamus Brady <seamus@corvideon.ie>
 
=cut

package IWPConfig;

use v5.22;
use strict;
use warnings;
use autodie;
use Config::INI::Reader;
use Moo;
use feature qw(signatures);
no warnings qw(experimental::signatures);


has 'config_file_path' => (
    is       => 'rw',
);

sub get_config_hash( $self ){
    my $config_hash = Config::INI::Reader->read_file($self->config_file_path);
    return $config_hash;
}

sub get_config_setting ( $self, $section, $setting ) {
    my $config_hash = $self->get_config_hash();
    my $return_value = $config_hash->{$section}->{$setting};
    return $return_value;
}

1;
