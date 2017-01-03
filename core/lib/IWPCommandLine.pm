=pod
 
=head1 DESCRIPTION
 
This module manages command line arguments.
(c) Corvideon Limited 2016
Seamus Brady <seamus@corvideon.ie>
 
=cut

package IWPCommandline;

use v5.22;
use strict;
use warnings;
use autodie;
use Moo;
use Getopt::Long qw(GetOptions);
use feature qw(signatures say);
no warnings qw(experimental::signatures);

has 'args' => (
    is       => 'rw',
);
 
sub initialise_opts( $self ){
    my $iwproot = "";       # IWP root directory
    my $start = 0;          # Start up IWP and various services
    my $quit = 0;           # Quit IWP and shut down services
    my $docs = 0;           # Open the documentation
    my $about = 0;          # Open the about page
    my $plugins = 0;        # Open the plugins folder
    my $themes  = 0;        # Open the themes folder
    my $mysql = 0;          # Open PHPMyAdmin
    my $wpfrontpage = 0;    # Open the WP front page
    my $wpadmin = 0;        # Open the WP Dashboard
    my $ssh = 0;            # Open an SSH session
    my $monitor = 0;        # Open the QEMU monitor
    my $status = 0;         # Check if IWP is running
    my $restart_apache = 0; # Restart Apache and MySQL services
    my $mount_webdav = 0;   # Mount webdav server
    my $unmount_webdav = 0; # Unmount webdav server
    my $show_vm = 0;        # Show VM window
    my $hide_vm = 0;        # Hide VM Window
    my %iwp_args = (
        "iwproot" => $iwproot,
        "start" => $start,
        "quit" => $quit, 
        "docs" => $docs,
        "about" => $about,   
        "plugins" => $plugins,    
        "themes" => $themes,     
        "mysql" => $mysql,
        "wpfrontpage" => $wpfrontpage, 
        "wpadmin" => $wpadmin,     
        "ssh" => $ssh,      
        "monitor" => $monitor,
        "status" => $status, 
        "restart_apache" => $restart_apache,
        "mount_webdav" => $mount_webdav,
        "unmount_webdav" => $unmount_webdav,
        "show_vm" => $show_vm,
        "hide_vm" => $hide_vm, 
    );
    GetOptions(\%iwp_args,
                'iwproot=s',
                'start',
                'quit', 
                'docs',
                'about',
                'plugins',  
                'themes',     
                'mysql',
                'wpfrontpage',
                'wpadmin',     
                'ssh',     
                'monitor',
                'status',
                'restart_apache',
                'mount_webdav',
                'unmount_webdav',
                'show_vm',
                'hide_vm', 
               );
    $self->args(\%iwp_args);
}


1;