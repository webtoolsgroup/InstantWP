
=pod
 
=head1 DESCRIPTION
 
IWPEnvironment
This module manages the environment for IWP.
(c) Corvideon Limited 2016
Seamus Brady <seamus@corvideon.ie>
 
=cut

package IWPEnvironment;

use v5.22;
use strict;
use warnings;
use autodie;
use Moo;
use Carp qw( croak );
use File::Copy;
use if $^O eq 'darwin', "Proc::ProcessTable";
use if $^O eq 'MSWin32', "Win32::Process::List";
use if $^O eq 'MSWin32', "Win32::OLE";
use feature qw(signatures say);
no warnings qw(experimental::signatures);

use constant OSX_CONFIG_FILE => "iwp-osx.ini";
use constant WIN_CONFIG_FILE => "iwp-win.ini";

has 'system_root_dir' => (
    is => 'rw',
);


sub bin_dir( $self ){
    if($^O eq 'MSWin32') {
         return $self->system_root_dir() ."\\bin\\";
    }
    return $self->system_root_dir() ."/bin/";
}

sub platform_dir( $self ){
    if($^O eq 'MSWin32') {
         return $self->system_root_dir() ."\\platform\\";
    }
    return $self->system_root_dir() ."/platform/";
}

sub vm_dir( $self ){
    if($^O eq 'MSWin32') {
         return $self->system_root_dir() ."\\vm\\";
    }
    return $self->system_root_dir() ."/vm/";
}

sub docs_dir( $self ){
    if($^O eq 'MSWin32') {
         return $self->system_root_dir() ."\\docs\\";
    }
    return $self->system_root_dir() ."/docs/";
}


sub config_file_path( $self ){
    if($self->is_osx()){
        return $self->system_root_dir() ."/config/".OSX_CONFIG_FILE;
    } else {
        return $self->system_root_dir() ."/config/".WIN_CONFIG_FILE;
    }
}

sub get_port_number( $self, $config, $setting ){
    my $offset = $config->get_config_setting("vmports", "PortOffset");
    my $port_setting = $config->get_config_setting("vmports", "$setting");
    return $offset + $port_setting;
}


sub win_qemu_exe_path( $self, $config ){
    if($self->is_windows()){
        my $QEMUBinary = $config->get_config_setting("qemu", "QEMUBinary");
        $QEMUBinary = $self->platform_dir().$QEMUBinary;
        my $ssh_port = $self->get_port_number($config, "SSH");
        my $Renamed_QEMUBinary  = $self->platform_dir()."win/qemu/instantwp-qemu-i386w-$ssh_port.exe";
        return $Renamed_QEMUBinary;
    }
}

sub copy_qemu_exe( $self, $config ){
    # rename the qemu exe so that we can find it, use the ssh port 
    if($self->is_windows()){
        my $QEMUBinary = $config->get_config_setting("qemu", "QEMUBinary");
        $QEMUBinary = $self->platform_dir().$QEMUBinary;
        my $Renamed_QEMUBinary = $self->win_qemu_exe_path($config);
        return if -e $Renamed_QEMUBinary;
        copy $QEMUBinary, $Renamed_QEMUBinary;
    }
}


sub set_and_validate_iwproot( $self, $iwp_opts ){
    # check that we have a root folder
    if (!$iwp_opts->args->{iwproot}){
        croak "ERROR: No IWP root folder parameter supplied!";
    }
    # set the root folder
    my $localroot =  $iwp_opts->args->{iwproot};
    $self->system_root_dir( $localroot);
    # check the config file exists
    if (-f $self->config_file_path() ){
        say "Loading config file at ". $self->config_file_path();
    } else {
        croak "ERROR: Configuration file does not exist at ". $self->config_file_path();
    }
}

sub is_windows( $self ){
    my $os = $^O;
    return $os eq 'MSWin32' ? 1 : 0;
}


sub is_osx( $self ){
    my $os = $^O;
    return $os eq 'darwin' ? 1 : 0;
}


sub is_iwp_running( $self, $iwp_config ){
    my $ssh_port = $self->get_port_number($iwp_config, "SSH");
    my $pid = $self->get_pid_from_port( $ssh_port );
    sleep 1;
    if($pid != -1){
        return 1; # IWP running
    } else {
        return 0; # not running
    }
}

sub get_pid_from_port( $self, $port ){
   if($self->is_osx()){
        return $self->get_pid_from_port_osx( $port );
   }
   if($self->is_windows()){
        return $self->get_pid_from_port_win( $port );
   }
   # should not get here
   return -1; 
}

####### Start Win functions ########

sub get_pid_from_port_win( $self, $port ){
    my $processes = Win32::Process::List->new();  
    my %list = $processes->GetProcesses();
    foreach my $key ( keys %list ) {
        if($list{$key} =~ /$port/){
            return 1;
        }   
    }
    return -1;
}

####### End Win functions ########

####### Start OSX functions ########

sub get_pid_from_port_osx( $self, $port ){
    # loop through all processes and get the IWPHostServ pid
    my $t = new Proc::ProcessTable;
    foreach my $p (@{$t->table}) {
        foreach my $f ($t->fields){
          if( $f eq "cmndline"){
            if(index($p->{$f}, $port) != -1 ){
                return $p->{pid};
            }     
          }
        }
    }
    return -1;
}

####### End OSX functions ########
1;