=pod
 
=head1 DESCRIPTION
 
IWPTasks
This module collects the tasks IWP can run.
(c) Corvideon Limited 2016
Seamus Brady <seamus@corvideon.ie>
 
=cut

package IWPTasks;

use v5.22;
use strict;
use warnings;
use autodie;
use Moo;
use Carp qw( croak );
use if $^O eq 'darwin', "Proc::Terminator";
use LWP::Simple qw($ua head);
use Term::Spinner;
use feature qw(signatures say);
no warnings qw(experimental::signatures);

use constant LOCALHOST => "127.0.0.1";
use constant RESTART_MYSQL => "sudo service mysql-server restart";
use constant RESTART_APACHE=> "sudo service apache24 restart";
use constant RESTART_APACHE=> "sudo service apache24 restart";
use constant POWEROFF => "system_powerdown";
use constant REBOOT => "system_reset";
use constant QUIT => "quit";
use constant LOADVM => "loadvm ";
use constant SAVEVM => "savevm ";
use constant DELVM => "delvm";
use constant ANSICONEXE => "win/ansicon.exe";

has 'iwp_env' => (
    is => 'rw',
);

has 'iwp_config' => (
    is => 'rw',
);

# subs for starting IWP services

sub start( $self ){
    
    if ( $self->iwp_env->is_iwp_running( $self->iwp_config ) ){
      $self->status_message();
      return;
    }
    
    say "Starting InstantWP...";

    # start the vm using qemu
    say "Loading the IWPServer VM...";
    
    # copy the qemu exe so we can find it
    $self->iwp_env->copy_qemu_exe($self->iwp_config());
    
    # start it up!
    if($self->iwp_env()->is_windows()){
        system( "start /b ".$self->vm_command_string() ); 
    } else {
        system( $self->vm_command_string() );
    }
    
    # sleep
    my $sleep = $self->iwp_config->get_config_setting("qemu", "SleepSeconds");
    sleep $sleep;
    
    # load the default vm snapshot if UseSnapshots enabled
    my $use_snapshots = $self->iwp_config->get_config_setting("qemu", "UseSnapshots");
    if($use_snapshots eq "yes"){
        $self->load_vm_snaphot_command_string();
    }
    
    # wait for the webserver to load and then hide VM window
    $self->wait_on_vm();
    
    # start the file sync
    $self->start_file_sync();
    
    # should we mount the WebDav folder?
    my $mount_webdav = $self->iwp_config->get_config_setting("startup", "MountWebDAV");
    if($mount_webdav eq "yes"){
        $self->mount_webdav();
    }
  
    # should we restart apache?
    my $restart_apache = $self->iwp_config->get_config_setting("startup", "RestartApacheOnStartup");
    if($restart_apache eq "yes"){
        $self->restart_apache();
    }

    # should we restart mysql?
    my $restart_mysql = $self->iwp_config->get_config_setting("startup", "RestartMySQLOnStartup");
    if($restart_mysql eq "yes"){
        $self->restart_mysql();
    }

    # status message
    $self->status_message();
    
    exit 0;
}

sub start_file_sync( $self ){
    if($self->iwp_env()->is_windows()){
        # sync params
        say "Starting file syncing...";
        my $SyncBinDir = $self->iwp_config->get_config_setting("FileSync", "SyncBinDir");
        my $SyncCommand = $self->iwp_config->get_config_setting("FileSync", "SyncCommand");
        my $SyncUser = $self->iwp_config->get_config_setting("FileSync", "SyncUser");
        my $SyncPassword = $self->iwp_config->get_config_setting("FileSync", "SyncPassword");
        my $SyncLocalRoot = $self->iwp_env()->system_root_dir().$self->iwp_config->get_config_setting("FileSync", "SyncLocalRoot");
        my $SyncVMRoot = $self->iwp_config->get_config_setting("FileSync", "SyncVMRoot");
        my $SyncTimerSeconds = $self->iwp_config->get_config_setting("FileSync", "SyncTimerSeconds");
        my $ssh_port = $self->iwp_env->get_port_number($self->iwp_config(), "SSH");
        my $sync_params = '"'.$SyncUser.'"'.' "'.$SyncPassword.'" 127.0.0.1 '.$ssh_port.' "'.$SyncLocalRoot.'" "'.$SyncVMRoot.'" "'.$SyncTimerSeconds.'"';
        my $batch_file_runner = $self->iwp_env->platform_dir().$SyncBinDir."chp.exe ";
        $SyncCommand = "start /b ".$batch_file_runner.$self->iwp_env->platform_dir().$SyncBinDir.$SyncCommand." ".$sync_params;
        # root root 127.0.0.1 10022 C:\Users\paperspace\Downloads\WinSCP-5.9.2-Portable\wordpress /usr/local/www/apache24/data/wordpress/ 15
        system($SyncCommand);
    }
}

sub quit_file_sync( $self ){
    if($self->iwp_env()->is_windows()){
        say "Shutting down file syncing...";
        # sync params
        my $SyncBinDir = $self->iwp_config->get_config_setting("FileSync", "SyncBinDir");
        $SyncBinDir= $self->iwp_env->platform_dir().$SyncBinDir;
        my $SyncPIDFile = $self->iwp_config->get_config_setting("FileSync", "SyncPIDFile");
        $SyncPIDFile = $SyncBinDir.$SyncPIDFile;
        open my $file, '<', $SyncPIDFile; 
        my $pid = <$file>; 
        close $file;
        kill $pid;
    }
}

sub wait_on_vm( $self ){
    my $wait_on_vm = $self->iwp_config->get_config_setting("startup", "WaitOnVMStart");
    if($wait_on_vm eq "yes"){
        say "Waiting for IWPServer to start...";
        my $wait_seconds = $self->iwp_config->get_config_setting("startup", "WaitOnVMSeconds");
        my $http_port = $self->iwp_env->get_port_number($self->iwp_config(), "HTTP");
        my $wp_url = $self->iwp_config->get_config_setting("shortcuts", "WPFrontPage");
        my $url = "http://".LOCALHOST.":".$http_port."/".$wp_url;
        my $web_server_up = 0;
        my $web_timeout = $self->iwp_config->get_config_setting("startup", "WebCheckTimeoutSeconds");
        my $spinner = Term::Spinner->new();
        $ua->timeout($web_timeout);
        for my $i (1 .. $wait_seconds){
            $spinner->advance();
            if (head($url)) { # is the web server up yet?
                $web_server_up = 1;
            }
            if($web_server_up){
                last;
            } else {
                sleep 1;
            } 
        }
        undef $spinner;
        if($web_server_up){
            say "IWPServer has loaded...";   
        } else {
            say "Oh dear, IWPServer failed to load...";
            exit 0;
        } 
    }
}

sub wait_on_vm_quit( $self ){
    # find the QEMU proc and wait on it
    if($self->iwp_env->is_osx()){
        my $wait_on_vm = $self->iwp_config->get_config_setting("shutdown", "WaitOnVMQuit");
        if($wait_on_vm eq "yes"){
            say "Waiting for IWPServer to close...";
            my $wait_seconds = $self->iwp_config->get_config_setting("shutdown", "WaitOnVMQuitSeconds");
            my $vm_running = 0;
            my $spinner = Term::Spinner->new();
            for my $i (1 .. $wait_seconds){
                $spinner->advance();
                $vm_running = $self->iwp_env->is_iwp_running($self->iwp_config());
                if(!$vm_running){
                    last;
                } else {
                    sleep 1;
                } 
            }
            undef $spinner;
        }
    } else {
        # windows code todo
    }
}

sub quit( $self ){
    say "Quitting InstantWP...";
    
    # save the default vm to a snapshot if UseSnapshots enabled
    my $use_snapshots = $self->iwp_config->get_config_setting("qemu", "UseSnapshots");
    if($use_snapshots eq "yes"){
        $self->del_vm_snaphot(); # delete snapshot if configured
        $self->save_vm_snaphot();
    }
    
    # sleep
    sleep 1;
    
    my $mount_webdav = $self->iwp_config->get_config_setting("startup", "MountWebDAV");
    if($mount_webdav eq "yes"){
         $self->unmount_webdav();
    }
    
    # shutdown file sync
    $self->quit_file_sync();
    
    # quit the vm
    say "Qutting the InstantWP Server VM...";
    if($use_snapshots eq "yes"){ 
        system( $self->quit_vm_via_qemu_command_string() );
    } else {
        $self->poweroff_vm();
        $self->wait_on_vm_quit();
    }
    
    
    say "Done";
    
    exit 0;
}


sub docs( $self ){
    say "Opening InstantWP documentation...";
    my $docs_dir = $self->iwp_env->docs_dir();
    my $docs_index = $self->iwp_config->get_config_setting("shortcuts", "DocIndexFile");
    $self->open_me( $docs_dir.$docs_index );
    exit 0;
}

sub about( $self ){
    say "Opening InstantWP about page...";
    my $about_dir = $self->iwp_env->docs_dir();
    my $about_index = $self->iwp_config->get_config_setting("shortcuts", "AboutIndexFile");
    $self->open_me( $about_dir.$about_index );
    exit 0;
}

sub plugins( $self ){
    say "Opening InstantWP plugins folder...";
    # windows
    if($self->iwp_env()->is_windows()){
        my $htdocs_dir = $self->iwp_env->htdocs_dir($self->iwp_config);
        my $plugins_dir = $self->iwp_config->get_config_setting("shortcuts", "PluginsDir");
        say $htdocs_dir.$plugins_dir;
        $self->open_me($htdocs_dir.$plugins_dir );
        exit 0;
    } else {
        # osx
        my $htdocs_dir = $self->iwp_env->webdav_root_dir($self->iwp_config);
        my $plugins_dir = $self->iwp_config->get_config_setting("shortcuts", "PluginsDir");
        say $htdocs_dir.$plugins_dir;
        $self->open_me($htdocs_dir.$plugins_dir );
        exit 0;
    }
}

sub themes( $self ){
    say "Opening InstantWP theme folder...";
    # windows
    if($self->iwp_env()->is_windows()){
        my $htdocs_dir = $self->iwp_env->htdocs_dir($self->iwp_config);
        my $plugins_dir = $self->iwp_config->get_config_setting("shortcuts", "ThemesDir");
        say $htdocs_dir.$plugins_dir;
        $self->open_me($htdocs_dir.$plugins_dir );
        exit 0;
    } else {
        # osx
        my $htdocs_dir = $self->iwp_env->webdav_root_dir($self->iwp_config);
        my $themes_dir = $self->iwp_config->get_config_setting("shortcuts", "ThemesDir");
        $self->open_me( $htdocs_dir.$themes_dir );
        exit 0;
    }
}

sub mysql( $self ){
    say "Opening InstantWP PHPMyAdmin...";
    my $http_port = $self->iwp_env->get_port_number($self->iwp_config(), "HTTP");
    my $PHPMyAdminUrl = $self->iwp_config->get_config_setting("shortcuts", "PHPMyAdminUrl");
    $self->open_me("http://".LOCALHOST.":".$http_port."/".$PHPMyAdminUrl);
    exit 0;
}


sub wpfrontpage( $self ){
    say "Opening InstantWP WP Front Page...";
    my $http_port = $self->iwp_env->get_port_number($self->iwp_config(), "HTTP");
    my $WPDashboard = $self->iwp_config->get_config_setting("shortcuts", "WPFrontPage");
    say "http://".LOCALHOST.":".$http_port."/".$WPDashboard;
    $self->open_me("http://".LOCALHOST.":".$http_port."/".$WPDashboard);
    exit 0;
}

sub wpadmin( $self ){
    say "Opening InstantWP WP Dashboard...";
    my $http_port = $self->iwp_env->get_port_number($self->iwp_config(), "HTTP");
    my $WPDashboard = $self->iwp_config->get_config_setting("shortcuts", "WPDashboard");
    $self->open_me("http://".LOCALHOST.":".$http_port."/".$WPDashboard);
    exit 0;
}

sub ssh( $self ){
    say "Opening InstantWP SSH...";
    say "Logging into Instant WordPress Server VM...";
    my $ssh_port = $self->iwp_env->get_port_number($self->iwp_config(), "SSH");
    my $ssh_user = $self->iwp_config->get_config_setting("IWPHostServ", "IWPHostServUser");
    my $ssh_pass = $self->iwp_config->get_config_setting("IWPHostServ", "IWPHostServPassword");
    my $ssh_term = $self->iwp_env->bin_dir()."ssh-term";

    if($self->iwp_env->is_osx()){
        exec($ssh_term." ".LOCALHOST." $ssh_user $ssh_pass $ssh_port");
    } else {
        my $PLinkExe = $self->iwp_config->get_config_setting("components", "PLinkExe");
        $ssh_term = $self->iwp_env->platform_dir().$PLinkExe;
        my $ansicon = $self->iwp_env->platform_dir().ANSICONEXE;
        system( "start $ansicon $ssh_term -ssh ".LOCALHOST." -P $ssh_port -l $ssh_user -pw $ssh_pass");
    }
    exit 0;
}

sub monitor( $self ){
    say "Opening InstantWP QEMU Monitor...";
    my $monitor_port = $self->iwp_env->get_port_number($self->iwp_config(), "Monitor");
    if($self->iwp_env->is_osx()){
        # osx telnet session
        exec("telnet ".LOCALHOST." $monitor_port");
    } else{
        my $TelnetBinary = $self->iwp_config->get_config_setting("components", "PLinkExe");
        $TelnetBinary = $self->iwp_env->platform_dir().$TelnetBinary;
        my $ansicon = $self->iwp_env->platform_dir().ANSICONEXE;
        system("start $ansicon $TelnetBinary -telnet ".LOCALHOST." -P $monitor_port");
    }
    exit 0;
}

sub status( $self ){
    say "Checking if InstantWP is running...";
    # find the QEMU process
    if($self->iwp_env->is_osx()){
        if( $self->iwp_env->is_iwp_running( $self->iwp_config ) ){
            $self->status_message();
        }  else {
            say "InstantWP does not seem to be running...";
        }
    } else {
        # windows code todo
    }
    exit 0;
}

sub status_message( $self ){
    my $ssh_port = $self->iwp_env->get_port_number($self->iwp_config(), "SSH");
    my $http_port = $self->iwp_env->get_port_number($self->iwp_config(), "HTTP");
    my $monitor_port = $self->iwp_env->get_port_number($self->iwp_config(), "Monitor");
    say "InstantWP is running!";
    say "HTTP port: $http_port";
    say "SSH port: $ssh_port";
    say "QEMU monitor (telnet) port: $monitor_port";
}

sub run_qemu_command( $self, $TelnetBinary, $monitor_port, $monitor_command){
    if($self->iwp_env()->is_windows()){
        system("$TelnetBinary $monitor_port $monitor_command"); 
    } else {
        system("$TelnetBinary $monitor_port '$monitor_command'"); 
    }
}


sub reboot_vm( $self ){
    say "Rebooting IWPServer...";
    my $TelnetBinary = $self->iwp_config->get_config_setting("components", "IWPQEMUTelnetPath");
    $TelnetBinary = $self->iwp_env->platform_dir().$TelnetBinary;
    my $monitor_port = $self->iwp_env->get_port_number($self->iwp_config(), "Monitor");
    my $monitor_command = REBOOT;
    $self->run_qemu_command( $TelnetBinary, $monitor_port, $monitor_command );
}

sub poweroff_vm( $self ){
    # this powers down the VM rather than just quitting QEMU
    say "Powering off IWPServer...";
    my $TelnetBinary = $self->iwp_config->get_config_setting("components", "IWPQEMUTelnetPath");
    $TelnetBinary = $self->iwp_env->platform_dir().$TelnetBinary;
    my $monitor_port = $self->iwp_env->get_port_number($self->iwp_config(), "Monitor");
    my $monitor_command = POWEROFF;
    $self->run_qemu_command( $TelnetBinary, $monitor_port, $monitor_command );
}

sub restart_apache_mysql( $self ){
    $self->restart_apache();
    $self->restart_mysql();
}

sub restart_apache( $self ){
    say "Restarting InstantWP Apache...";
    my $IWPSSHCommand = $self->iwp_config->get_config_setting("components", "IWPSSHCommandPath");
    $IWPSSHCommand= $self->iwp_env->platform_dir().$IWPSSHCommand;
    my $IWPHostServPort = $self->iwp_env->get_port_number($self->iwp_config(), "IWPHostServPort");
    my $ssh_port = $self->iwp_env->get_port_number($self->iwp_config(), "SSH");
    my $ssh_user = $self->iwp_config->get_config_setting("IWPHostServ", "IWPHostServUser");
    my $ssh_pass = $self->iwp_config->get_config_setting("IWPHostServ", "IWPHostServPassword");
    my $restart_apache_command = "sudo rc-service apache2 restart";
    if($self->iwp_env->is_osx()){
        # osx
        system("$IWPSSHCommand $ssh_port $ssh_user $ssh_pass '$restart_apache_command'");
    } else {
        # to be done, start on windows
        
    }
    say("Apache web server restarted.");
}

sub restart_mysql( $self ){
    say "Restarting InstantWP MySQL...";
    my $IWPSSHCommand = $self->iwp_config->get_config_setting("components", "IWPSSHCommandPath");
    $IWPSSHCommand= $self->iwp_env->platform_dir().$IWPSSHCommand;
    my $IWPHostServPort = $self->iwp_env->get_port_number($self->iwp_config(), "IWPHostServPort");
    my $ssh_port = $self->iwp_env->get_port_number($self->iwp_config(), "SSH");
    my $ssh_user = $self->iwp_config->get_config_setting("IWPHostServ", "IWPHostServUser");
    my $ssh_pass = $self->iwp_config->get_config_setting("IWPHostServ", "IWPHostServPassword");
    my $restart_mysql_command = "sudo rc-service mariadb restart";
    if($self->iwp_env->is_osx()){
        # osx
        system("$IWPSSHCommand $ssh_port $ssh_user $ssh_pass '$restart_mysql_command'");
    } else {
        # to be done, start on windows
        
    }
    say("MySQL server restarted.");
}



# show hide window, $class not used for now
sub toggle_vm_window( $self, $window_title, $class){
    if($self->iwp_env->is_osx()){
        # to do
    } else {
        my $toggle_cmd = $self->iwp_env->platform_dir()."win\\ShowHideWin.bat $window_title";
        say "Toggling visibility of QEMU window...";
        system("start ".$toggle_cmd);
    }
}

sub show_vm_window( $self ){
    # ssh port used in qemu window title
    my $ssh_port = $self->iwp_env->get_port_number($self->iwp_config(), "SSH");
    toggle_vm_window( $self, $ssh_port, "" );
}

sub hide_vm_window( $self ){
    # ssh port used in qemu window title
    my $ssh_port = $self->iwp_env->get_port_number($self->iwp_config(), "SSH");
    toggle_vm_window( $self, $ssh_port, "" );
}

# utility function to open a folder/file/url
sub open_me( $self, $item_to_open){
    if($self->iwp_env->is_osx()){
        system("open ".$item_to_open);
    } else {
        system("start ".$item_to_open);
    }
}

sub del_vm_snaphot( $self ){
    # The command will save the default snapshot before saving
    my $TelnetBinary = $self->iwp_config->get_config_setting("components", "IWPQEMUTelnetPath");
    $TelnetBinary = $self->iwp_env->platform_dir().$TelnetBinary;
    my $monitor_port = $self->iwp_env->get_port_number($self->iwp_config(), "Monitor");
    my $snapshot_name = $self->iwp_config->get_config_setting("qemu", "DefaultSnapshot");
    my $delete_snapshot = $self->iwp_config->get_config_setting("qemu", "DeleteSnapShotBeforeSave");
    if($delete_snapshot eq "yes"){
        my $monitor_command =  DELVM." $snapshot_name";
        $self->run_qemu_command( $TelnetBinary, $monitor_port, $monitor_command );
    }
}
    
sub save_vm_snaphot( $self ){
    # The command to save a particular VM snapshot via telnet into the QEMU monitor
    my $TelnetBinary = $self->iwp_config->get_config_setting("components", "IWPQEMUTelnetPath");
    $TelnetBinary = $self->iwp_env->platform_dir().$TelnetBinary;
    my $monitor_port = $self->iwp_env->get_port_number($self->iwp_config(), "Monitor");
    my $snapshot_name = $self->iwp_config->get_config_setting("qemu", "DefaultSnapshot");
    my $monitor_command = SAVEVM." $snapshot_name";
    $self->run_qemu_command( $TelnetBinary, $monitor_port, $monitor_command );
}


# command strings

sub vm_command_string( $self ){
    # The command to start QEMU and the VM
    
    my $QEMUBinary = $self->iwp_config->get_config_setting("qemu", "QEMUBinary");
    
    if($self->iwp_env->is_windows()){
        $QEMUBinary = $self->iwp_env->win_qemu_exe_path($self->iwp_config);
    } else {
        $QEMUBinary = $self->iwp_env->platform_dir().$QEMUBinary;
    }
   
    my $RAM = $self->iwp_config->get_config_setting("qemu", "RAM");
    
    my $VMFile = $self->iwp_config->get_config_setting("qemu", "VMFile");
    $VMFile = $self->iwp_env->vm_dir().$VMFile;
    
    my $ssh_port = $self->iwp_env->get_port_number($self->iwp_config(), "SSH");
    my $http_port = $self->iwp_env->get_port_number($self->iwp_config(), "HTTP");
    my $monitor_port = $self->iwp_env->get_port_number($self->iwp_config(), "Monitor");
    
    
    # should we show any QEMU gui?
    my $ShowQEMUWindow = $self->iwp_config->get_config_setting("qemu", "ShowQEMUWindow");
    if($ShowQEMUWindow eq "no"){
        $ShowQEMUWindow = " -nographic ";
    } else {
       undef $ShowQEMUWindow;
    }
    
    # should the system wait for the vm to load?
    my $nowait = $self->iwp_config->get_config_setting("qemu", "NoWait");
    my $amp = " ";
    if($nowait eq "yes"){
        $amp = " &"; # append an ampersand
    }
    
    my $returnString =   "$QEMUBinary ".
                        " -m $RAM ".  
                        " -hda $VMFile -boot c ".
                        " -net nic ".
                        " -net user,hostfwd=tcp::$ssh_port-:22 ".
                        " -redir tcp:".$http_port."::80 ". 
                        " -redir tcp:10139::139 ". 
                        " -monitor telnet:127.0.0.1:$monitor_port,server,nowait ".
                        " -name IWPServer-$ssh_port";
            
    if(defined $ShowQEMUWindow){
        $returnString =  $returnString." $ShowQEMUWindow".$amp;
    } else {
        $returnString =  $returnString.$amp;
    }
    return $returnString;
}

sub mount_webdav( $self ){    
    my $webdav_script = $self->iwp_config->get_config_setting("webdav", "MountWebDevScript");
    $webdav_script = $self->iwp_env->platform_dir().$webdav_script;
    system( $webdav_script );
}

sub unmount_webdav( $self ) {
    say "Shutting down the IWPServer WebDav Server..";
    my $webdav_script = $self->iwp_config->get_config_setting("webdav", "UnmountWebDevScript");
    $webdav_script = $self->iwp_env->platform_dir().$webdav_script;
    system( $webdav_script );
}

sub load_vm_snaphot_command_string( $self ){
    # The command to load a particular VM snapshot via telnet into the QEMU monitor
    
    my $TelnetBinary = $self->iwp_config->get_config_setting("components", "IWPQEMUTelnetPath");
    $TelnetBinary = $self->iwp_env->platform_dir().$TelnetBinary;
    my $monitor_port = $self->iwp_env->get_port_number($self->iwp_config(), "Monitor");
    my $snapshot_name = $self->iwp_config->get_config_setting("qemu", "DefaultSnapshot");
    my $monitor_command = LOADVM." $snapshot_name";
    $self->run_qemu_command( $TelnetBinary, $monitor_port, $monitor_command );
}


sub quit_vm_via_qemu_command_string( $self ){
    # The command to quit VM via telnet into the QEMU monitor
    
    my $TelnetBinary = $self->iwp_config->get_config_setting("components", "IWPQEMUTelnetPath");
    $TelnetBinary = $self->iwp_env->platform_dir().$TelnetBinary;
    my $monitor_port = $self->iwp_env->get_port_number($self->iwp_config(), "Monitor");
    my $monitor_command = QUIT;
    return "$TelnetBinary $monitor_port '$monitor_command'";
}



1;
