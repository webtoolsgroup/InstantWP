=pod
 
=head1 DESCRIPTION
 
InstantWP Start SSH Terminal

Based on a Simple Terminal Resizing Example (C) 2006 Jeff Carr

=cut


if( ! defined $ARGV[0] ) {
	print "Usage: ssh-term <host> [ <username> [ <password> ] <port> ]\n";
	exit;
}

my ($host, $username, $password, $port) = @ARGV;
$username = $ENV{USER} if $username eq "";

use Expect;
use IO::Pty;

my $spawn = new Expect;
$spawn->raw_pty(1);

# This gets the size of your terminal window
$spawn->slave->clone_winsize_from(\*STDIN);

my $PROMPT;

# This function traps WINCH signals and passes them on
sub winch {
	my $signame = shift;
	my $pid = $spawn->pid;
	$shucks++;
	$spawn->slave->clone_winsize_from(\*STDIN);
	kill WINCH => $spawn->pid if $spawn->pid;
}

$SIG{WINCH} = \&winch;  # best strategy

$spawn=Expect->spawn("ssh -p $port $username\@$host");

$PROMPT  = '[\]\$\>\#]\s$';
my $ret = $spawn->expect(10,
	[ qr/\(yes\/no\)\?\s*$/ => sub { $spawn->send("yes\n"); exp_continue; } ],
	[ qr/assword:\s*$/ 	=> sub { $spawn->send("$password\n") if defined $password;  } ],
	[ qr/ogin:\s*$/		=> sub { $spawn->send("$username\n"); exp_continue; } ],
	[ qr/REMOTE HOST IDEN/ 	=> sub { print "FIX: .ssh/known_hosts\n"; exp_continue; } ],
	[ qr/$PROMPT/ 		=> sub { $spawn->send("echo Now try window resizing\n"); } ],
);

# Hand over control
$spawn->interact();
exit;
