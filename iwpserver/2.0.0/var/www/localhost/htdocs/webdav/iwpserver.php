<?php

use Sabre\DAV;
use Sabre\DAV\Auth;


// The autoloader
require 'vendor/autoload.php';

date_default_timezone_set('UTC');

// Now we're creating a whole bunch of objects
$rootDirectory = new DAV\FS\Directory('/var/www/localhost/htdocs/');

// The server object is responsible for making sense out of the WebDAV protocol
$server = new DAV\Server($rootDirectory);

// If your server is not on your webroot, make sure the following line has the
// correct information
$server->setBaseUri('/webdav/iwpserver.php');

// The lock manager is reponsible for making sure users don't overwrite
// each others changes.
$lockBackend = new DAV\Locks\Backend\File('data/locks');
$lockPlugin = new DAV\Locks\Plugin($lockBackend);
$server->addPlugin($lockPlugin);

// This ensures that we get a pretty index in the browser, but it is
// optional.
$server->addPlugin(new DAV\Browser\Plugin());

// Connnect to the database
$pdo = new PDO('mysql:host=localhost;dbname=webdav', 'root','');

// Throwing exceptions when PDO comes across an error:
$pdo->setAttribute(PDO::ATTR_ERRMODE,PDO::ERRMODE_EXCEPTION);


// Create the backend
$locksBackend = new Sabre\DAV\Locks\Backend\PDO($pdo);

// Add the plugin to the server.
$locksPlugin = new Sabre\DAV\Locks\Plugin(
    $locksBackend
);
$server->addPlugin($locksPlugin);

// Creating the backend.
$authBackend = new Auth\Backend\PDO($pdo);

// We're assuming that the realm name is called 'SabreDAV'.
$authBackend->setRealm('SabreDAV');

// Creating the plugin.
$authPlugin = new Auth\Plugin($authBackend);

// Adding the plugin to the server.
$server->addPlugin($authPlugin);

// All we need to do now, is to fire up the server
$server->exec();

?>
