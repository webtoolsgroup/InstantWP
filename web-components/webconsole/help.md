![](images/logo-top.png)

# InstantWP Web Console Documentation

Web-based command line access to your WordPress installation!
    
## What is the InstantWP Web Console?

The InstantWP Web Console is not a real shell, but it's close enough for some useful purposes. 

## What can it do then?

The InstantWP Web Console is useful in three main ways:

 * As it is running as the web server user, you have command line access to the web server. You can create, delete and move files and folders the same way as the web server can.
 
 * You can run many useful Linux commands such as *ps* or *df*. Many other commands will work (subject to the caveats below). This means you get to see what Apache Web Server is seeing, which is most useful for testing and developing. 

 *  The WP-CLI tool is also installed and provides excellent command line access to the WordPress installation. Most WP-CLI commands will work. See below.

## What is WP-CLI?

WP-CLI is the command-line interface for WordPress. You can update plugins, export the database and much more. For more information on WP-CLI see [http://wp-cli.org/ ] ()

For instance, to see what plugins are installed try:
 
    wp plugin list
      
The commands below *should* work. Try typing in the command to see the help text.



**Download, install, update and manage a WordPress installation:**

    wp core

**Manage WP-Cron events and schedules:**

    wp cron	


**Perform basic database operations using credentials stored in wp-config.ph:**

    wp db

**Execute arbitrary PHP code:**

    wp eval	

**Load and execute a PHP file:**

    wp eval-file	

**Export WordPress content to a WXR file:**

    wp export	

**Import content from a WXR file:**

    wp import	

**Manage WP-CLI packages:**

    wp package

**Manage plugins:**

    wp plugin	

**Manage posts:**
    
    wp post	

**Manage rewrite rules:**

    wp rewrite	
    
**Search/replace strings in the database:**

    wp search-replace	

**Perform site-wide operations:**

    wp site	

**Manage themes:**

    wp theme	

**Manage users:**

    wp user	

## How does it work then?

The Instant Web Console is running under the web server user account 'apache'. 

All commands will be executed using that user account. 

Once you login, your location is at the root of the InstantWP WordPress installation:

    /var/www/localhost/htdocs/wordpress

When you type in a command, the command gets sent to the server along with authentication data. The server will then change to that directory and execute the command and return the output, which is what you see show up in the InstantWP Web Console.

## What does not work?

 * The InstantWP Web Console can't use sudo or root access as it is running as the web server user.

 * The InstantWP Web Console uses a single request so anything requiring an ongoing open connection will fail. This includes text editors like Vi, Nano and Emacs.

 * The InstantWP Web Console is an ordinary webscript so it will timeout whenever the PHP *max execution time* is up and then it will be killed. So very long processes will fail. 

 * The InstantWP Web Console cannot handle some commands that return huge amounts of text.


## Is this SSH? Is it secure?

No, the InstantWP Web Console is not SSH. 

It is not recommended for a production website as there is no proper authentication mechanism enabled. 

But it is a fantastic tool for a local development server like InstantWP.



## This is great! Where can I get the code?

InstantWP Web Console is a modified form of the excellent Web-Console script as provided by Nickolay Kovalev at [http://web-console.org ]()