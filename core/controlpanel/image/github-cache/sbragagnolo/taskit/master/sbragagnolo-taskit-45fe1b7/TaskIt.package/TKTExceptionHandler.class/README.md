I am an abstract exception handler. When an unhandled error happens in a runner, it is forwarded to an exception handler using the #handleException: message.

Subclasses of me are meant to define different exception handling behaviours such as debugging or logging.