# finger
Simple implementation of the finger protocol for the last exercise found in [Haskell Programming from first principles](http://haskellbook.com/).
In order to query the database an external finger client is required. It can be found in most linux distributions.
For modifications to the database two methods are supported:
* Command line (fingerctl)
* Control socket

# Command line (fingerctl)
Please consult fingerctl --help

# Control Socket
The socket listens on localhost:8080 and expects messages according to a simple protocol. 
It is a bit similar to HTTP in that each message consists of a verb and headers separated by newline.
Supported verbs (commands) are as follows: USERADD, USERMOD and USERDEL.
Headers are one of: Name, RealName, Shell, Home and Phone. Each command requires some headers while others may be optional as in fingerctl.
Examples:
```
USERADD
Name: test
RealName: Testing Test
Shell: /bin/bash
Home: /home/test
Phone: 123 456 789
```

```
USERMOD
Name: test
Shell: /bin/zsh
```

```
USERDEL
Name: Test
```

You can use the telnet command to send these messages.