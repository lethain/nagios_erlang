# README

This repository contains scripts to facilitate monitoring [Erlang][erlang].
systems using the [Nagios][nagios] monitoring framework.

At this time it supports monitoring nodes, applications and process groups.

[erlang]: http://ftp.sunet.se/pub/lang/erlang/ "Erlang Programming Language"
[nagios]: http://www.nagios.org/ "Nagios Monitoring Software"

## Files

* README                      - this file
* Makefile                    - build script
* check_erlang_application.sh - shell script interface to check erlang apps are running
* check_erlang_node.sh        - shell script interface to check erlang node is running
* check_erlang_pg.sh          - shell script interface to check process group is running
* nagios_erlang.erl           - Erlang implementation of checking apps/nodes/pgs
* ebin/                       - contains compiled Erlang files

## Command Line Usage Examples

Examples of running the verification scripts directly (not via Nagios).
These scripts are all being run in a Git checkout of the code, and after
running ``make`` within that directory.

To run in other directorys, the ``--beam`` parameter will need to be updated
to point to the directory where ``nagios_erlang.beam`` can be found.

### Checking Nodes

    bash-3.2$ ./check_erlang_node.sh -e `which erl` -n my_node -c `cat /path/to/.erlang.cookie`
    OK - Node my_node running.

### Checking Applications

    bash-3.2$ ./check_erlang_application.sh -e `which erl` -n my_node -c `cat /path/to/.erlang.cookie` -a application1,application2
    OK - Applications ["application1","application2"] running on Node my_node.

### Checking Process Groups

    ./check_erlang_pg.sh -e /usr/bin/erl -n my_node -p my_group -w 1
    OK - Process group my_group populated on Node my_node with 3 processes.

### Full Example

First, we check that the node doesn't exist before we start it:

    bash-3.2$ ./check_erlang_node.sh -e erl -n my_node@`hostname` -c cookie
    CRITICAL - Node 'my_node@will-larsons-macbook.local' not running.

Next, actually start the node:

    will-larsons-macbook:~ lethain$ erl -setcookie cookie -name my_node@`hostname`
    Erlang R13B03 (erts-5.7.4) [source] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]
    Eshell V5.7.4  (abort with ^G)
    (my_node@will-larsons-macbook.local)1> node().
    'my_node@will-larsons-macbook.local'

And we can verify the node is accessible:

    bash-3.2$ ./check_erlang_node.sh -e erl -n my_node@`hostname` -c cookie
    OK - Node 'my_node@will-larsons-macbook.local' running.

Next we can check for started applications (note that kernel2 is a made up
application which doesn't exist, while kernel is a real application which
is indeed running):

    bash-3.2$ ./check_erlang_application.sh -e erl -c cookie -n my_node@`hostname` -a kernel2
    CRITICAL - Applications ["kernel2"] not running on Node 'my_node@will-larsons-macbook.local'.
    bash-3.2$ ./check_erlang_application.sh -e erl -c cookie -n my_node@`hostname` -a kernel
    OK - Applications ["kernel"] running on Node 'my_node@will-larsons-macbook.local'.

Check statistics (memory, processes, ports):

    bash-3.2$ ./check_erlang_statistics.sh -e erl -n my_node@will-larsons-macbook.local -c cookie -p sys_mem:300-500
    bash-3.2$ ./check_erlang_statistics.sh -e erl -n my_node@will-larsons-macbook.local -c cookie -p processes:300-500
    bash-3.2$ ./check_erlang_statistics.sh -e erl -n my_node@will-larsons-macbook.local -c cookie -p ports:300-500

And that is all there is to it.
