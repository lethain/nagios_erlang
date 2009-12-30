#!/bin/sh
#
# ## Overview
#
#   This script is used for checking that a specified process group
#   is populated with a given number of living processes.
#   (Living is defined as erlang:is_process_alive returns true.)
#
#   The script returns OK if more than or equal to --warning processes exist in the process group.
#   The script returns WARN if less than --warning processes exist.
#   The script returns CRITICAL if less than or equal to --critical processes exist, or if process group doesn't exist.
#
# ## Licence
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
# ## Acknowledgements
#
#   This script owes heavily to the [check_nginx.sh][cnsh] script, upon which the Bash components are modeled.
#   [cnsh]: http://exchange.nagios.org/directory/Plugins/Uncategorized/Software/check_nginx-2Esh/details "check_nginx.sh"
#

PROGNAME=`basename $0`
VERSION="Version 0.1,"
AUTHOR="2009, Will Larson [http://lethain.com]"

ST_OK=0
ST_WR=1
ST_CR=2
ST_UK=3
COOKIE="cookie"                          # cookie used by the local node
NODE="node@localhost"                    # name of node to inspect process group from
TMP_NODE="nagios_check_pg@`hostname`"    # name of temporary node to run code from
ERL="/usr/bin/erl"                       # full path to erlang executable
BEAM="`pwd`/ebin/"                       # full path to directory where nagios_erlang.beam exists
VERBOSITY=0                              # amount of detail to be returned, 0-3
PROCESS_GROUP="unknown"                  # name of process group to investigate
WARNING=0                                # minimum number of procs in PG before warning state
CRITICAL=0                               # minimum number of procs in PG before critical state

print_version() {
    echo "$VERSION $AUTHOR"
}

print_help() {
    print_version $PROGNAME $VERSION
    echo ""
    echo "$PROGNAME is a Nagios plugin to check the number of processes in a process group."
    echo ""
    echo "$PROGNAME -e /usr/bin/erl -b /home/wl/nagios_erlang/ebin/ -n my_server -c my_cookie"
    echo "  -w 5 -C 1 -p my_server_group"
    echo ""
    echo "Options:"
    echo "  -e/--erl       : the absolute path to erl binary (/usr/bin/erl)"
    echo "  -n/--node      : the node whose process groups will be inspected"
    echo "  -b/--beam      : the absolute path to directory with nagios_erlang.beam"
    echo "  -w/--warning   : minimum number of nodes before warning issued"
    echo "  -C/--critical  : minimum number of nodes before critical issued"
    echo "  -p/--pgroup    : name of process group to inspect"
    echo "  -c/--cookie    : the cookie used by node (cookie)"
    echo "  -v/--verbosity : level of detail, 0-3 (0)"
    echo "  -V/--version   : version of package"
    echo "  -h/--help      : show this screen"
}

while test -n "$1"; do
    case "$1" in
	--help|-h)
	    print_help
	    exit $ST_UK
	    ;;
	--verbosity|-v)	    
	    VERBOSITY=$2
	    shift
	    ;;
	--pgroup|-p)
	    PROCESS_GROUP=$2
	    shift
	    ;;
	--warning|-w)
	    WARNING=$2
	    shift
	    ;;
	--critical|-C)
	    CRITICAL=$2
	    shift
	    ;;
	--version|-V)
	    print_version $PROGNAME $VERSION
	    exit $ST_UK
	    ;;
	--erl|-e)
	    ERL=$2
	    shift
	    ;;
	--cookie|-c)
	    COOKIE=$2
	    shift
	    ;;
	--node|-n)
	    NODE=$2
	    shift
	    ;;
	*)
	    echo "Uknown argument: $1"
	    print_help
	    exit $ST_UK
	    ;;
	esac
    shift
done
CMD="$ERL -pa $BEAM -run nagios_erlang check_process_group $NODE $PROCESS_GROUP $WARNING $CRITICAL -noshell -setcookie $COOKIE -name $TMP_NODE"
if [ $VERBOSITY -ge 3 ]
 then
    echo "verbosity: $VERBOSITY"
    echo "warning: $WARNING"
    echo "critical: $CRITICAL"
    echo "pgroup: $PROCESS_GROUP"
    echo "erl: $ERL"
    echo "cookie: $COOKIE"
    echo "node: $NODE"
    echo "tmp_node: $TMP_NODE"
    echo "beam: $BEAM"
    echo "full command: $CMD"
fi
$CMD
