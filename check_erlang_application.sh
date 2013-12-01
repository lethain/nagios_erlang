#!/bin/sh
#
# ## Overview
#
#   This script is used for checking that a specified node is running a specific
#   application.
#
#   This script only uses two status codes: OK and CRITICAL. This is due
#   to the fact that it only checks existence, and does so in a purely
#   binary fashion.
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
#   This script owes heavily to the [check_nginx.sh][cnsh] script, upon which the Bash components
#   are modeled.
#
#   [cnsh]: http://exchange.nagios.org/directory/Plugins/Uncategorized/Software/check_nginx-2Esh/details "check_nginx.sh"
#

PROGNAME=`basename $0`
VERSION="Version 0.1,"
AUTHOR="2009, Will Larson [http://lethain.com]"

RANDOM=`echo | awk '{srand(); print rand()}' | awk '{sub(/\./, ""); print}'` # $RANDOM for sh

ST_OK=0
ST_WR=1
ST_CR=2
ST_UK=3
COOKIE="cookie"                                  # cookie used by the local node
NODE="node@localhost"                            # name of node to check
TMP_NODE="nagios_check_app_$RANDOM"              # name of temporary node to ping $NODE
TMP_HOST="`hostname`"
ERL="/usr/bin/erl"                               # full path to erlang executable
BEAM="`pwd`/ebin/"                               # full path to directory where nagios_erlang.beam exists
VERBOSITY=0                                      # amount of detail to be returned, 0-3
APPLICAITON="unknown"                            # name of application to check

print_version() {
    echo "$VERSION $AUTHOR"
}

print_help() {
    print_version $PROGNAME $VERSION
    echo ""
    echo "$PROGNAME is a Nagios plugin to check if an Erlang node is pingable from the local host."
    echo ""
    echo "$PROGNAME -e /usr/bin/erl -b /home/wl/nagios_erlang/ebin/ -n my_server -c my_cookie -a my_application"
    echo ""
    echo "Options:"
    echo "  -a/--application : name of application to check on" 
    echo "  -e/--erl         : the absolute path to erl binary (/usr/bin/erl)"
    echo "  -n/--node        : the node to ping against"
    echo "  -b/--beam        : the absolute path to directory with nagios_erlang.beam"
    echo "  -c/--cookie      : the cookie used by node (cookie)"
    echo "  -h/--help        : show this screen"
    echo "  -H/--host        : host of erlang node"
    echo "  -v/--verbosity   : level of detail, 0-3 (0)"
    echo "  -V/--version     : version of package"
    echo "  -h/--help        : show this screen"
}

while test -n "$1"; do
    case "$1" in
    --application|-a)
        APPLICATION=$2
        shift
        ;;
    --help|-h)
        print_help
        exit $ST_UK
        ;;
    --verbosity|-v)
        VERBOSITY=$2
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
    --beam|-b)
        BEAM=$2
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
    --host|-H)
        TMP_HOST=$2
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
TMP_NODE="$TMP_NODE@$TMP_HOST"
CMD="$ERL -pa $BEAM -run nagios_erlang check_application $NODE $APPLICATION -noshell -name $TMP_NODE -setcookie $COOKIE"
if [ $VERBOSITY -ge 3 ]
 then
    echo "version: $VERSION"
    echo "application: $APPLICATION"
    echo "node: $NODE"
    echo "cookie: $COOKIE"
    echo "tmp_node: $TMP_NODE"
    echo "erl: $ERL"
    echo "beam: $BEAM"
    echo "verbosity: $VERBOSITY"
    echo "full command: $CMD"
fi
$CMD

