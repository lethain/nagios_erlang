#!/bin/bash

cd $(dirname $0)

./check_erlang_statistics.sh -e `which erl` -n ejabberd@$HOSTNAME -c `cat /var/lib/ejabberd/.erlang.cookie` -p $1

exit $?
