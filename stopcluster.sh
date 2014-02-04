#!/bin/sh
APP=gandalf
for d in dev/dev*; do $d/bin/${APP} stop; done
ERTS=`ps aux | grep $APP | grep -v grep | awk '{print $2}'`
kill -9 $ERTS
