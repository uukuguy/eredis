#!/bin/sh
APP=gandalf

for d in dev/dev*; do 
    if [ ! "$d" = "dev/dev1" ]; then
        $d/bin/${APP}-admin cluster join ${APP}1@127.0.0.1
    fi
done

./clusterstatus.sh
