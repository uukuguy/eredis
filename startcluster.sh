#!/bin/sh
APP=gandalf
for d in dev/dev*; do $d/bin/${APP} start; done

./clusterstatus.sh
