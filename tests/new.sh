#!/bin/sh
cd $(dirname "$0")
testname="$1"
[ -n "$testname" ] || exit 1
#cmdrecord tests/"$testname".t --source input --product output --no-stdin --env empty -- ../../dist/build/frquotes/frquotes bla input output
cmdrecord "$testname".t --env empty -- ../../dist/build/frquotes/frquotes
