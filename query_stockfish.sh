#!/bin/bash

# get the operation system
# NOTE: only supports linux and OSX (darwin)
OS=`uname`
path=$1

expect << EOF
    spawn $path

    expect -timeout 1  Linscott

    send "setoption name UCI_LimitStrength value true \r isready \r"

    expect -timeout 1  readyok

    send "setoption name UCI_Elo value $3 \r isready \r"

    expect -timeout 1  readyok

    send "position fen $2 \r isready \r"

    expect -timeout 1  readyok

    send "go depth 12 \r"

    expect -timeout 3 readyok
EOF
