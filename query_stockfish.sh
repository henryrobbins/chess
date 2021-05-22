#!/bin/bash

# get the operation system
# NOTE: only supports linux and OSX (darwin)
OS=`uname`

if [ $OS = "Darwin" ]; then
    path="/usr/local/Cellar/stockfish/13/bin/stockfish"
else
    path="./stockfish_13_linux_x64_bmi2/stockfish_13_linux_x64_bmi2/stockfish_13_linux_x64_bmi2"
fi

expect << EOF
    spawn $path

    expect -timeout 1  Linscott

    send "setoption name UCI_LimitStrength value true \r isready \r"

    expect -timeout 1  readyok

    send "setoption name UCI_Elo value $2 \r isready \r"

    expect -timeout 1  readyok

    send "position fen $1 \r isready \r"

    expect -timeout 1  readyok

    send "go depth 12 \r"

    expect -timeout 3 readyok
EOF
