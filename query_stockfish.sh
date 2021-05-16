#!/bin/bash

# get the operation system
# NOTE: only supports linux and OSX (darwin)
OS=`uname`

if [ $OS = "Linux" ]; then
    path="./stockfish_13_linux_x64_bmi2/stockfish_13_linux_x64_bmi2/stockfish_13_linux_x64_bmi2"
else
    path="/usr/local/Cellar/stockfish/13/bin/stockfish"
fi

expect << EOF
    spawn $path

    expect -timeout 1  Linscott

    send "setoption name Ponder value false \r isready \r"

    expect -timeout 1  readyok

    send "position fen $1 \r isready \r"

    expect -timeout 1  readyok

    send "go depth 15 \r"

    expect -timeout 2 readyok
EOF
