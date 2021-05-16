#!/bin/bash
echo $1
    /usr/bin/expect << EOF
    spawn ./stockfish_13_linux_x64_bmi2/stockfish_13_linux_x64_bmi2/stockfish_13_linux_x64_bmi2

    expect -timeout 1  Linscott 

    send "setoption name Ponder value false \r isready \r"

    expect -timeout 1  readyok

    send "position fen $1 \r isready \r"

    expect -timeout 1  readyok

    send "go depth 15 \r" 

    expect -timeout 2  readyok
EOF


