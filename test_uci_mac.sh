#!/bin/bash
/usr/bin/expect << EOF
    spawn stockfish

    expect -timeout 1  Linscott

    send "setoption name Ponder value false \r isready \r"

    expect -timeout 1  readyok

    send "position fen $1 \r isready \r"

    expect -timeout 1  readyok

    send "go depth 15 \r"

    expect -timeout 3  readyok
EOF


