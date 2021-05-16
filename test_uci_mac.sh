#!/bin/bash
expect << EOF
    spawn /usr/local/Cellar/stockfish/13/bin/stockfish

    expect -timeout 1  Linscott

    send "setoption name Ponder value false \r isready \r"

    expect -timeout 1  readyok

    send "position fen $1 \r isready \r"

    expect -timeout 1  readyok

    send "go depth 15 \r"

    expect -timeout 2 readyok
EOF
