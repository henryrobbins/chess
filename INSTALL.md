# Installation Instructions

## (OSX)
- Install [Homebrew](https://brew.sh/)
- `brew install gtk+`
- `opam install lablgtk`
- `brew install stockfish`

## (WSL)
- `sudo apt update`
- `sudo apt install libgtk2.0-dev`
- `opam install lablgtk`
- install an X-server like xming https://sourceforge.net/projects/xming/
- launch xming
- run `export DISPLAY=:0` in ubuntu to link the linux GUI to Xming
- `sudo apt-get install expect`
- Download [Stockfish 13](https://stockfishchess.org/download/)
