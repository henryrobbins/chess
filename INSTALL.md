# Installation Instructions

## (OSX)
- Install [Homebrew](https://brew.sh/)
- `brew install gtk+`
- `opam install lablgtk`
- `brew install stockfish`
- Verify that the install location is `/usr/local/Cellar/stockfish/13` <br>
  (You may verify that `brew --prefix` returns `/usr/local`)

## (WSL)
- `sudo apt update`
- `sudo apt install libgtk2.0-dev`
- `opam install lablgtk`
- install an X-server like xming https://sourceforge.net/projects/xming/
- launch xming
- run `export DISPLAY=:0` in ubuntu to link the linux GUI to Xming
- `sudo apt-get install expect`
