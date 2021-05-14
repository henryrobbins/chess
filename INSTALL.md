## Installation Instructions (GUI)

(OSX)
- Install [Homebrew](https://brew.sh/)
- `brew install gtk+`
- `opam install lablgtk`

(WSL):
- `sudo apt update`
- `sudo apt install libgtk2.0-dev`
- `opam install lablgtk`
- install an X-server like xming https://sourceforge.net/projects/xming/
- launch xming
- run `export DISPLAY=:0` in ubuntu to link the linux GUI to Xming

(WORK IN PROGRESS)
- `sudo apt-get install expect`

## OCaml Chess

Welcome to Chess! To start, run `make build` to build the game from the source
code provided in the zip file. Next, run `make test` to verify everything is
working properly.

You can now run `make command-line` to play chess in the command line! To move
a piece, type a command of the form `move [piece_id] [sqaure] to [sqaure]`
where `piece_id` is in `[P,R,B,N,K,Q]` and `square` is a two-character string
of the form `[file][rank]`. For example, `move P d2 to d4` is the command to
move the pawn at sqaure d2 to d4. To quit the game, type `quit`. Lastly, if you
would like to generate documentation, run `make docs`.

Alternatively, you can run `make gui` to play chess via a graphical user
interface! To move a piece, first click on the piece to move and then the
square you wish to move it to. The color of the current player is given
at the bottom of the GUI.
