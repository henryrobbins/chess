# Chess Installation

Welcome to Chess! To start, run `make build` to build the game from the source
code provided in the zip file. Next, run `make test` to verify everything is
working properly. You can now run `make play` to play chess in the command
line! To move a piece, type a command of the form
`move [piece_id] [sqaure] to [sqaure]` where `piece_id` is in `[P,R,B,N,K,Q]`
and `square` is a two-character string of the form `[file][rank]`. For example,
`move P d2 to d4` is the command to move the pawn at sqaure d2 to d4. To quit
the game, type `quit`. Lastly, if you would like to generate documentation,
run `make docs`.