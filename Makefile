MODULES=board command validation engine puzzle game_text game_gui endgame
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
TEXT_MAIN=game_text.byte
GUI_MAIN=game_gui.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind \
	-plugin-tag 'package(bisect_ppx-ocamlbuild)'

default: build
        OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

bisect-test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

command-line:
	$(OCAMLBUILD) -tag 'debug' -I game_text $(TEXT_MAIN) && \
	OCAMLRUNPARAM=b ./$(TEXT_MAIN)

gui:
	$(OCAMLBUILD) -tag 'debug' -I game_gui $(GUI_MAIN) && \
	OCAMLRUNPARAM=b ./$(GUI_MAIN)

bisect: clean bisect-test
		bisect-ppx-report html

zip:
	zip chess.zip *.ml* *.sh _tags .merlin .ocamlformat .ocamlinit *.json test_board_jsons/* INSTALL.md README.md Makefile assets/*

docs: docs-public docs-private

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package yojson -package lablgtk2 \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package yojson -package lablgtk2 \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private chess.zip _coverage bisect*.coverage