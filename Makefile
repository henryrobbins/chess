MODULES=board command validation
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
GUI=gui.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
        OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'debug' -I main $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

game:
	$(OCAMLBUILD) -tag 'debug' -I gui $(GUI) && OCAMLRUNPARAM=b ./$(GUI)

zip:
	zip chess.zip *.ml* *.sh _tags .merlin .ocamlformat .ocamlinit *.json test_board_jsons/* INSTALL.md Makefile

docs: docs-public docs-private

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package yojson \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package yojson lablgtk2 \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private chess.zip