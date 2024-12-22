OCAMLOPT = ocamlopt
OCAMLFLAGS = -w +A -warn-error +A
TARGET = ex02
SOURCES = calc.ml main.ml

re : fclean $(TARGET)

$(TARGET): $(SOURCES:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLFLAGS) -o $(TARGET) $(SOURCES:.ml=.cmx)

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

clean:
	@rm -f rm -f *.cm[iox] *.o

fclean: clean
	@rm -f $(TARGET)
