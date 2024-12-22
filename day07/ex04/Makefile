OCAMLOPT = ocamlopt
OCAMLFLAGS = -w +A -warn-error +A
TARGET = ex04
SOURCES = atom.ml boron.ml sodium.ml carbon.ml helium.ml hydrogen.ml oxygen.ml zinc.ml molecule.ml water.ml carbon_dioxide.ml sodium_carbonate.ml zinc_oxide.ml sodium_helide.ml  dioxygen.ml carbon_dioxyde.ml alkane.ml ethane.ml methane.ml octane.ml reaction.ml alkane_combustion.ml main.ml

re : fclean $(TARGET)

$(TARGET): $(SOURCES:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLFLAGS) -o $(TARGET) $(SOURCES:.ml=.cmx)

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

clean:
	@rm -f rm -f *.cm[iox] *.o

fclean: clean
	@rm -f $(TARGET)
