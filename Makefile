
TARGET := example_assoc_cbtrie

FC     := gfortran
FFLAGS := -O2 -fimplicit-none

SRC := ./src
OLD := $(SRC)/old
EXP := ./examples

all: $(TARGET)

example_string: $(SRC)/converter.f03 $(OLD)/string.f03 $(EXP)/example_string.f03
	$(FC) $(FFLAGS) -o $@ $^

example_deque: $(SRC)/converter.f03 $(OLD)/string.f03 $(OLD)/deque.f03 $(EXP)/example_deque.f03
	$(FC) $(FFLAGS) -o $@ $^

example_typack: $(SRC)/converter.f03 $(SRC)/typack.f03 $(EXP)/example_typack.f03
	$(FC) $(FFLAGS) -o $@ $^

libcbtrie.a: $(SRC)/converter.f03 $(SRC)/typack.f03 $(SRC)/assoc_cbtrie.f03
	$(FC) $(FFLAGS) -c $^ -fPIC
	ar csr $@ converter.o typack.o assoc_cbtrie.o
	@rm -f *.o

example_assoc_cbtrie: $(EXP)/example_assoc_cbtrie.f03 libcbtrie.a
	$(FC) $(FFLAGS) -o $@ $< -L. -lcbtrie

cleanlib:
	-rm -f *.o *.mod libcbtrie.a

clean: cleanlib
	-rm -f example_assoc_cbtrie example_typack example_deque example_string

