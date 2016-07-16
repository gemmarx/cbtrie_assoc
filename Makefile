
TARGET = example_assoc_cbtrie

FC     := gfortran
FFLAGS := -O2 -fimplicit-none

all: $(TARGET)

example_string: converter.f03 string.f03 example_string.f03
	$(FC) $(FFLAGS) -o $@ $^

example_deque: converter.f03 string.f03 deque.f03 example_deque.f03
	$(FC) $(FFLAGS) -o $@ $^

example_typack: converter.f03 typack.f03 example_typack.f03
	$(FC) $(FFLAGS) -o $@ $^

libcbtrie.a: converter.f03 typack.f03 assoc_cbtrie.f03
	$(FC) $(FFLAGS) -c $^ -fPIC
	ar csr $@ converter.o typack.o assoc_cbtrie.o
	@rm -f *.o

example_assoc_cbtrie: example_assoc_cbtrie.f03 libcbtrie.a
	$(FC) $(FFLAGS) -o $@ $< -L. -lcbtrie

cleanlib:
	-rm -f *.o *.mod libcbtrie.a

clean: cleanlib
	-rm -f example_assoc_cbtrie example_typack example_deque example_string

