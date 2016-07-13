
TARGET = example_assoc_critbit

FC     := gfortran
FFLAGS := -O2 -fimplicit-none

all: $(TARGET)

example_string: converter.f03 string.f03 example_string.f03
	$(FC) $(FFLAGS) -o $@ $^

example_deque: converter.f03 string.f03 deque.f03 example_deque.f03
	$(FC) $(FFLAGS) -o $@ $^

example_typack: converter.f03 typack.f03 example_typack.f03
	$(FC) $(FFLAGS) -o $@ $^

example_assoc_critbit: converter.f03 typack.f03 assoc_critbit.f03 example_assoc_critbit.f03
	$(FC) $(FFLAGS) -o $@ $^

clean:
	-rm -rf *.o *.mod

