
FC     := gfortran
FFLAGS := -O2 -fimplicit-none

all: example_assoc_critbit

example_assoc_critbit: string_array.f03 assoc_critbit.f03 example_assoc_critbit.f03
	$(FC) $(FFLAGS) -o $@ $^

clean:
	-rm -rf *.o *.mod

