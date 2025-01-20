# Fortran Compiler
F90 =  ifort
FC = icc
# Library (must be compiled with the same Fortran compiler)
#LIBS = -I./ -L./ -lioapi -lnetcdff -lnetcdf

# compiler flags for debugging code
#FFLAGS = -check all -C -g -warn all,nodec,interfaces -gen_interfaces -traceback -fpe0 -O0 -132 -openmp 

# compiler flags for production code with openMP
#FFLAGS = -check all -g -traceback -xT -132
# compiler flags for production code without openMP
#FFLAGS = -g -traceback -132 -fpe0 -check all
FFLAGS = -O0 -132 -check all -traceback -g -fpe0
CFLAGS = -O0 -D_UNDERSCORE 

exec = testread.exe 
default = $(exec)
all: testread.exe

objects = testread.o read_geogrid.o read_modis_500m.o
	
testread.exe: $(objects)
	 $(F90) $(FFLAGS) -o testread.exe $(objects) $(LIBS) 

.f.o :
	$(F90) $(FFLAGS) -c $< $(LIBS) 

.F.o :
	$(F90) $(FFLAGS) -c $< $(LIBS) 

.c.o :
	$(FC) $(CFLAGS) -c $<

clean:
	rm -fr *.o *.oo *.mod *~

