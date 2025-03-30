# intel compiler
F90 = ifort
FC  = ifort
CC  = icc

# Library (must be compiled with the same Fortran compiler)
LIBS = -L/scratch/user/qying/opt/ioapi-3.2/Linux2_x86_64ifort -lioapi \
            -L/scratch/user/qying/opt/netcdf-4.7.4-intel/lib -lnetcdff -lnetcdf
INCLUDE = -I/scratch/user/qying/opt/netcdf-4.7.4-intel/include \
          -I/scratch/user/qying/opt/ioapi-3.2/ioapi/fixed_src \
          -I/scratch/user/qying/opt/ioapi-3.2/Linux2_x86_64ifort\
          -I./

FFLAGS = -132 -O0 -check all -traceback -g 
CFLAGS = -O3 -D_UNDERSCORE
FLINK = -fopenmp 

exec = genfire.exe 
default = $(exec)
all: genfire.exe mergefire.exe

objects = genfire.o csv.o strings.o fire_plmris.o preplm.o plsprd.o read_geogrid.o read_modis_500m.o gridgroup.o findgroup.o
	
objects1 = mergefire.o strings.o 

genfire.exe: $(objects)
	 $(F90) $(FFLAGS) $(FLINK) -o genfire.exe $(objects) $(LIBS) 

mergefire.exe: $(objects1)
	 $(F90) $(FFLAGS) $(FLINK) -o mergefire.exe $(objects1) $(LIBS) 

.f.o :
	$(F90) $(FFLAGS) -c $< $(INCLUDE) $(LIBS) 

.F.o :
	$(F90) $(FFLAGS) -c $< $(INCLUDE) $(LIBS) 

.c.o :
	$(CC) $(CFLAGS) -c $<

clean:
	rm -fr *.o *.oo *.mod *~

