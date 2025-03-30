include ../../Makeinclude.txt

exec = genfire.exe 
default = $(exec)
all: genfire.exe mergefire.exe

objects = genfire.o csv.o strings.o fire_plmris.o preplm.o plsprd.o read_geogrid.o read_modis_500m.o gridgroup.o findgroup.o
	
objects1 = mergefire.o strings.o 

genfire.exe: $(objects)
	 $(F90) $(FFLAGS) $(FLINK) -o genfire.exe $(objects) $(LIBS) 

mergefire.exe: $(objects1)
	 $(F90) $(FFLAGS) $(FLINK) -o mergefire.exe $(objects) $(LIBS) 

.f.o :
	$(F90) $(FFLAGS) -c $< $(INCLUDE) $(LIBS) 

.F.o :
	$(F90) $(FFLAGS) -c $< $(INCLUDE) $(LIBS) 

.c.o :
	$(CC) $(CFLAGS) -c $<

clean:
	rm -fr *.o *.oo *.mod *~

