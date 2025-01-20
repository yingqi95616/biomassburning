# biomassburning
The program generates wildfire and open biomass burning emission inputs for the CMAQ model using data from MODIS and VIIRS datasets. 

CMAQ-ready emissions are generated in three steps. 
1. Satellite data needs to be downloaded from the [Fire Information for Resource Management System] (https://firms.modaps.eosdis.nasa.gov/download/) as csv files. These are for archived historical fires. Active fires can also be downloaded from the [FIRMS website](https://firms.modaps.eosdis.nasa.gov/active_fire/). In the runscript directory, getfire2.sh and getfire3.sh are examples to show how to download the active fires. 
2. Run genfire to generate the fire emissions. It needs meteorological inputs generated from MCIP for plume rise calculations. There are some example run scripts in the run_scripts directly.
3. Run mergefire to generate CMAQ-ready IOAPI emission files. The program can merge the fire emissions with the existing emission files or generate a file that contains fire emissions only. There are some example run scripts in the run_scripts directly.
   
The program needs netcdf and IOAPI libraries. You should already have them when you install the CMAQ model. Also, the program needs a high-resolution landuse database. Currently, we use the MODIS data from the WPS V4 Geographical Static Data Downloads Page ([modis_landuse_20class_15s_with_lakes.tar.gz](https://www2.mmm.ucar.edu/wrf/src/wps_files/modis_landuse_20class_15s_with_lakes.tar.gz)). The file is too big to include in the repository. Please download directly using the provided link. 

For more details, please see the [paper] (https://doi.org/10.1016/j.atmosenv.2024.120942).  
