#!/bin/bash
# speciation profiles
domain=d01
year=2022
month=03

NMHC_FOREST=./profiles/VOC/SAPRC07T/profile.voc.forest.txt
NMHC_CROP=./profiles/VOC/SAPRC07T/profile.voc.crop_res.txt
NMHC_GRASS=./profiles/VOC/SAPRC07T/profile.voc.grass.txt
NMHC_SHRUB=./profiles/VOC/SAPRC07T/profile.voc.shrubland.txt
NMHC_SAVANA=./profiles/VOC/SAPRC07T/profile.voc.savana.txt
NMHC_OTHER=./profiles/VOC/SAPRC07T/profile.voc.other.txt
PM25_FOREST=./profiles/PM2.5/AERO6/profile.pm.forest.txt
PM25_CROP=./profiles/PM2.5/AERO6/profile.pm.crop_res.txt
PM25_GRASS=./profiles/PM2.5/AERO6/profile.pm.grass.txt
PM25_SHRUB=./profiles/PM2.5/AERO6/profile.pm.shrubland.txt
PM25_SAVANA=./profiles/PM2.5/AERO6/profile.pm.savana.txt
PM25_OTHER=./profiles/PM2.5/AERO6/profile.pm.other.txt

DIR_GEOG=./modis_landuse_20class_15s_with_lakes
METDIR=/scratch/user/qying/Texas_Wildfire/mcip_out/${domain}

#for days in {12..29}
for days in {28..28}
do

METCRO3D=$METDIR/METCRO3D_${year}-${month}-${days}
METDOT3D=$METDIR/METDOT3D_${year}-${month}-${days}
METCRO2D=$METDIR/METCRO2D_${year}-${month}-${days}

MODIS=./data/${year}/fire_archive_M-C61_${year}-${month}.csv
VIIRS=./data/${year}/fire_archive_SV-C2_${year}-${month}.csv

# gridded fires with plume rise. 
# gridded FRP and burned area are also included
OUTFILE=./TX${year}.${domain}.SAPRC07T.fire3d.${month}${days}${year}.txt

rm -f $OUTFILE 

DEBUG=.false.
cat << EOF | ./genfire.exe
$DEBUG
${year} ${month} ${days}
$NMHC_FOREST
$NMHC_SAVANA
$NMHC_SHRUB
$NMHC_GRASS
$NMHC_CROP
$NMHC_OTHER
$PM25_FOREST
$PM25_SAVANA
$PM25_SHRUB
$PM25_GRASS
$PM25_CROP
$PM25_OTHER
$DIR_GEOG
$METCRO3D
$METDOT3D
$METCRO2D
2
$MODIS
$VIIRS
$OUTFILE
EOF
done
