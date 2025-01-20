#!/bin/bash
# speciation profiles
domain=D001

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
METDIR=/scratch/user/minsusee/CMAQ/ev-project/met/2017/${domain}

#for days in {12..30}
for days in {01..31}
do

GRIDCRO2D=${MCIP_OUTDIR}/GRIDCRO2D_2017-07-${days}
METCRO3D=$METDIR/METCRO3D_2017-07-${days}
METDOT3D=$METDIR/METDOT3D_2017-07-${days}
METCRO2D=$METDIR/METCRO2D_2017-07-${days}

MODIS=./data/072017/fire_archive_M-C61_418637.csv
VIIRS=./data/072017/fire_archive_SV-C2_418639.csv

# gridded fires with plume rise. 
# gridded FRP and burned area are also included
OUTFILE=./BC2017.${domain}.SAPRC07T.fire3d.07${days}2017.txt

rm -f $OUTFILE 

DEBUG=.false.
cat << EOF | ./genfire.exe
$DEBUG
2017 7 ${days}
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
