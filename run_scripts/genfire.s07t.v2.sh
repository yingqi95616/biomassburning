#!/bin/bash
#
# Air Quality Forecasting System version 1.0.2
# programmed by: Qi Ying
#
# This is the main run script to generate wildfire emissions.
# Last revision: 2/20/2024

source setdir.sh

if [ $# -ne 4 ]; then 
  echo "Usage: " $0 "year month day domain"
  exit -1
fi
year=${1}
month=${2}
day=${3}
domain=${4}

yymmdd=$( date -d "${year}/${month}/${day}" +%Y-%m-%d )

# download data
./getfire3.sh 

# speciation profiles
export GAS_MECH=SAPRC07T
export PM_MECH=AERO6

VOCDIR=${AQ_MODEL_ROOT}/tools/fire/profiles/VOC/${GAS_MECH}
PMDIR=${AQ_MODEL_ROOT}/tools/fire/profiles/PM2.5/${PM_MECH}

NMHC_FOREST=profile.voc.forest.txt
NMHC_CROP=profile.voc.crop_res.txt
NMHC_GRASS=profile.voc.grass.txt
NMHC_SHRUB=profile.voc.shrubland.txt
NMHC_SAVANA=profile.voc.savana.txt
NMHC_OTHER=profile.voc.other.txt
PM25_FOREST=profile.pm.forest.txt
PM25_CROP=profile.pm.crop_res.txt
PM25_GRASS=profile.pm.grass.txt
PM25_SHRUB=profile.pm.shrubland.txt
PM25_SAVANA=profile.pm.savana.txt
PM25_OTHER=profile.pm.other.txt

DIR_GEOG=/opt/WPS_GEOG/modis_landuse_20class_15s_with_lakes
METDIR=${AQ_MODEL_DATA}/mcip_out/${domain}
METCRO3D=$METDIR/METCRO3D_${yymmdd}
METDOT3D=$METDIR/METDOT3D_${yymmdd}
METCRO2D=$METDIR/METCRO2D_${yymmdd}

MODIS=MODIS_C6_1_Global_48h.csv
VIIRS=SUOMI_VIIRS_C2_Global_48h.csv
VIIRS2=J1_VIIRS_C2_Global_48h.csv

OUTFILE=${AQ_MODEL_DATA}/emission/wildfire/${yymmdd}.${domain}.${GAS_MECH}-${PM_MECH}.fire3d.txt
OUTFILE2=./most_recent_fire3d.${domain}.txt
rm -f $OUTFILE 

DEBUG=.false.
ulimit -s unlimited
cat << EOF | ${AQ_MODEL_ROOT}/tools/fire/genfire.exe
$DEBUG
${year} ${month} ${day}
${VOCDIR}/$NMHC_FOREST
${VOCDIR}/$NMHC_SAVANA
${VOCDIR}/$NMHC_SHRUB
${VOCDIR}/$NMHC_GRASS
${VOCDIR}/$NMHC_CROP
${VOCDIR}/$NMHC_OTHER
${PMDIR}/$PM25_FOREST
${PMDIR}/$PM25_SAVANA
${PMDIR}/$PM25_SHRUB
${PMDIR}/$PM25_GRASS
${PMDIR}/$PM25_CROP
${PMDIR}/$PM25_OTHER
$DIR_GEOG
$METCRO3D
$METDOT3D
$METCRO2D
3
$MODIS
$VIIRS
$VIIRS2
$OUTFILE
EOF

cp $OUTFILE $OUTFILE2
