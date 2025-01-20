#!/bin/bash
domain=D001
METDIR=/scratch/user/minsusee/CMAQ/ev-project/met/2017/${domain}

#for days in {12..28}
for days in {01..31}
do
ddd=$( date -d "2017/7/${days}" +%j )
METCRO3D=$METDIR/METCRO3D_2017-07-${days}
layerfile=$METCRO3D
#layerfile=/scratch/user/qying/Fe_PKU/emission/${domain}/${domain}.ENE_2018_0.1x0.1_04_w0.ncf
OUTFILE=./output/BC2017/${domain}.SAPRC07T.fire3d.201707${days}.ncf
GEN_FIRE_EMIS_FILE=.true.
rm -rf $OUTFILE
cat << EOF | ./mergefire.exe
./BC2017.${domain}.SAPRC07T.fire3d.07${days}2017.txt
${layerfile}
${GEN_FIRE_EMIS_FIRE}
2017${ddd}
000000
$OUTFILE
EOF
done
