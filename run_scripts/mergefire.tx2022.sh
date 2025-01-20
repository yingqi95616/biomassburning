#!/bin/bash
domain=${1}
METDIR=/scratch/user/qying/Texas_Wildfire/mcip_out/${domain}

for days in {28..28}
do
ddd=$( date -d "2022/3/${days}" +%j )
METCRO3D=$METDIR/METCRO3D_2022-03-${days}
layerfile=$METCRO3D
#layerfile=/scratch/user/qying/Fe_PKU/emission/${domain}/${domain}.ENE_2018_0.1x0.1_04_w0.ncf
OUTFILE=/scratch/user/qying/Texas_Wildfire/emission/${domain}/${domain}.SAPRC07T.fire3d.202203${days}.ncf
N_LAYER_EMIS=15 # not used if GEN_FIRE_EMIS_FILE_ONLY=.false. 
GEN_FIRE_EMIS_FILE_ONLY=.true.
rm -rf $OUTFILE
cat << EOF | ./mergefire.exe
./TX2022.${domain}.SAPRC07T.fire3d.03${days}2022.txt
$layerfile
$N_LAYER_EMIS
${GEN_FIRE_EMIS_FILE_ONLY}
2022${ddd}
000000
$OUTFILE
EOF
done
