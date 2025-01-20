#!/bin/bash
domain=${1}
METDIR=/scratch/user/qying/Fe_PKU/mcip_out/${domain}

#for days in {12..28}
for days in 19 
do
ddd=$( date -d "2022/4/${days}" +%j )
METCRO3D=$METDIR/METCRO3D_2022-04-${days}
layerfile=$METCRO3D
#layerfile=/scratch/user/qying/Fe_PKU/emission/${domain}/${domain}.ENE_2018_0.1x0.1_04_w0.ncf
OUTFILE=/scratch/user/qying/Fe_PKU/emission/${domain}/${domain}.SAPRC07T.fire3d.new3.202204${days}.ncf
rm -rf $OUTFILE
cat << EOF | ./mergefire.exe
./KSCN.${domain}.SAPRC07T.fire3d.04${days}2022.txt
$layerfile
30
.true.
2022${ddd}
000000
$OUTFILE
EOF
done
