#!/bin/bash
domain=${1}
METDIR=/scratch/user/qying/Tibet_PKU/mcip_out/${domain}
month=05

for days in {01..31}
do
ddd=$( date -d "2022/${month}/${days}" +%j )
METCRO3D=$METDIR/METCRO3D_2022-${month}-${days}
layerfile=$METCRO3D
#layerfile=/scratch/user/qying/Fe_PKU/emission/${domain}/${domain}.ENE_2018_0.1x0.1_04_w0.ncf
OUTFILE=/scratch/user/qying/Tibet_PKU/emis/fire/${domain}/${domain}.SAPRC07T.fire3d.new3.2022${month}${days}.ncf
rm -rf $OUTFILE
cat << EOF | ./mergefire.exe
./SEA.${domain}.SAPRC07T.fire3d.${month}${days}2022.txt
$layerfile
30
.true.
2022${ddd}
000000
$OUTFILE
EOF
done
