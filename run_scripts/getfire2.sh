#!/bin/bash
year=${1}
month=${2}
day=${3}
rm -f MODIS* VNP*
wget https://firms.modaps.eosdis.nasa.gov/data/active_fire/c6/csv/MODIS_C6_Global_7d.csv
wget https://firms.modaps.eosdis.nasa.gov/data/active_fire/viirs/csv/VNP14IMGTDL_NRT_Global_7d.csv
yymmdd=$( date -d "${year}/${month}/${day}" +%Y-%m-%d )
cp modis.header.txt ${yymmdd}-MODIS_C6_Global_7d.csv
cp viirs.header.txt ${yymmdd}-VNP14IMGTDL_NRT_Global_7d.csv

for f in MODIS_C6_Global_7d.csv VNP14IMGTDL_NRT_Global_7d.csv
do
cp ${f} temp.txt
grep ${yymmdd} temp.txt >> ${yymmdd}-$f 
done
rm -f temp.txt 
