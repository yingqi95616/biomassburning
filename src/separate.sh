#!/bin/bash
year=2022
modis_input=fire_archive_SV-C2_419250.csv
for month in {01..12} 
do 
  echo ${month}
  cat modis.header.txt > fire_archive_SV-C2_${year}-${month}.csv 
  grep "${year}-${month}" $modis_input >> fire_archive_SV-C2_${year}-${month}.csv
done
