#Create lists of those missing a CT, GMD, or volume (Ravens) nii.gz file on chead.

#!/bin/bash

cat /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1375_bblids_scanids.csv | while IFS="," read -r a b ;
do

if [ ! -f /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/voxelwiseMaps_antsCt/${b}_CorticalThicknessNormalizedToTemplate2mm.nii.gz ]; then

    echo $a,$b >> /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/MissingCT.csv

fi


if [ ! -f /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/voxelwiseMaps_gmd/${b}_atropos3class_prob02SubjToTemp2mm.nii.gz ]; then

    echo $a,$b >> /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/MissingGMD.csv

fi


if [ ! -f /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/voxelwiseMaps_ravens/${b}_RAVENS_2GM_2mm.nii.gz ]; then

    echo $a,$b >> /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/MissingRavens.csv

fi

done


if [ ! -f /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/MissingCT.csv ]; then

    echo "No missing CT data."

fi

if [ ! -f /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/MissingGMD.csv ]; then

    echo "No missing GMD data."

fi

if [ ! -f /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/MissingRavens.csv ]; then

    echo "No missing Ravens data."

fi
