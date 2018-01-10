#Create lists of bblids/scanids of those missing either:
#1) a Ravens subject folder on chead, 2) the appropriate scanid folder on chead, 3) the *150.nii.gz Ravens file on chead,  or 4) the *150_2mm.nii.gz Ravens file on chead.
#Make directories for missing subject folders and scanid folders
#Copy the bblid lists to monstrum (needed for the CopyRavens.sh script)

#!/bin/bash

cat /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/bblids_scanids_date.csv | while IFS="," read -r a b c ;
do

if [ ! -d /data/joy/BBL/studies/pnc/processedData/structural/ravens/${a} ]; then

    echo $a,$b,$c >> /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/MissingDirs.csv
    
    subjFolder="/data/joy/BBL/studies/pnc/processedData/structural/ravens/${a}";
    echo "subject folder is $subjFolder";
    mkdir "$subjFolder"

fi


if [ ! -d /data/joy/BBL/studies/pnc/processedData/structural/ravens/${a}/*x${b} ]; then

    echo $a,$b,$c >> /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/MissingScanidDirs.csv

    scanFolder="/data/joy/BBL/studies/pnc/processedData/structural/ravens/${a}/${c}x${b}/";
    echo "scan folder is $scanFolder";
    mkdir "$scanFolder"

fi


if [ ! -f /data/joy/BBL/studies/pnc/processedData/structural/ravens/${a}/*x${b}/*_150.nii.gz ]; then

    echo $a,$b,$c >> /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/Missing150.csv

fi


if [ ! -f /data/joy/BBL/studies/pnc/processedData/structural/ravens/${a}/*x${b}/*_150_2mm.nii.gz ]; then

    echo $a,$b,$c >> /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/Missing2mm.csv

fi


done


#Copy the missing bblid list to monstrum.

cd /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData
scp "Missing150.csv" antoniak@youknowwho.uphs.upenn.edu:/import/monstrum/Users/antoniak/PNC_MDD/subjectData

