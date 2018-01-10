#Double check that there is no missing data after running the "FindMissingRavens.sh" script

cat /data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/bblids_scanids_dates.csv | while IFS="," read -r a b c ;
do

if [ ! -d /data/joy/BBL/studies/pnc/processedData/structural/ravens/${a} ]; then

    echo $a,$b,$c >> /data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/MissingDirs_2.csv

fi


if [ ! -d /data/joy/BBL/studies/pnc/processedData/structural/ravens/${a}/*x${b} ]; then

    echo $a,$b,$c >> /data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/MissingScanidDirs_2.csv

fi


if [ ! -f /data/joy/BBL/studies/pnc/processedData/structural/ravens/${a}/*x${b}/*_150.nii.gz ]; then

    echo $a,$b,$c >> /data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/Missing150_2.csv

fi


if [ ! -f /data/joy/BBL/studies/pnc/processedData/structural/ravens/${a}/*x${b}/*_150_2mm.nii.gz ]; then

    echo $a,$b,$c >> /data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/Missing2mm_2.csv

fi


done

