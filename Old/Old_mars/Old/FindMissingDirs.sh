#Create list of bblids/scanids of those missing their Raven's data folder on chead.

cat /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/bblids_scanids.csv | while IFS="," read -r a b ;
do

if [ ! -d /data/joy/BBL/studies/pnc/processedData/structural/ravens/${a} ]; then

    echo $a,$b >> /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/MissingDirs.csv

fi

done
