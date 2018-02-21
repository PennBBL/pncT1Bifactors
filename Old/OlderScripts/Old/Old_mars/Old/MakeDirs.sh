#Create directories for the missing subjects

cat /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/MissingDirs_withDates.csv | while IFS="," read -r a b c ;
do

subjFolder="/data/joy/BBL/studies/pnc/processedData/structural/ravens/${a}";
echo "subject folder is $subjFolder";
mkdir "$subjFolder"

scanFolder="/data/joy/BBL/studies/pnc/processedData/structural/ravens/${a}/${c}x${b}/";
echo "scan folder is $scanFolder";
mkdir "$scanFolder"

done
