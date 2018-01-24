#Get the Ravens paths.

cat /data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/bblids_scanids_dates.csv | while IFS="," read -r a b c ;

do

path=`ls -d /data/joy/BBL/studies/pnc/processedData/structural/ravens/${a}/${c}x${b}/*_150_2mm.nii.gz`;


fileName=`echo $path | cut -d " " -f1`;

echo $a,$b,$fileName >> /data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1355_RavensPaths.csv

done

