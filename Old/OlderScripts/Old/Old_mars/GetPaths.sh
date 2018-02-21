#Get the Ravens paths.

cat /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/bblids_scanids_date.csv | while IFS="," read -r a b c ;

do

path=`ls -d /data/joy/BBL/studies/pnc/processedData/structural/ravens/${a}/${c}x${b}/*_150_2mm.nii.gz`;


fileName=`echo $path | cut -d " " -f1`;

echo $a,$b,$fileName >> /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1385_RavensPaths.csv

done

