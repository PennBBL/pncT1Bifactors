#Get the cortical thickness paths.

cat /data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/subjectData/n1360_bblids_scanids.csv | while IFS="," read -r a b ;

do 

path=`ls -d /data/joy/BBL/studies/pnc/processedData/structural/antsCorticalThickness/${a}/*x${b}/CorticalThicknessNormalizedToTemplate.nii.gz`;

echo $path >> /data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/subjectData/n1360_antsCt1mmImages.txt

done

