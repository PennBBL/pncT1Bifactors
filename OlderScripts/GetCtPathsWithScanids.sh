#Get the cortical thickness paths from the data freeze folder.

cat /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1360_bblids_scanids.csv | while IFS="," read -r a b ;

do 

path=`ls -d /data/joy/BBL/studies/pnc/n1601_dataFreeze2016/neuroimaging/t1struct/voxelwiseMaps_antsCt/${b}_CorticalThicknessNormalizedToTemplate2mm.nii.gz`;

echo $b,$path >> /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1360_antsCtPathsWithScanids.csv

done

