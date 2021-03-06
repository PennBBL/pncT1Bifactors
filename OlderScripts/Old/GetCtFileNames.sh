#Get the cortical thickness paths from the data freeze folder.

cat /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1360_bblids_scanids.csv | while IFS="," read -r a b ;

do 

path=`ls -d /data/joy/BBL/studies/pnc/n1601_dataFreeze2016/n1601_voxelwiseMaps/antsCt/${b}_CorticalThicknessNormalizedToTemplate2mm.nii.gz`;

#get just the file name without the path: first echo the path (echo $i), then cut the path up by delimiter "/" (-d'/'), then take the 10th field (-f10), which is the file name.

        fileName=$(echo $path | cut -d'/' -f10 )
        echo "File name is $fileName"


echo $fileName >> /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1360_antsCtFileNames.csv

done

