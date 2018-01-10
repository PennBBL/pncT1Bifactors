#Create downsampled files for those with missing *150_2mm.nii.gz files.

cat /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/Test.csv | while IFS="," read -r a b ;
do

path=`ls -d /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/*_brain_bc_short_RAVENS_150.nii.gz`;

pathName=`echo $path | cut -d " " -f1`;

fileName=`echo $path | cut -d "." -f1`;

#echo $pathName
#echo $fileName

fslmaths "$pathName" -subsamp2 "$name"_2mm.nii.gz

done
