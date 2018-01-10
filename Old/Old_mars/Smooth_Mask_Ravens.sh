#Smooth and mask the Ravens data

cat /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/bblids_scanids_date.csv | while IFS="," read -r a b c ;

do

path=`ls -d /data/joy/BBL/studies/pnc/processedData/structural/ravens/${a}/${c}x${b}/*_150_2mm.nii.gz`;

mask=/data/joy/BBL/templates/pncRavensTemplate/mniGreyCombEons2mmEro.nii.gz

outdir=/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/images/Smoothed_Masked_Ravens

pathName=$(echo $path|cut -d, -f1)

fileName=$(basename $path | cut -d. -f1)

echo $pathName                                                                                                                                                          
echo ${fileName}                                                                                                                                                          
fslmaths $pathName -s 0.85 -mas $mask $outdir/${fileName}_s2mm_masked.nii.gz

done

