#Create a mask that will remove Ravens images with too many 0 or <.1 values.

cat /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1375_bblids_scanids.csv | while IFS="," read -r a b ;

do 

#this is where the output mask goes                                                                                                                                             
outdir=/data/joy/BBL/projects/pncT1AcrossDisorder/images/Ravens

#this is your list of input image paths                                                                                                                                         
imgList=`ls -d /data/joy/BBL/studies/pnc/n1601_dataFreeze2016/neuroimaging/t1struct/voxelwiseMaps_ravens/${a}_${b}_RAVENS_2GM_2mm.nii.gz`;

#Create mask
for i in $imgList; do 

#get scanid: first echo the path (echo $i), then cut the path up by delimiter "/" (-d'/'), then take the 10th field (-f11) which is the file name, then cut up the file name by delimiter "." and just keep the first field (which will be the file name without ".nii.gz").                                                                                                                                                              
        fileName=$(echo $i | cut -d'/' -f11  | cut -d'.' -f1)
        echo "file name is $fileName"

	ThresholdImage 3 $i ${outdir}/${fileName}_mask.nii.gz 0.1 Inf
done

done

#Average the masks together and binarize/threshold the final mask.
outdir=/data/joy/BBL/projects/pncT1AcrossDisorder/images/Ravens

AverageImages 3 ${outdir}/RavensCoverage.nii.gz 0  ${outdir}/*mask.nii.gz

fslmaths ${outdir}/RavensCoverage.nii.gz -thr .9 -bin ${outdir}/n1375_Ravens_mask.nii.gz
