#Create a mask that will remove CT images with too many 0 or <.1 values.

#this is where the output mask goes                                                                                                                                             
outdir=/data/joy/BBL/projects/pncT1AcrossDisorder/images/antsCT

echo "outdir is $outdir"

echo ""

#this is your list of input image paths                                                                                                                                         

imgList=`find /data/joy/BBL/studies/pnc/processedData/structural/antsCorticalThickness/*/*/CorticalThicknessNormalizedToTemplate2mm.nii.gz`;

#Create mask
for i in $imgList; do 

#get scanid: first echo the path (echo $i), then cut the path up by delimiter "/" (-d'/'), then take the 12th field (-f12) which is "2632_CorticalThicknessNormalizedToTemplate2mm.nii.gz", then cut up the file name by delimiter "." and just keep the first field (which will be the file name without ".nii.gz").                                                                                                                                                              
        fileName=$(echo $i | cut -d'/' -f12  | cut -d'.' -f1)
        echo "file name is $fileName"

	ThresholdImage 3 $i ${outdir}/${fileName}_mask.nii.gz 0.1 Inf
done

AverageImages 3 ${outdir}/thicknessCoverage.nii.gz 0  ${outdir}/*mask.nii.gz

fslmaths ${outdir}/thicknessCoverage.nii.gz -thr .9 -bin ${outdir}/n1360_antsCT_mask.nii.gz
