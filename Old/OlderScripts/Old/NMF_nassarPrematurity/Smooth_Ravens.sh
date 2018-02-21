#Smooth the n279 Ravens data for Rula's study
#We tried 4mm FWHM, 6mm FWHM, 8mm FWHM

cat /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n279_nassarPrematurity_bblids_scanids.csv | while IFS="," read -r a b ;

do

path=`ls -d /data/joy/BBL/studies/pnc/n1601_dataFreeze2016/neuroimaging/t1struct/voxelwiseMaps_ravens/${a}_${b}_RAVENS_2GM_2mm.nii.gz`;

mask=/data/joy/BBL/studies/pnc/template/priors/prior_grey_thr01_2mm.nii.gz

outdir=/data/joy/BBL/projects/pncT1AcrossDisorder/images/Ravens/Ravens_smoothed8mm_masked

pathName=$(echo $path|cut -d, -f1)

fileName=$(basename $path | cut -d. -f1)

echo $pathName                                                                                                                                                          
echo ${fileName}  

#NOTE: fslmaths and susan require smoothing parameters in sigma
#FWHM = 2.355*sigma
#4mm FWHM = 1.70 sigma
#6mm FWHM = 2.55 sigma
#8mm FWHM = 3.40 sigma

#susan <input> <bt> <dt> <dim> <use_median> <n_usans> [<usan1> <bt1> [<usan2> <bt2>]] <output>
#<bt> is brightness threshold and should be greater than noise level and less than contrast of edges to be preserved.
#<dt> is spatial size (sigma, i.e., half-width) of smoothing, in mm.
#<dim> is dimensionality (2 or 3), depending on whether smoothing is to be within-plane (2) or fully 3D (3).
#<use_median> determines whether to use a local median filter in the cases where single-point noise is detected (0 or 1).
#<n_usans> determines whether the smoothing area (USAN) is to be found from secondary images (0, 1 or 2).
#A negative value for any brightness threshold will auto-set the threshold at 10% of the robust range

#for i in GM_mod_merg ; do                                                                                                                                                                                                                                 
#  for j in 2 3 4 ; do                                                                                                                                                                                                                                     
#    \$FSLDIR/bin/fslmaths \$i -s \$j \${i}_s\${j}                                                                                                                                                                                                         
#    \$FSLDIR/bin/randomise -i \${i}_s\${j} -o \${i}_s\${j} -m GM_mask -d design.mat -t design.con -V                                                                                                                                                      
#  done                                                                                                                                                                                                                                                    
#done     

fslmaths $pathName -s 3.40 -mas $mask $outdir/${fileName}_smoothed8mm_masked.nii.gz

#susan $pathName <bt> 3.40 3 1 1 [<usan1> <bt1> [<usan2> <bt2>]] $outdir/${fileName}_smoothedSusan8mm.nii.gz

done
