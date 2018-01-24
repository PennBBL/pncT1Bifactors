#Create masks.

#See TemplateCreation wiki (https://github.com/PennBBL/pncReproc2015Scripts/wiki/TemplateCreation) for documentation of how the priors were made.

#prior image (cortical grey matter only) for CT, Ravens/GMD masks.
prior002=/data/joy/BBL/studies/pnc/template/priors/prior_002_2mm.nii.gz

#prior image (subcortical grey matter) for Ravens/GMD mask.
prior004=/data/joy/BBL/studies/pnc/template/priors/prior_004_2mm.nii.gz

#This is where the output masks go                                                                                                                                             
outdir=/data/joy/BBL/projects/pncT1AcrossDisorder/masks

echo "outdir is $outdir"

echo ""

##CT mask
#Threshold the image and binarize it.
fslmaths $prior002 -thr .1 -bin ${outdir}/prior_002_2mm_bin.nii.gz

#Fill the holes in the mask.
ImageMath 3 ${outdir}/corticalGM_mask.nii.gz FillHoles ${outdir}/prior_002_2mm_bin.nii.gz 1.5


##Ravens and GMD mask
#Combine the cortical and subcortical priors.
fslmaths $prior002 -add $prior004 ${outdir}/prior_002_004_2mm_cortSubcort.nii.gz

#Threshold the image and binarize it.                                                                                                                                                                                                  
fslmaths ${outdir}/prior_002_004_2mm_cortSubcort.nii.gz -thr .1 -bin ${outdir}/prior_002_004_2mm_cortSubcort_bin.nii.gz

#Fill the holes in the mask.                                                                                                                                                                                                          
ImageMath 3 ${outdir}/corticalSubcortical_mask.nii.gz FillHoles ${outdir}/prior_002_004_2mm_cortSubcort_bin.nii.gz 1.5
