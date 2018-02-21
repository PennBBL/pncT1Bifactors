#Create a cortical, subcortical, and cerebellum mask to mask the Ravens and GMD masks to remove brainstem.

#See TemplateCreation wiki (https://github.com/PennBBL/pncReproc2015Scripts/wiki/TemplateCreation) for documentation of how the priors were made.

#prior image (cortical grey matter only) 
prior002=/data/joy/BBL/studies/pnc/template/priors/prior_002_2mm.nii.gz

#prior image (subcortical grey matter)
prior004=/data/joy/BBL/studies/pnc/template/priors/prior_004_2mm.nii.gz

#prior image (cerebellum)
prior006=/data/joy/BBL/studies/pnc/template/priors/prior_006_2mm.nii.gz

#This is where the output masks go                                                                                                                                             
outdir=/data/joy/BBL/projects/pncT1AcrossDisorder/masks

echo "outdir is $outdir"

echo ""

##CT mask
#Threshold the image and binarize it.
#NOTE: thresholds greater than .01 create holes in the mask that directly impact the creation of NMF components.
fslmaths $prior002 -thr .01 -bin ${outdir}/cortical_mask.nii.gz


##Ravens and GMD mask
#Combine the cortical and subcortical priors.
fslmaths $prior002 -add $prior004 ${outdir}/prior_002_004_2mm_cortSubcort.nii.gz

#Threshold and binarize.                                                                                                                                                                                                  
fslmaths ${outdir}/prior_002_004_2mm_cortSubcort.nii.gz -thr .01 -bin ${outdir}/corticalSubcortical_mask.nii.gz
