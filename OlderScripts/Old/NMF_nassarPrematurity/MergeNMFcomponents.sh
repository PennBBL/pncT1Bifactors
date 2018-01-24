#Merge the 18 NMF components into a single image for plotting

inputImages=/data/joy/BBL/projects/pncT1AcrossDisorder/images/NMF_nassarPrematurity/NMF_sge_job_output/MaskedByNeighborThresh400_WarpedToMNI/*.nii.gz

outdir=/data/joy/BBL/projects/pncT1AcrossDisorder/images/NMF_nassarPrematurity/NMF_sge_job_output
 
fslmerge -t $outdir/n279_18NmfComponentsMerged.nii.gz $inputImages
