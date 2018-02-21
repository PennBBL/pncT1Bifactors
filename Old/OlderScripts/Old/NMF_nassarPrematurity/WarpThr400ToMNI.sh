#Convert the NMF components to MNI space for Caret images.

numComponents="1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18"

indir=/data/joy/BBL/projects/pncT1AcrossDisorder/images/NMF_nassarPrematurity/NMF_sge_job_output/MaskedByNeighborThresh400

outdir=/data/joy/BBL/projects/pncT1AcrossDisorder/images/NMF_nassarPrematurity/NMF_sge_job_output/MaskedByNeighborThresh400_WarpedToMNI

for i in $numComponents
do
        echo ""

        echo "Component number is $i"

antsApplyTransforms -e 3 -d 3 -r /share/apps/fsl/5.0.8/data/standard/MNI152_T1_2mm_brain.nii.gz -o $outdir/Basis_${i}_thr004_maskedByNeighborThr400_2mmMNI.nii.gz -i $indir/Basis_${i}_thr004_maskedByNeighborThr400.nii.gz -t /data/joy/BBL/studies/pnc/template/pnc2mni0Warp.nii.gz -t /data/joy/BBL/studies/pnc/template/pnc2mni1GenericAffine.mat

done

