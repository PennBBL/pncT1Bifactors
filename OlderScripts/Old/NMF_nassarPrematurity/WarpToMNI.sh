#Convert the NMF components to MNI space for Caret images.

numComponents="1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18"

Directory=/data/joy/BBL/projects/pncT1AcrossDisorder/images/NMF_nassarPrematurity

for i in $numComponents
do
        echo ""

        echo "Component number is $i"

antsApplyTransforms -e 3 -d 3 -r /share/apps/fsl/5.0.8/data/standard/MNI152_T1_1mm_brain.nii.gz -o $Directory/NMF_$i.nii -i $Directory/Basis_$i.nii -t /data/joy/BBL/studies/pnc/template/pnc2mni0Warp.nii.gz -t /data/joy/BBL/studies/pnc/template/pnc2mni1GenericAffine.mat

done
