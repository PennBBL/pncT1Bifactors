#Remove small clusters using the function "returnNumberOfNeighboorhoods"

numComponents="1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18"

indir=/data/joy/BBL/projects/pncT1AcrossDisorder/images/NMF_nassarPrematurity/NMF_sge_job_output/Threshold004Bin

outdir=/data/joy/BBL/projects/pncT1AcrossDisorder/images/NMF_nassarPrematurity/NMF_sge_job_output/Threshold004Bin/NeighborThresh200

for i in $numComponents
do
        echo ""

        echo "Component number is $i"

matlab -nodisplay -nojvm -r "returnNumberOfNeighboorhoods('${indir}/Basis_${i}_thr004Bin.nii.gz', 200); exit;"

done

