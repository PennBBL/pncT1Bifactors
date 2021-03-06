#FDR correct results from GAM wrapper

outputList=$(cat /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/results/n1385_RavensPath_ACROSS.INCLUDE_smooth3.4/n1385gam_Cov_sage_sex_White_meduCnbGo1_mprageMassICV_averageRating_goassessItemBifactor4FactorMood_goassessItemBifactor4FactorPsych_goassessItemBifactor4FactorExt_goassessItemBifactor4FactorPhb_goassessItemBifactor4FactorOverallPsy/filenames.txt)

for i in $outputList; do
    echo "";
    echo "filename is $i";
    var=$(basename "$i" .nii.gz);
    echo "variable is $var";
    fdr -i $i -m /data/joy/BBL/templates/pncRavensTemplate/mniGreyCombEons2mmEro.nii.gz -q 0.05 > fdr_${var}
    FDRTHRESH=`awk 'NR==2' fdr_${var}`
    echo "FDR threshold is" $FDRTHRESH
    FDRTHRESH=`awk -v a=$FDRTHRESH -v b="1" 'BEGIN{print (b - a)}'`
    echo "1 minus FDR threshold is" $FDRTHRESH
    fslmaths $i -mul -1 -add 1 -thr $FDRTHRESH -mas /data/joy/BBL/templates/pncRavensTemplate/mniGreyCombEons2mmEro.nii.gz thresh_fdr_1minusP_${i}
done
