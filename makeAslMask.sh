bblids=$(cat /import/monstrum/Users/antoniak/PNC_asl/voxelwise_analyses/n875_cbf.paths.ants/bblids.txt)  #this is list of bblids
outdir=/import/monstrum/Users/antoniak/PNC_asl/masks
outfile=$outdir/n875_maskList.txt
logfile=$outdir/n875_maskMissing.txt

rm -f $outfile #cleanup mask list at beginning-- we may not want to regenerate this list each time. . . .

#assemble list of masks
for b in $bblids; do
	echo ""
	echo $b
	image=$(ls /import/monstrum/eons_xnat/subjects/${b}*/*_ep2d_se_pcasl_PHC_1200ms/*quant*/reg_std_ants/mask_for_reg_check.nii.gz)
	if [ ! -e "$image" ]; then 
		echo "mask missing!!!"
		echo $b >> $logfile
	else
		echo $image >> $outfile
	fi
done

#merge masks

echo "now merging masks"
fslmerge -t $outdir/n875_masksCombined.nii.gz $(cat $outfile)


#binarize sum across masks-- this is hard coded 'cus I am lazy
echo "now taking temporal mean"
fslmaths $outdir/n875_masksCombined.nii.gz -Tmin $outdir/n875_mask  #binary-- requires everyone have coverage

fslmaths $outdir/n875_masksCombined.nii.gz -Tmean $outdir/n875_mask_mean  #ranges from 0-1 in terms of mean coverage

#now go look for coverage hope it is good
	
