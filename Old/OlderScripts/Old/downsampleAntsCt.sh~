#this is where the output images go
outdir=/data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/images/antsCtDownsampled/

echo "outdir is $outdir"

echo ""

#this is your list of input image paths
#example path /data/joy/BBL/studies/pnc/processedData/structural/antsCorticalThickness/100031/20100918x3818/CorticalThicknessNormalizedToTemplate.nii.gz                         
imgList=$(cat /data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/subjectData/n1360_antsCt1mmImages.txt)

for i in $imgList; do 
	echo ""

	echo $i
	
#get scanid: first echo the path (echo $i), then cut the path up by delimiter "x" (-dx), then take the second field (-f2) which is everything after "x", then remove the text after the "/" and just keep the first field (which will be the scanid number).

	scanid=$(echo $i | cut -dx -f2  | cut -d'/' -f1)
	echo "scanid is $scanid"

#downsample the data and save the output file
	fslmaths $i -subsamp2 $outdir/${scanid}_CorticalThicknessNormalizedToTemplate2mm
done

