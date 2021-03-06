#Smooth the CT, GMD, and volume (Ravens) data before running NMF.
#We tried 2mm, 4mm, 6mm, and 8mm FWHM                                                                                                               

cat /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n10_bblids_scanids.csv | while IFS="," read -r a b ;

do

#Define paths
CtPath=`ls -d /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/voxelwiseMaps_antsCt/${b}_CorticalThicknessNormalizedToTemplate2mm.nii.gz`;

#GmdPath=`ls -d /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/voxelwiseMaps_gmd/${b}_atropos3class_prob02SubjToTemp2mm.nii.gz`;

#RavensPath=`ls -d /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/voxelwiseMaps_ravens/${b}_RAVENS_2GM_2mm.nii.gz`;

#Define output directories
CtOutdir=/data/joy/BBL/projects/pncT1AcrossDisorder/images/antsCT/TEST

#GmdOutdir=/data/joy/BBL/projects/pncT1AcrossDisorder/images/GMD

#RavensOutdir=/data/joy/BBL/projects/pncT1AcrossDisorder/images/Ravens

#Define file names
CtPathName=$(echo $CtPath|cut -d, -f1)
CtFullFileName=$(basename $CtPath)
CtFileName=$(basename $CtPath | cut -d. -f1)

#GmdPathName=$(echo $GmdPath|cut -d, -f1)
#GmdFullFileName=$(basename $GmdPath)
#GmdFileName=$(basename $GmdPath | cut -d. -f1)

#RavensPathName=$(echo $RavensPath|cut -d, -f1)
#RavensFullFileName=$(basename $RavensPath)
#RavensFileName=$(basename $RavensPath | cut -d. -f1)

#Save full filenames to text files for zipping the data later
#echo $CtFullFileName >> /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1375_antsCt_FileNames.csv
#echo $GmdFullFileName >> /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1375_GMD_FileNames.csv
#echo $RavensFullFileName >> /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1375_Ravens_FileNames.csv

#NOTE: fslmaths requires smoothing parameters in sigma
#FWHM = 2.355*sigma
#2mm FWHM = 0.85 sigma
#4mm FWHM = 1.70 sigma
#6mm FWHM = 2.55 sigma
#8mm FWHM = 3.40 sigma                   

sigma="0.85 1.70 2.55 3.40"

for i in $sigma
do

fslmaths $CtPathName -s $i $CtOutdir/antsCT_${i}sigma/${CtFileName}_smoothed${i}sigma.nii.gz

#fslmaths $GmdPathName -s $i $GmdOutdir/GMD_${i}sigma/${GmdFileName}_smoothed${i}sigma.nii.gz

#fslmaths $RavensPathName -s $i $RavensOutdir/Ravens_${i}sigma/${RavensFileName}_smoothed${i}sigma.nii.gz

done

done

