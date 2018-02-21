#Create lists of those missing a Ravens nii.gz file on chead.

#!/bin/bash

cat /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n279_nassarPrematurity_scanids.csv | while IFS="," read -r a ;
do

if [ ! -f /data/joy/BBL/studies/pnc/n1601_dataFreeze2016/neuroimaging/t1struct/voxelwiseMaps_ravens/${a}_RAVENS_2.nii.gz ]; then

    echo $a >> /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n279_MissingRavens.csv
   
fi

done

