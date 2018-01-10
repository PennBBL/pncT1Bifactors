#Get the Ravens volume filenames for the nassarPrematurity sample (n=279) using the smoothed Ravens files.

cat /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n279_nassarPrematurity_bblids_scanids.csv | while IFS="," read -r a b ;

do 

path=`ls -d /data/joy/BBL/projects/pncT1AcrossDisorder/images/Ravens/Ravens_smoothed8mm_masked/${a}_${b}_RAVENS_2GM_2mm_smoothed8mm_masked.nii.gz`;

#get just the file name without the path: first echo the path (echo $i), then cut the path up by delimiter "/" (-d'/'), then take the 10th field (-f10), which is the file name.

        fileName=$(echo $path | cut -d'/' -f10 )
        echo "File name is $fileName"


echo $fileName >> /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n279_nassarPrematurity_FileNames_smoothed8mm_masked.csv

done

