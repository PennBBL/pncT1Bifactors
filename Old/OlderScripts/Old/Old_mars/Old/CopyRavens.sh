#Copy the missing Raven's data from monstrum to chead.

cat /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/Test.csv | while IFS="," read -r a b ;
do

destination="/data/joy/BBL/studies/pnc/processedData/structural/ravens/${a}/*x${b}/";

path1=`ls -d /import/monstrum/eons_xnat/subjects/${a}_${b}/*_mprage/sbia/*_150.nii.gz`;
path2=`ls -d /import/monstrum/eons_xnat/subjects/${a}_${b}/*_mprage/sbia/*_150_2mm.nii.gz`;

Name1=$(basename "$path1");
Name2=$(basename "$path2");

echo "desination is $destination";
echo "path1 is $path1";
echo "path2 is $path2";
echo "filename1 is $Name1";
echo "filename2 is $Name2";

#mkdir "$destination"
#scp "$path1" "akaczkurkin@chead.uphs.upenn.edu:$destination/$Name1" 
#cp "$path1" "$destination"/"$Name";

done
