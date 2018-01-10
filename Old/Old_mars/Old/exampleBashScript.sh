###inputs and arguments
subjList=$(cat /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/testRavensPaths.txt)

for i in $subjList; do
	echo ""
	echo $i
	
	#append a random extension this
	filename=$(echo $i | cut -d. -f1)
	echo $filename
	fslmaths $i ${filename}_random
done

