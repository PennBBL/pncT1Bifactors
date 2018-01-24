#Sym link the Ravens data into a temp folder and then copy to CBICA.                                                                                                                                                   
input_list=/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/RavensPaths.txt
 

for i in $(cat $input_list); do

        echo "--- image path ---"

        imagePath=/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/images/Smoothed_Masked_Ravens/$(echo $i|cut -d, -f1)
        echo $imagePath

        if [ ! -e "$imagePath" ]; then
                echo "--- IMAGE IS MISSING ---"

        elif [ -e "$imagePath" ]; then

                ln -sf $imagePath /data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/tmpRavensSymLink2

        fi

done

