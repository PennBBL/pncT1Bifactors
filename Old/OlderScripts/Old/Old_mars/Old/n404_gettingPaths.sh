cat n404_bblids.csv | while read line
do

path=`ls -d /data/joy/BBL/studies/pnc/processedData/pcasl/asl_201604/${line}/*/norm/*_asl_quant_stdT1_std.nii.gz`;

t1=`echo $path | cut -d " " -f1`;
t2=`echo $path | cut -d " " -f2`;

if [ $t1 == $t2 ] 
then
t2='';
fi

echo $line,go1,$t1 >> n404_asl_chead_path.csv
echo $line,go2,$t2 >> n404_asl_chead_path.csv

done
