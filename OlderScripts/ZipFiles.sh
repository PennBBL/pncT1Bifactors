##Zip the files

#CT
cd /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/voxelwiseMaps_antsCT/
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/antsCT/n1375_antsCt_data.tar.gz -T /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1375_antsCt_FileNames.csv
cd /data/joy/BBL/projects/pncT1AcrossDisorder/images/antsCT/antsCT_0.85sigma
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/antsCT/n1375_antsCt_data_smoothed2mm.tar.gz *
cd /data/joy/BBL/projects/pncT1AcrossDisorder/images/antsCT/antsCT_1.70sigma
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/antsCT/n1375_antsCt_data_smoothed4mm.tar.gz *
cd /data/joy/BBL/projects/pncT1AcrossDisorder/images/antsCT/antsCT_2.55sigma
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/antsCT/n1375_antsCt_data_smoothed6mm.tar.gz *
cd /data/joy/BBL/projects/pncT1AcrossDisorder/images/antsCT/antsCT_3.40sigma
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/antsCT/n1375_antsCt_data_smoothed8mm.tar.gz *

#GMD
cd /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/voxelwiseMaps_gmd/
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/GMD/n1375_GMD_data.tar.gz -T /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1375_GMD_FileNames.csv
cd /data/joy/BBL/projects/pncT1AcrossDisorder/images/GMD/GMD_0.85sigma
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/GMD/n1375_GMD_data_smoothed2mm.tar.gz *
cd /data/joy/BBL/projects/pncT1AcrossDisorder/images/GMD/GMD_1.70sigma
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/GMD/n1375_GMD_data_smoothed4mm.tar.gz *
cd /data/joy/BBL/projects/pncT1AcrossDisorder/images/GMD/GMD_2.55sigma
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/GMD/n1375_GMD_data_smoothed6mm.tar.gz *
cd /data/joy/BBL/projects/pncT1AcrossDisorder/images/GMD/GMD_3.40sigma
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/GMD/n1375_GMD_data_smoothed8mm.tar.gz *

#Ravens
cd /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/voxelwiseMaps_ravens/
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/Ravens/n1375_Ravens_data.tar.gz -T /data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1375_Ravens_FileNames.csv
cd /data/joy/BBL/projects/pncT1AcrossDisorder/images/Ravens/Ravens_0.85sigma
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/Ravens/n1375_Ravens_data_smoothed2mm.tar.gz *
cd /data/joy/BBL/projects/pncT1AcrossDisorder/images/Ravens/Ravens_1.70sigma
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/Ravens/n1375_Ravens_data_smoothed4mm.tar.gz *
cd /data/joy/BBL/projects/pncT1AcrossDisorder/images/Ravens/Ravens_2.55sigma
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/Ravens/n1375_Ravens_data_smoothed6mm.tar.gz *
cd /data/joy/BBL/projects/pncT1AcrossDisorder/images/Ravens/Ravens_3.40sigma
tar -cvf /data/joy/BBL/projects/pncT1AcrossDisorder/images/Ravens/n1375_Ravens_data_smoothed8mm.tar.gz *
