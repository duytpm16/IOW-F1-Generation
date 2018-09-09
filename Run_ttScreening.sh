#PBS -l nodes=1:ppn=12
#PBS -q batch

#################################################################################################################
#  Shell script to run Run_ttScreening_HPC.R
#
#  Input:
#     1.) Path to .RData containing puberty onset, DNAm, cell type data for one gender
#     2.) Which gender: 'boy' or 'girl'
#     3.) Name of puberty event column to run ttScreening on
#
#  Output:
#     1.) .rds file containing the ttScreening output.
#	    Name of file will be 'tt_name of puberty event column.rds'
################################################################################################################

cd /home/dtpham/F1_850k
module load R/3.5.0


### Boys
#Rscript Run_ttScreening_HPC.R ~/F1_850k/Boys_F1_850k_Filtered_401.RData boy AGEBODYHAIRMALE_18
#Rscript Run_ttScreening_HPC.R ~/F1_850k/Boys_F1_850k_Filtered_401.RData boy AGEDEEPENINGVOICEMALE_18
#Rscript Run_ttScreening_HPC.R ~/F1_850k/Boys_F1_850k_Filtered_401.RData boy AGEFACIALHAIRMALE_18
#Rscript Run_ttScreening_HPC.R ~/F1_850k/Boys_F1_850k_Filtered_401.RData boy AGEGROWTHSPURTMALE_18
#Rscript Run_ttScreening_HPC.R ~/F1_850k/Boys_F1_850k_Filtered_401.RData boy AGESKINCHANGESMALE_18


### Girls
#Rscript Run_ttScreening_HPC.R ~/F1_850k/Girls_F1_850k_Filtered_395.RData girl AGEGROWTHBODYHAIRFEMALE_18
#Rscript Run_ttScreening_HPC.R ~/F1_850k/Girls_F1_850k_Filtered_395.RData girl AGEBREASTGROWTHFEMALE_18
#Rscript Run_ttScreening_HPC.R ~/F1_850k/Girls_F1_850k_Filtered_395.RData girl AGEGROWTHSPURTFEMALE_18
#Rscript Run_ttScreening_HPC.R ~/F1_850k/Girls_F1_850k_Filtered_395.RData girl AGEPERIODFEMALE_18
#Rscript Run_ttScreening_HPC.R ~/F1_850k/Girls_F1_850k_Filtered_395.RData girl AGESKINCHANGESFEMALE_18
