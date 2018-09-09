####################################################################################################################################
#  This script runs the ttScreening function on the HPC given the required input data below
#
#  Input:
#     1.) Path to .RData file containing puberty onset, cell type, and DNAm data
#     2.) Which gender: 'boy' or 'girl'
#     3.) Name of puberty event column to run ttScreening on
#
#  Output:
#     1.) .rds file containing the ttScreening output.
#           Name of file will be 'tt_name of puberty event.rds'
#
#  Author: Duy Pham
#  E-mail: dtpham@memphis.edu
####################################################################################################################################

### Load libraries
options(stringsAsFactors = FALSE)
library(ttScreening)
library(dplyr)


### Receive command line inputs
args = commandArgs(trailingOnly=TRUE)


### Variables to change
#     1.) Path to .RData containing puberty onset, DNAm, cell type data for one gender
#     2.) Which gender: 'boy' or 'girl'
#     3.) Name of puberty event column to run ttScreening on
load(as.character(args[1]))
gender = args[2]
pubertyEvent = args[3]




### Prepare data based on gender
if(gender == 'boy'){
   DNAm <- log2(boy_DNAm/(1-boy_DNAm))  # Logit-transformation of DNAm data
   rm(boy_DNAm)                         # Removing original DNAm 
   DNAm <- data.matrix(t(DNAm))         # Transposing DNAm data for ttScreening: subjects x CpG
   
   
   # Get puberty event
   pubertyEvent_df <- boy_pubertyOnset %>% select(one_of(pubertyEvent)) %>% as.data.frame()  
   
   
   # Combine pubtery event with celltype data, excluding study id and CD8T
   tempData <- cbind(pubertyEvent_df,boy_cellType %>% select(-study_id, -CD8T))    
}

if(gender == 'girl'){
   DNAm <- log2(girl_DNAm/(1-girl_DNAm))  # Logit-transformation
   rm(girl_DNAm)                          # Removing original DNAm 
   DNAm <- data.matrix(t(DNAm))           # Transposing DNAm data for ttScreening: subjects x CpG
   
   
   # Get puberty event
   pubertyEvent_df <- girl_pubertyOnset %>% select(one_of(pubertyEvent)) %>% as.data.frame()  
  
   
   # Combine pubtery event with celltype data, excluding study id and CD8T
   tempData <- cbind(pubertyEvent_df,girl_cellType %>% select(-study_id, -CD8T)) 
}



### Make formula for ttScreening
n <- make.names(names(tempData))
form <- as.formula(paste("~ ", paste(n, collapse = "+")))



### Run ttScreening
ttOut <- ttScreening(y = DNAm, 
                     formula = form,
                     data = tempData, 
                     imp.var=1,
                     sva.method="two-step",
                     train.alpha=0.05, 
                     test.alpha=0.1,
                     linear= "ls",
                     rowname=TRUE)



### Save ttOut as .rds data
saveRDS(ttOut, file = paste0('tt_',pubertyEvent,'.rds'))
