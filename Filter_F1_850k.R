####################################################################################################################
#  This script is used to subset the puberty event, cell type, and DNAm data into males and females
#         and making sure the study id are the same across the 3 data 
#
#   Input:
#       1.) .RData containing the puberty event, cell type, and DNAm data
#
#   Output:
#       1.) .RData containing the puberty event, cell type, and DNAm data for female subjects
#       2.) .RData containing the puberty evetn, cell type, and DNAM data for male subjects
#
#   Author: Duy Pham
#   E-mail: dtpham@memphis.edu
#################################################################################################################
options(stringsAsFactors = FALSE)
library(dplyr)
library(tidyr)



### Read in the original 850k data which contains the cell types, age at puberty onset, and DNAm data
load("~/Original_F1_850k_Data.RData")


### Find overlapping study ids (subjects) in pubertyonset and celltype dataframes. 
#     Then subset data to contain the same subjects.
#         Overlapping study ids:  796
#         Original puberty onset to filtered: 1536 x 12 -> 796 x 12
#         Original celltype to filtered:      796 x 8 -> 796 x 8
overlap_samples <- intersect(pubertonset$STUDYid, celltype$studyid)

pubertyonset_filtered <- pubertyonset %>% 
                            filter(STUDYid %in% overlap_samples)

celltype_filtered <- celltype %>% 
                          filter(studyid %in% overlap_samples)




### Using the puberty onset filtered data to subset by males and females: 796 x 12
#
#      After filtering for girls: 395 x 7
#      After filtering for boys:  401 x 7
girl_pubertyOnset <- pubertyonset_filtered %>% 
                          filter(SEX_18 == 'Female') %>%                              # Filter by female subjects
                          mutate(STUDYid = paste0('X_',STUDYid)) %>%                  # Make subject ids = 'X_id#'
                          select(STUDYid, SEX_18, AGEGROWTHBODYHAIRFEMALE_18,         # Select columns containing puberty events for females
                                 AGEBREASTGROWTHFEMALE_18,AGEGROWTHSPURTFEMALE_18, 
                                 AGEPERIODFEMALE_18, AGESKINCHANGESFEMALE_18) %>%
                          rename(study_id = STUDYid, sex = SEX_18) %>%                # Renaming study id and sex column
                          `rownames<-`(.[,which(colnames(.) == 'study_id')])          # Make rownames the same as the study id column

boy_pubertyOnset <- pubertyonset_filtered %>% 
                          filter(SEX_18 == 'Male') %>%                                # Filter by male subjects
                          mutate(STUDYid = paste0('X_',STUDYid)) %>%                  # Make subject ids = 'X_id#'
                          select(STUDYid, SEX_18, AGEBODYHAIRMALE_18,                 # Select columns containing puberty events for males
                                 AGEDEEPENINGVOICEMALE_18, AGEFACIALHAIRMALE_18,
                                 AGEGROWTHSPURTMALE_18, AGESKINCHANGESMALE_18)  %>%
                          rename(study_id = STUDYid, sex = SEX_18) %>%                # Renaming study id and sex column
                          `rownames<-`(.[,which(colnames(.) == 'study_id')])          # Make rownames the same as the study id column





### Using the filtered celltype data to subset by males and females: 796 x 8
#
#      After filtering for girls: 395 x 8
#      After filtering for boys:  401 x 8
girl_cellType <- celltype_filtered %>% 
                      mutate(studyid = paste0('X_',studyid)) %>%                        # Make subject ids = 'X_id#'
                      filter(studyid %in% rownames(girl_pubertyOnset)) %>%              # Filter female subjects by matching study id in the girl_pubertyOnset data
                      rename(study_id = studyid) %>%                                    # Renaming study id column
                      select(study_id, CD8T, CD4T, NK, Bcell, Mono, Neu, Eos) %>%       # Reording columns
                      `rownames<-`(.[,which(colnames(.) == 'study_id')])                # Make rownames the same as the study id column
                          
boy_cellType <- celltype_filtered %>% 
                      mutate(studyid = paste0('X_',studyid)) %>%                        # Make subject ids = 'X_id#'
                      filter(studyid %in% rownames(boy_pubertyOnset)) %>%               # Filter female subjects by matching study id in the girl_pubertyOnset data
                      rename(study_id = studyid) %>%                                    # Renaming study id column
                      select(study_id, CD8T, CD4T, NK, Bcell, Mono, Neu, Eos) %>%       # Reording columns
                      `rownames<-`(.[,which(colnames(.) == 'study_id')])                # Make rownames the same as the study id column




### Filtering the DNAm data: 551710 x 796
#
#       After filtering for girls: 551710 x 395
#       After filtering for boys:  551710 x 401
girl_DNAm <- dnam %>% 
                select(colnames(.)[colnames(.) %in% girl_pubertyOnset$study_id])


boy_DNAm <- dnam %>% 
                select(colnames(.)[colnames(.) %in% boy_pubertyOnset$study_id])



### Save the data for boys and girls
save(boy_cellType, boy_DNAm, boy_pubertyOnset, file = 'Boys_F1_850k_Filtered_401.RData')
save(girl_cellType, girl_DNAm, girl_pubertyOnset, file = 'Girls_F1_850k_Filtered_395.RData')




