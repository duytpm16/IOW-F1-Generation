#################################################################################################################################
#   This script summarizes each puberty event for boys and girls
#
#   Input:
#      1.) puberty event data
#      2.) cell type data
#
#   Output:
#      1.) .xlsx file containing entire puberty data (1), subset by gender (2), summaries for (2)
#               and 2 more summaries for overlapping subjects in puberty event and cell type data by gender
#
#   Author: Duy Pham
#   E-mail: dtpham@memphis.edu
####################################################################################################################################
options(stringsAsFactors = FALSE)
library(dplyr)
library(tidyr)
library(xlsx)



### Load in the puberty event data
pubertyonset <- read.csv('puberty_data_F1_generation.csv')
celltype <- read.csv('~/F1_guthrie850k_cell_24march2018')



### Subsetting pubertyonset to contain same sample ids as in celltype
#
#       pubertyonset_filtered dimensions: 796 x 12
overlap_subjects <- intersect(pubertyonset$STUDYid, celltype$studyid)
pubertyonset_filtered <- pubertyonset %>% 
                              filter(STUDYid %in% overlap_samples)



### Subset and filter pubertyonset into boys and girls
#
#       girls: 395 x 7
#       boys:  401 x 7
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
                                  


### Summarizing the data

### Summary of each puberty event for girls without filtering
girl_pubertyOnset_summary <- pubertyonset %>% 
                                    filter(SEX_18 == 'Female') %>%
                                    select(AGEGROWTHBODYHAIRFEMALE_18,AGEBREASTGROWTHFEMALE_18, 
                                           AGEGROWTHSPURTFEMALE_18, AGEPERIODFEMALE_18,
                                           AGESKINCHANGESFEMALE_18) %>%
                                    gather(.) %>%                                     # Turn wide dataframe to long
                                    filter(complete.cases(.)) %>%                     # Remove NAs (for summary functions)
                                    group_by(key) %>%                                 # Group by each puberty event
                                    summarise_all(funs(n(), mean, sd, min,max)) %>%   # Summary of each puberty event
                                    mutate(mean = sprintf("%0.2f", mean),             # Round mean and sd column to 2 decimal
                                           sd = sprintf("%0.2f", sd)) %>% as.data.frame()

### Summary of each puberty event for girls after filtering                
girl_filteredPubertyOnset_summary <- girl_pubertyOnset %>%
                                          select(-study_id,-sex) %>%
                                          gather(.) %>%                                     # Turn wide dataframe to long
                                          filter(complete.cases(.)) %>%                     # Remove NAs (for summary functions)
                                          group_by(key) %>%                                 # Group by each puberty event
                                          summarise_all(funs(n(), mean, sd, min,max)) %>%   # Summary of each puberty event
                                          mutate(mean = sprintf("%0.2f", mean),             # Round mean and sd column to 2 decimal
                                                 sd = sprintf("%0.2f", sd)) %>% as.data.frame()





### Summary of each puberty events for boys without filter
boy_pubertyOnset_summary <- pubertyonset %>%
                                    filter(SEX_18 == 'Male') %>%
                                    select(AGEBODYHAIRMALE_18, AGEDEEPENINGVOICEMALE_18, 
                                           AGEFACIALHAIRMALE_18, AGEGROWTHSPURTMALE_18,
                                           AGESKINCHANGESMALE_18) %>%
                                    gather(.) %>%                                           # Turn wide dataframe to long
                                    filter(complete.cases(.)) %>%                           # Remove NAs (for summary functions)
                                    group_by(key) %>%                                       # Group by each puberty event
                                    summarise_all(funs(n(), mean, sd, min,max)) %>%         # Summary of each puberty event
                                    mutate(mean = sprintf("%0.2f", mean),                   # Round mean and sd column to 2 decimal
                                           sd = sprintf("%0.2f", sd)) %>% as.data.frame()

### Summary of each puberty events for boys after filtering
boy_filteredPubertyOnset_summary <- boy_pubertyOnset %>%
                                            select(-study_id,-sex) %>%
                                            gather(.) %>%                                     # Turn wide dataframe to long
                                            filter(complete.cases(.)) %>%                     # Remove NAs (for summary functions)
                                            group_by(key) %>%                                 # Group by each puberty event
                                            summarise_all(funs(n(), mean, sd, min,max)) %>%   # Summary of each puberty event
                                            mutate(mean = sprintf("%0.2f", mean),             # Round mean and sd column to 2 decimal
                                                   sd = sprintf("%0.2f", sd)) %>% as.data.frame()



### Save data to .xlsx file
write.xlsx(pubertyonset, file = 'Summary_Pubert_Events.xlsx', sheetName = 'PubertyEvents', row.names = FALSE)
write.xlsx(girl_pubertyOnset, file = 'Summary_Pubert_Events.xlsx', sheetName = 'Girl_PubertyEvents', row.names = TRUE, append = TRUE)
write.xlsx(boy_pubertyOnset, file = 'Summary_Pubert_Events.xlsx', sheetName = 'Boy_PubertyEvents', row.names = TRUE, append = TRUE)
write.xlsx(girl_pubertyOnset_summary, file = 'Summary_Pubert_Events.xlsx', sheetName = 'All_Girls', row.names = FALSE, append = TRUE)
write.xlsx(girl_filteredPubertyOnset_summary, file = 'Summary_Pubert_Events.xlsx', sheetName = 'Filtered_Girls', row.names = FALSE, append = TRUE)
write.xlsx(boy_pubertyOnset_summary, file = 'Summary_Pubert_Events.xlsx', sheetName = 'All_Boys', row.names = FALSE, append = TRUE)
write.xlsx(boy_filteredPubertyOnset_summary, file = 'Summary_Pubert_Events.xlsx', sheetName = 'Filtered_Boys', row.names = FALSE, append = TRUE)
