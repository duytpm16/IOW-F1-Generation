### Summarizing each puberty event for boys and girls


options(stringsAsFactors = FALSE)
library(dplyr)
library(tidyr)


### Load in the puberty event data
pubertyonset <- read.csv('puberty_data_F1_generation.csv')



### Subset pubertyonset into boys and girls
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
                                  


### Summarizing the original pubertyOnset data (All the cohort) for girls
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
                                           sd = sprintf("%0.2f", sd))


### Summarize the filtered pubertyOnset data for girls                  
girl_filteredPubertyOnset_summary <- girl_pubertyOnset %>%
                                          select(-study_id,-sex) %>%
                                          gather(.) %>%                                     # Turn wide dataframe to long
                                          filter(complete.cases(.)) %>%                     # Remove NAs (for summary functions)
                                          group_by(key) %>%                                 # Group by each puberty event
                                          summarise_all(funs(n(), mean, sd, min,max)) %>%   # Summary of each puberty event
                                          mutate(mean = sprintf("%0.2f", mean),             # Round mean and sd column to 2 decimal
                                                 sd = sprintf("%0.2f", sd))





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
                                           sd = sprintf("%0.2f", sd))



### Summarize the filtered pubertyOnset data for boys 
boys_filteredPubertyOnset_summary <- boy_pubertyOnset %>%
                                            select(-study_id,-sex) %>%
                                            gather(.) %>%                                     # Turn wide dataframe to long
                                            filter(complete.cases(.)) %>%                     # Remove NAs (for summary functions)
                                            group_by(key) %>%                                 # Group by each puberty event
                                            summarise_all(funs(n(), mean, sd, min,max)) %>%   # Summary of each puberty event
                                            mutate(mean = sprintf("%0.2f", mean),             # Round mean and sd column to 2 decimal
                                                   sd = sprintf("%0.2f", sd))

