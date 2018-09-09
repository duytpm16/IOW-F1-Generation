#####################################################################################################################
#  This script uses the iow_v36a data to select the height, fvc, and birthwt columns.
#     Height difference and fvc lung changes are computed and stored as individual columns.
#     Finally the data is saved by males and females.
#
#  Input:
#     1.) celltype data: used to intersect subjects
#     2.) iow_v36a.sas7bdat file
#
#  Output:
#     1.) subset iow data by males
#     2.) subset iow data be females
#
#  Author: Duy Pham
#  E-mail: dtpham@memphis.edu
####################################################################################################################
# Load libraries
options(stringsAsFactors = FALSE)

library(haven)
library(dplyr)


### Load in data
#
#     celltype: 796 x 8
#     iow:  1536 x 2243
cellType <- read.csv("~/F1_guthrie850k_cell_24march2018.csv")
iow <- read_sas("~/iow_v36a.sas7bdat")v


### Find overlaping subjects
overlap.subjects <- intersect(iow$STUDYid, cellType$studyid)


### Filtering iow data
iow_sub <- iow %>% 
              select(STUDYid,SEX_18,HEIGHTCM_10, HEIGHTCM_18, FVC_10, FVC_18, BIRTHWT) %>%   # Select required columns
              filter(STUDYid %in% overlap.subjects) %>%                                      # Subset by overlapping subjects
              mutate(STUDYid = paste0('X_',STUDYid),                                         # Make study ids = 'X_study id #'
                     HEIGHTCM_DIFF = HEIGHTCM_18 - HEIGHTCM_10,                              # Add height difference to data
                     FVC_CHANGE = FVC_18 - FVC_10) %>%                                       # Add FVC change to data
              rename(study_id = STUDYid) %>%                                                 # Renaming study id column
              as.data.frame() %>%                                                            # Make data as data frame
              `rownames<-`(.[,which(colnames(.) == 'study_id')])                             # Make rownames study id


### Subset iow by females
iow_female <- iow_sub %>%
                  filter(SEX_18 == 2)

### Subset iow by males
iow_male <- iow_sub %>% filter(SEX_18 == 1)


### Save the data as .RData file
save(iow_female, file = 'iow_v36_filtered_girls_395.RData')
save(iow_male, file = 'iow_v36_filtered_boys_401.RData')
