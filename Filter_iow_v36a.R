options(stringsAsFactors = FALSE)

library(haven)
library(dplyr)



cellType <- read.csv("~/F1_guthrie850k_cell_24march2018.csv")
iow <- read_sas("~/iow_v36a.sas7bdat")

overlap.subjects <- intersect(iow$STUDYid, cellType$studyid)


iow_sub <- iow %>% 
              select(STUDYid,SEX_18,HEIGHTCM_10, HEIGHTCM_18, FVC_10, FVC_18, BIRTHWT) %>%
              filter(STUDYid %in% overlap.subjects) %>%
              mutate(STUDYid = paste0('X_',STUDYid),
                     HEIGHTCM_DIFF = HEIGHTCM_18 - HEIGHTCM_10,
                     FVC_CHANGE = FVC_18 - FVC_10) %>%
              rename(study_id = STUDYid) %>%
              as.data.frame() %>%
              `rownames<-`(.[,which(colnames(.) == 'study_id')])


iow_female <- iow_sub %>%
                  filter(SEX_18 == 2)

iow_male <- iow_sub %>% filter(SEX_18 == 1)



save(iow_female, file = 'iow_v36_filtered_girls_395.RData')
save(iow_male, file = 'iow_v36_filtered_boys_401.RData')
