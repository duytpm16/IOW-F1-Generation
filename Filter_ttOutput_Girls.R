#################################################################################################################
#  This script creates a dataframe of CpGs, coefficient, p-value, MapInfo, and UCSC gene name for each of 
#       the ttScreening output for each pubertal marker.
#
#  Input:
#    1.)   Illumina Methylation EPIC manifest file
#    2-5.) ttScreening output for the five pubertal markers in girls 
#
#  Output:
#    1.) .RData file of the dataframes generated in Input 2-5
#
#  Author: Duy Pham
#  E-mail: dtpham@memphis.edu
#################################################################################################################

options(stringsAsFactors = FALSE)

library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(xlsx)



### Load in the data
epic <- read.csv("C:/Users/DUY/Desktop/F1_850k data/Original Data/MethylationEPIC_v-1-0_B2 YJ.csv")
bodyhair <- readRDS("C:/Users/DUY/Desktop/F1_850k Data/ttOutput/test_alpha_0.1/Girls/tt_AGEGROWTHBODYHAIRFEMALE_18.rds")
breastgrowth <- readRDS("C:/Users/DUY/Desktop/F1_850k Data/ttOutput/test_alpha_0.1/Girls/tt_AGEBREASTGROWTHFEMALE_18.rds")
growthspurt <- readRDS("C:/Users/DUY/Desktop/F1_850k Data/ttOutput/test_alpha_0.1/Girls/tt_AGEGROWTHSPURTFEMALE_18.rds")
period <- readRDS("C:/Users/DUY/Desktop/F1_850k Data/ttOutput/test_alpha_0.1/Girls/tt_AGEPERIODFEMALE_18.rds")
skinchanges <- readRDS("C:/Users/DUY/Desktop/F1_850k Data/ttOutput/test_alpha_0.1/Girls/tt_AGESKINCHANGESFEMALE_18.rds")




### Filter ttScreening output for body hair in girls
#
#     233 x 5
girls_bodyhair <- cbind(bodyhair$TT.cpg,
                         bodyhair$TT.output %>% select(`Coeff AGEGROWTHBODYHAIRFEMALE_18`, `Pvalue AGEGROWTHBODYHAIRFEMALE_18`)) %>%
                      plyr::rename(c('bodyhair$TT.cpg' = 'CpGs_BodyHair',
                                     'Pvalue AGEGROWTHBODYHAIRFEMALE_18' = 'p_value',
                                     'Coeff AGEGROWTHBODYHAIRFEMALE_18' = 'coefficient')) %>%
                      dplyr::arrange(CpGs_BodyHair) %>%
                      merge(epic[,c('Name','CHR','MAPINFO','UCSC_RefGene_Name')], by.x = 'CpGs_BodyHair', by.y = 'Name', all.x = TRUE) %>%
                      unite(MapInfo, CHR, MAPINFO, sep = ':') %>%
                      separate_rows(UCSC_RefGene_Name, sep = ";") %>%
                      distinct() %>%
                      group_by(CpGs_BodyHair, coefficient, p_value) %>%
                      mutate(UCSC_RefGene_Name = paste0(UCSC_RefGene_Name, collapse = ',')) %>%
                      unique() %>%
                      mutate(Gene  = gsub(';',',',UCSC_RefGene_Name)) %>%
                      mutate(MapInfo = paste0('chr',MapInfo)) %>%
                      select(-UCSC_RefGene_Name)



### Filter ttScreening output for breast growth in girls
#
#     240 x 5
girls_breastgrowth <- cbind(breastgrowth$TT.cpg,
                            breastgrowth$TT.output %>% select(`Coeff AGEBREASTGROWTHFEMALE_18`, `Pvalue AGEBREASTGROWTHFEMALE_18`)) %>%
                        plyr::rename(c('breastgrowth$TT.cpg' = 'CpGs_BreastGrowth',
                                       'Pvalue AGEBREASTGROWTHFEMALE_18' = 'p_value',
                                       'Coeff AGEBREASTGROWTHFEMALE_18' = 'coefficient')) %>%
                        dplyr::arrange(CpGs_BreastGrowth) %>%
                        merge(epic[,c('Name','CHR','MAPINFO','UCSC_RefGene_Name')], by.x = 'CpGs_BreastGrowth', by.y = 'Name', all.x = TRUE) %>%
                        unite(MapInfo, CHR, MAPINFO, sep = ':') %>%
                        separate_rows(UCSC_RefGene_Name, sep = ";") %>%
                        distinct() %>%
                        group_by(CpGs_BreastGrowth, coefficient, p_value) %>%
                        mutate(UCSC_RefGene_Name = paste0(UCSC_RefGene_Name, collapse = ',')) %>%
                        unique() %>%
                        mutate(Gene  = gsub(';',',',UCSC_RefGene_Name)) %>%
                        mutate(MapInfo = paste0('chr',MapInfo)) %>%
                        select(-UCSC_RefGene_Name)
  


### Filter ttScreening output for growth spurt in girls
#
#     178 x 5
girls_growthspurt <- cbind(growthspurt$TT.cpg,
                           growthspurt$TT.output %>% select(`Coeff AGEGROWTHSPURTFEMALE_18`, `Pvalue AGEGROWTHSPURTFEMALE_18`)) %>%
                        plyr::rename(c('growthspurt$TT.cpg' = 'CpGs_GrowthSpurt',
                                       'Pvalue AGEGROWTHSPURTFEMALE_18' = 'p_value',
                                       'Coeff AGEGROWTHSPURTFEMALE_18' = 'coefficient')) %>%
                        dplyr::arrange(CpGs_GrowthSpurt) %>%
                        merge(epic[,c('Name','CHR','MAPINFO','UCSC_RefGene_Name')], by.x = 'CpGs_GrowthSpurt', by.y = 'Name', all.x = TRUE) %>%
                        unite(MapInfo, CHR, MAPINFO, sep = ':') %>%
                        separate_rows(UCSC_RefGene_Name, sep = ";") %>%
                        distinct() %>%
                        group_by(CpGs_GrowthSpurt, coefficient, p_value) %>%
                        mutate(UCSC_RefGene_Name = paste0(UCSC_RefGene_Name, collapse = ',')) %>%
                        unique() %>%
                        mutate(Gene  = gsub(';',',',UCSC_RefGene_Name)) %>%
                        mutate(MapInfo = paste0('chr',MapInfo)) %>%
                        select(-UCSC_RefGene_Name)
  


### Filter ttScreening output for period in girls
#
#     223 x 5
girls_period <- cbind(period$TT.cpg,
                       period$TT.output %>% select(`Coeff AGEPERIODFEMALE_18`, `Pvalue AGEPERIODFEMALE_18`)) %>%
                    plyr::rename(c('period$TT.cpg' = 'CpGs_Period',
                                   'Pvalue AGEPERIODFEMALE_18' = 'p_value',
                                   'Coeff AGEPERIODFEMALE_18' = 'coefficient')) %>%
                    dplyr::arrange(CpGs_Period) %>%
                    merge(epic[,c('Name','CHR','MAPINFO','UCSC_RefGene_Name')], by.x = 'CpGs_Period', by.y = 'Name', all.x = TRUE) %>%
                    unite(MapInfo, CHR, MAPINFO, sep = ':') %>%
                    separate_rows(UCSC_RefGene_Name, sep = ";") %>%
                    distinct() %>%
                    group_by(CpGs_Period, coefficient, p_value) %>%
                    mutate(UCSC_RefGene_Name = paste0(UCSC_RefGene_Name, collapse = ',')) %>%
                    unique() %>%
                    mutate(Gene  = gsub(';',',',UCSC_RefGene_Name)) %>%
                    mutate(MapInfo = paste0('chr',MapInfo)) %>%
                    select(-UCSC_RefGene_Name)



### Filter ttScreening output for skin changes in girls
#
#     119 x 5
girls_skinchanges <- cbind(skinchanges$TT.cpg,
                            skinchanges$TT.output %>% select(`Coeff AGESKINCHANGESFEMALE_18`, `Pvalue AGESKINCHANGESFEMALE_18`)) %>%
                        plyr::rename(c('skinchanges$TT.cpg' = 'CpGs_SkinChanges',
                                       'Pvalue AGESKINCHANGESFEMALE_18' = 'p_value',
                                       'Coeff AGESKINCHANGESFEMALE_18' = 'coefficient')) %>%
                        dplyr::arrange(CpGs_SkinChanges) %>%
                        merge(epic[,c('Name','CHR','MAPINFO','UCSC_RefGene_Name')], by.x = 'CpGs_SkinChanges', by.y = 'Name', all.x = TRUE) %>%
                        unite(MapInfo, CHR, MAPINFO, sep = ':') %>%
                        separate_rows(UCSC_RefGene_Name, sep = ";") %>%
                        distinct() %>%
                        group_by(CpGs_SkinChanges, coefficient, p_value) %>%
                        mutate(UCSC_RefGene_Name = paste0(UCSC_RefGene_Name, collapse = ',')) %>%
                        unique() %>%
                        mutate(Gene  = gsub(';',',',UCSC_RefGene_Name)) %>%
                        mutate(MapInfo = paste0('chr',MapInfo)) %>%
                        select(-UCSC_RefGene_Name)
                                          




save(girls_bodyhair,girls_breastgrowth,girls_period,girls_growthspurt,girls_skinchanges, file = 'ttOut_Autosome_Girls_850k_395.RData')
