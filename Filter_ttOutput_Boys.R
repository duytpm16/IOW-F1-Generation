#################################################################################################################
#  This script creates a dataframe of CpGs, coefficient, p-value, MapInfo, and UCSC gene name for each of 
#       the ttScreening output for each pubertal marker.
#
#  Input:
#    1.)   Illumina Methylation EPIC manifest file
#    2-5.) ttScreening output for the five pubertal markers in boys 
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
library(xlsx)



### Load in the data
epic <- read.csv("~/Desktop/MethylationEPIC_v-1-0_B2 YJ.csv")
bodyhair <- readRDS("~/Desktop/tt_AGEBODYHAIRMALE_18.rds")
deepvoice <- readRDS("~/Desktop/tt_AGEDEEPENINGVOICEMALE_18.rds")
facialhair <- readRDS("~/Desktop/tt_AGEFACIALHAIRMALE_18.rds")
growthspurt <- readRDS("~/Desktop/tt_AGEGROWTHSPURTMALE_18.rds")
skinchanges <- readRDS("~/Desktop/tt_AGESKINCHANGESMALE_18.rds")




### Filter ttScreening output for body hair in boys
#
#       132 x 4
boys_bodyhair <- cbind(bodyhair$TT.cpg,
                        bodyhair$TT.output %>% select(`Coeff AGEBODYHAIRMALE_18`, `Pvalue AGEBODYHAIRMALE_18`)) %>%
                    plyr::rename(c('bodyhair$TT.cpg' = 'CpGs_BodyHair',
                                   'Pvalue AGEBODYHAIRMALE_18' = 'p_value',
                                   'Coeff AGEBODYHAIRMALE_18' = 'coefficient')) %>%
                    dplyr::arrange(CpGs_BodyHair) %>%
                    merge(epic[,c('Name','CHR','MAPINFO')], by.x = 'CpGs_BodyHair', by.y = 'Name', all.x = TRUE) %>%
                    unite(MapInfo, CHR, MAPINFO, sep = ':')




### Filter ttScreening output for deepening voice in boys
#
#       142 x 4
boys_deepvoice <- cbind(deepvoice$TT.cpg,
                           deepvoice$TT.output %>% select(`Coeff AGEDEEPENINGVOICEMALE_18`, `Pvalue AGEDEEPENINGVOICEMALE_18`)) %>%
                        plyr::rename(c('deepvoice$TT.cpg' = 'CpGs_DeepVoice',
                                       'Pvalue AGEDEEPENINGVOICEMALE_18' = 'p_value',
                                       'Coeff AGEDEEPENINGVOICEMALE_18' = 'coefficient')) %>%
                        dplyr::arrange(CpGs_DeepVoice) %>%
                        merge(epic[,c('Name','CHR','MAPINFO')], by.x = 'CpGs_DeepVoice', by.y = 'Name', all.x = TRUE) %>%
                        unite(MapInfo, CHR, MAPINFO, sep = ':')




### Filter ttScreening output for facial hair in boys
#
#      128 x 4
boys_facialhair <- cbind(facialhair$TT.cpg,
                         facialhair$TT.output %>% select(`Coeff AGEFACIALHAIRMALE_18`, `Pvalue AGEFACIALHAIRMALE_18`)) %>%
                      plyr::rename(c('facialhair$TT.cpg' = 'CpGs_Period',
                                     'Pvalue AGEFACIALHAIRMALE_18' = 'p_value',
                                     'Coeff AGEFACIALHAIRMALE_18' = 'coefficient')) %>%
                      dplyr::arrange(CpGs_Period) %>%
                      merge(epic[,c('Name','CHR','MAPINFO')], by.x = 'CpGs_Period', by.y = 'Name', all.x = TRUE) %>%
                      unite(MapInfo, CHR, MAPINFO, sep = ':')




### Filter ttScreening output for growth spurt in boys
#
#     114 x 4
boys_growthspurt <- cbind(growthspurt$TT.cpg,
                           growthspurt$TT.output %>% select(`Coeff AGEGROWTHSPURTMALE_18`, `Pvalue AGEGROWTHSPURTMALE_18`)) %>%
                        plyr::rename(c('growthspurt$TT.cpg' = 'CpGs_GrowthSpurt',
                                       'Pvalue AGEGROWTHSPURTMALE_18' = 'p_value',
                                       'Coeff AGEGROWTHSPURTMALE_18' = 'coefficient')) %>%
                        dplyr::arrange(CpGs_GrowthSpurt) %>%
                        merge(epic[,c('Name','CHR','MAPINFO')], by.x = 'CpGs_GrowthSpurt', by.y = 'Name', all.x = TRUE) %>%
                        unite(MapInfo, CHR, MAPINFO, sep = ':')




### Filter ttScreening output for skin changes in boys
#
#     44 x 4
boys_skinchanges <- cbind(skinchanges$TT.cpg,
                           skinchanges$TT.output %>% select(`Coeff AGESKINCHANGESMALE_18`, `Pvalue AGESKINCHANGESMALE_18`)) %>%
                      plyr::rename(c('skinchanges$TT.cpg' = 'CpGs_SkinChanges',
                                     'Pvalue AGESKINCHANGESMALE_18' = 'p_value',
                                     'Coeff AGESKINCHANGESMALE_18' = 'coefficient')) %>%
                      dplyr::arrange(CpGs_SkinChanges) %>%
                      merge(epic[,c('Name','CHR','MAPINFO')], by.x = 'CpGs_SkinChanges', by.y = 'Name', all.x = TRUE) %>%
                      unite(MapInfo, CHR, MAPINFO, sep = ':')



### Save data
save(boys_bodyhair,boys_deepvoice,boys_facialhair,boys_growthspurt,boys_skinchanges, file = 'ttOut_Autosome_Boys.RData')
