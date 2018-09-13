####################################################################################################################
# This script annotates the significant CpGs identified by ALSPAC
#
# Input:
#    1.) Illumina EPIC manifest file v1
#    2.) Alspac significant CpGs for menarche
#    3.) Alspac significant CpGs for testicular volume
#
# Output:
#    1.) Filtered and annotated CpGs in Input (2)
#    2.) Filtered and annotated CpGs in Input (3)
#
# Author: Duy Pham
# E-mail: dtpham@memphis.edu
####################################################################################################################
options(stringsAsFactors = FALSE)
library(tidyverse)



### Read in Illumina EPIC manifest file
epic <- read.csv("~/MethylationEPIC_v-1-0_B2 YJ.csv")

### Read in ALSPEC data
#
#    menarche_alspac: 525 x 5 -> 525 x 3
#    testes_alspac:   302 x 5 -> 302 x 3
menarche_alspac <- read.csv("Pheno_menarche_ALSPAC_results.csv")[,-c(1,2)]
testes_alspac <- read.csv("tanner_Pheno_testes_multilevel_ALSPAC_results.csv")[,-c(1,2)]



### Get CpGs from Alspec menarche result
query_cpgs <- menarche_alspac$CpG



#  Merge menarche_alspec and epic together. Then filter
#
#     menarche_alspac: 31 x 6
menarche_alspac <- merge(menarche_alspac, epic[,c('Name','CHR','MAPINFO','UCSC_RefGene_Name')], by.x = 'CpG', by.y = 'Name', sort = FALSE, all.x = TRUE) %>%
                        filter(p < 0.05) %>%
                        mutate(MAPINFO = paste0('chr',CHR,':',MAPINFO)) %>%
                        separate_rows(UCSC_RefGene_Name, sep = ";") %>%
                        distinct() %>%
                        group_by(CpG, Coeff, p) %>%
                        mutate(GeneName = paste0(UCSC_RefGene_Name, collapse = ',')) %>%
                        select(-UCSC_RefGene_Name, -CHR) %>%
                        unique()
  

write.csv(menarche_alspac, 'menarche_alspac_v2.csv', row.names = FALSE)




#  Merge testes_alspac and epic together. Then filter
#
#     testes_alspac: 127 x 6
testes_alspac <- merge(testes_alspac, epic[,c('Name','CHR','MAPINFO','UCSC_RefGene_Name')], by.x = 'CpG', by.y = 'Name', sort = FALSE, all.x = TRUE) %>%
                            filter(p < 0.05) %>%
                            mutate(MAPINFO = paste0('chr',CHR,':',MAPINFO)) %>%
                            separate_rows(UCSC_RefGene_Name, sep = ";") %>%
                            distinct() %>%
                            group_by(CpG, Coeff, p) %>%
                            mutate(GeneName = paste0(UCSC_RefGene_Name, collapse = ',')) %>%
                            select(-UCSC_RefGene_Name, -CHR) %>%
                            unique()

write.csv(testes_alspac, 'testes_pheno_alspac_v2.csv', row.names = FALSE)
