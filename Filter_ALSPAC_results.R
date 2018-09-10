source("https://bioconductor.org/biocLite.R")
biocLite("IlluminaHumanMethylationEPICanno.ilm10b2.hg19")


options(stringsAsFactors = FALSE)
library(tidyverse)
library(IlluminaHumanMethylationEPICanno.ilm10b2.hg19)

data(IlluminaHumanMethylationEPICanno.ilm10b2.hg19)



### Read in ALSPEC data
menarche_alspac <- read.csv("Pheno_menarche_ALSPAC_results.csv")[,-c(1,2)]
testes_alspac <- read.csv("tanner_Pheno_testes_multilevel_ALSPAC_results.csv")[,-c(1,2)]


### Get CpGs from Alspec menarche data
query_cpgs <- menarche_alspac$CpG

#  Get UCSC  annotation of the CpGs
anno <- IlluminaHumanMethylationEPICanno.ilm10b2.hg19 %>% 
            getAnnotation %>% 
            as.data.frame %>% 
            dplyr::slice(match(query_cpgs, Name))

#  Merge menarche_alspec and anno together. Then filter
menarche_alspac <- merge(menarche_alspac, anno[,c('Name','chr','pos','UCSC_RefGene_Name')], by.x = 'CpG', by.y = 'Name', sort = FALSE, all.x = TRUE) %>%
                        filter(p < 0.05) %>%
                        mutate(chr = gsub('chr','', chr),
                               MAPINFO = paste0(chr,':',pos)) %>%
                        separate_rows(UCSC_RefGene_Name, sep = ";") %>%
                        distinct() %>%
                        group_by(CpG, Coeff, p) %>%
                        mutate(GeneName = paste0(UCSC_RefGene_Name, collapse = ',')) %>%
                        select(-UCSC_RefGene_Name) %>%
                        unique()
  

write.csv(menarche_alspac, 'menarche_alspac_v2.csv', row.names = FALSE)





### Get CpGs from Alspec tanner pheno data
query_cpgs <- testes_alspac$CpG

anno <- IlluminaHumanMethylationEPICanno.ilm10b2.hg19 %>% 
            getAnnotation %>% 
            as.data.frame %>% 
            dplyr::slice(match(query_cpgs, Name))

#  Merge menarche_alspec and anno together. Then filter
testes_alspac <- merge(testes_alspac, anno[,c('Name','chr','pos','UCSC_RefGene_Name')], by.x = 'CpG', by.y = 'Name', sort = FALSE, all.x = TRUE) %>%
                            filter(p < 0.05) %>%
                            mutate(chr = gsub('chr','', chr),
                                   MAPINFO = paste0(chr,':',pos)) %>%
                            separate_rows(UCSC_RefGene_Name, sep = ";") %>%
                            distinct() %>%
                            group_by(CpG, Coeff, p) %>%
                            mutate(GeneName = paste0(UCSC_RefGene_Name, collapse = ',')) %>%
                            select(-UCSC_RefGene_Name) %>%
                            unique()

write.csv(testes_alspac, 'testes_pheno_alspac_v2.csv', row.names = FALSE)
