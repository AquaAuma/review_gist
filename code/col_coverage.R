################################################################################
#### Assessment of COL coverage per group
#### Coding and data processing: Aurore Maureaud & Emily L. Sandall
#### October 2022
################################################################################

rm(list = ls())

# set date
date <- 'OCT2022'

# libraries
library(ggplot2)
library(tidyverse)
library(readxl)
library(egg)
library(grid)
library(png)
library(writexl)
library(rredlist)
library(readr)

# load data
taxonomies <- read.csv("data/taxonomies_OCT2022.csv")


################################################################################
#### 1. MATCH FUNCTION
################################################################################
match_col <- function(taxonomies, col, group, families) {
    
  ## subset both datasets for the taxa in focus
  mol_m <- taxonomies[taxonomies$group==group,] %>% 
    dplyr::select(canonical, family, group) %>% 
    distinct() # to remove the duplicates from different sources
  
  ## a. Match with all names
  match_tot <- full_join(mol_m, col, by = c("canonical" = "canonical_col"),keep = TRUE) %>% 
    filter(!is.na(canonical))
  
  ## b. No match
  no_match <- anti_join(col, mol_m, by = c("canonical_col" = "canonical"))
  
  ## c. Accepted names match
  mol_acc <- taxonomies[taxonomies$group==group,] %>% 
    filter(accid == 0)
  match_acc <- left_join(mol_acc, col, by = c("canonical" = "canonical_col"),keep=TRUE)
  
  ## d. Synonyms match
  mol_syn <- taxonomies[taxonomies$group==group,] %>% 
    filter(accid != 0)
  match_syn <- left_join(mol_syn, col, by = c("canonical" = "canonical_col"),keep=TRUE) %>% 
    filter(!is.na(canonical_col)) %>% 
    mutate(col_syn = "COL") %>% 
    dplyr::select(accid, col_syn) %>% 
    distinct()
  
  ## e. Total accepted species match
  match_acc_syn <- left_join(match_acc, match_syn,
                             by = c("id" = "accid")) %>% 
    mutate(col = ifelse(is.na(col), col_syn, col))
  
  ## f. Summary
  prop_no_match <- round(nrow(no_match)/nrow(col)*100,2)
  nbr_spp_col <- length(unique(col$canonical_col))
  nbr_spp_mol <- length(unique(mol_acc$canonical))
  prop_tot <- round(nrow(match_tot[!is.na(match_tot$col),])/nrow(match_tot)*100,2)
  prop_acc <- round(nrow(match_acc[!is.na(match_acc$col),])/nrow(match_acc)*100,2)
  prop_acc_syn <- round(nrow(match_acc_syn[!is.na(match_acc_syn$col),])/nrow(match_acc_syn)*100,2)
  group <- group
  return(data.frame(cbind(group, nbr_spp_col, nbr_spp_mol, prop_tot, prop_acc, 
                          prop_acc_syn, prop_no_match)))
}


################################################################################
#### 2. APPLY TO GROUPS
################################################################################

### A. Dragonflies #############################################################
col_dragonflies <- read_delim("data/COL/COL_taxonomy/OdonataTaxonomy_COL2021.tsv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE) %>% 
  filter(`col:rank` == "species",
         `col:status` == "accepted") %>% 
  mutate(canonical_col = paste(`col:genericName`,`col:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()

match_dragonflies <- match_col(taxonomies = taxonomies,
                               col = col_dragonflies,
                               group = "dragonflies")


### B. Mammals #################################################################
col_mammals <- read_delim("data/COL/COL_taxonomy/COL_Mammalia_DWC_122021/NameUsage.tsv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE) %>% 
  filter(`col:rank` == "species",
         `col:status` == "accepted") %>% 
  mutate(canonical_col = paste(`col:genericName`,`col:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()
  
match_mammals <- match_col(taxonomies = taxonomies,
                           col = col_mammals,
                           group = "mammals")


### C. Crabs ###################################################################
crab_families <- taxonomies %>%
  filter(group == "crabs") %>%
  dplyr::select(family) %>%
  filter(!is.na(family)) %>%
  distinct() %>% pull()

col_crabs <- read_delim("E:/Yale data/COL/COL_taxonomy/COL_Brachyura_122021/NameUsage.tsv",
                        delim = "\t", escape_double = FALSE,
                        trim_ws = TRUE) %>%
  filter(`col:rank` == "species",
         `col:status` == "accepted") %>%
  mutate(canonical_col = paste(`col:genericName`,`col:specificEpithet`, sep = " "),
         col = "COL") %>%
  select(canonical_col, col) %>%
  distinct()
# col_anomura <- read_delim("E:/Yale data/COL/COL_taxonomy/COL_Anomura_122021/NameUsage.tsv",
#                             delim = "\t", escape_double = FALSE,
#                             trim_ws = TRUE) %>%
#   filter(`col:rank` == "species",
#          `col:status` == "accepted") %>%
#   mutate(canonical_col = paste(`col:genericName`,`col:specificEpithet`, sep = " "),
#          col = "COL") %>%
#   select(canonical_col, col) %>%
#   distinct()


match_crabs <- match_col(taxonomies = taxonomies,
                         col = col_crabs,
                         group = "crabs")


### D. Reptiles ################################################################
# reptiles are a paraphyletic group, so they are not searchable by that term.
# the higher classification term (Sauropsida) is also not searchable.
# groups were searched by sub-groups: Eusuchia, Sphenodon, Squamata, Testuidines
eusuchia <- read_delim("data/COL/COL_taxonomy/COL_Eusuchia_DwC0922/Taxon.tsv",
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE) %>% 
  filter(`dwc:taxonRank` == "species",
         `dwc:taxonomicStatus` == "accepted") %>% 
  mutate(canonical_col = paste(`dwc:genericName`,`dwc:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()

squamata <- read_delim("data/COL/COL_taxonomy/COL_Squamata_DwC0922/Taxon.tsv",
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE) %>% 
  filter(`dwc:taxonRank` == "species",
         `dwc:taxonomicStatus` == "accepted") %>% 
  mutate(canonical_col = paste(`dwc:genericName`,`dwc:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()

sphenodon <- read_delim("data/COL/COL_taxonomy/COL_Sphenodon_DwC0922 2/Taxon.tsv",
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE) %>% 
  filter(`dwc:taxonRank` == "species",
         `dwc:taxonomicStatus` == "accepted") %>% 
  mutate(canonical_col = paste(`dwc:genericName`,`dwc:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()

testudines <- read_delim("data/COL/COL_taxonomy/COL_Testudines_DwC0922/Taxon.tsv",
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE) %>% 
  filter(`dwc:taxonRank` == "species",
         `dwc:taxonomicStatus` == "accepted") %>% 
  mutate(canonical_col = paste(`dwc:genericName`,`dwc:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()

col_rept <- rbind(squamata, sphenodon, testudines, eusuchia)
match_rept <- match_col(taxonomies = taxonomies,
                               col = col_rept,
                               group = "reptiles")


### E.Ants #####################################################################
col_ants <- read_delim("data/COL/COL_taxonomy/COL_Formicidae_122021/NameUsage.tsv", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE) %>% 
  filter(`col:rank` == "species",
         `col:status` == "accepted") %>% 
  mutate(canonical_col = paste(`col:genericName`,`col:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()
  
match_ants <- match_col(taxonomies = taxonomies,
                        col = col_ants,
                        group = "ants")


### F. Butterflies #############################################################
fam1 <- read_delim("data/COL/COL_taxonomy/COL_Riodinidae_122021/NameUsage.tsv", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE) %>% 
  filter(`col:rank` == "species",
         `col:status` == "accepted") %>% 
  mutate(canonical_col = paste(`col:genericName`,`col:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()
fam2 <- read_delim("data/COL/COL_taxonomy/COL_Pieridae_122021/NameUsage.tsv", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE) %>% 
  filter(`col:rank` == "species",
         `col:status` == "accepted") %>% 
  mutate(canonical_col = paste(`col:genericName`,`col:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()
fam3 <- read_delim("data/COL/COL_taxonomy/COL_Papilionidae_122021/NameUsage.tsv", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE) %>% 
  filter(`col:rank` == "species",
         `col:status` == "accepted") %>% 
  mutate(canonical_col = paste(`col:genericName`,`col:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()
fam4 <- read_delim("data/COL/COL_taxonomy/COL_Nymphalidae_122021/NameUsage.tsv", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE) %>% 
  filter(`col:rank` == "species",
         `col:status` == "accepted") %>% 
  mutate(canonical_col = paste(`col:genericName`,`col:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()
fam5 <- read_delim("data/COL/COL_taxonomy/COL_Lycaenidae_122021/NameUsage.tsv", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE) %>% 
  filter(`col:rank` == "species",
         `col:status` == "accepted") %>% 
  mutate(canonical_col = paste(`col:genericName`,`col:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()
fam6 <- read_delim("data/COL/COL_taxonomy/COL_Hesperiidae_122021/NameUsage.tsv", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE) %>% 
  filter(`col:rank` == "species",
         `col:status` == "accepted") %>% 
  mutate(canonical_col = paste(`col:genericName`,`col:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()
fam7 <- read_delim("data/COL/COL_taxonomy/COL_Hedylidae_122021/NameUsage.tsv", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE) %>% 
  filter(`col:rank` == "species",
         `col:status` == "accepted") %>% 
  mutate(canonical_col = paste(`col:genericName`,`col:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()

col_butt <- rbind(fam1, fam2, fam3, fam4, fam5, fam6, fam7)

match_butterflies <- match_col(taxonomies = taxonomies,
                               col = col_butt,
                               group = "butterflies")


### G. Birds ###################################################################
col_birds <- read_csv("data/COL/COL_taxonomy/AvesTaxonomy_COL2021.csv") %>% 
  filter(`dwc:taxonRank` == "species",
         `dwc:taxonomicStatus` == "accepted") %>% 
  mutate(canonical_col = paste(`dwc:genericName`,`dwc:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()
  
match_birds <- match_col(taxonomies = taxonomies,
                         col = col_birds,
                         group = "birds")


### H. Amphibians ##############################################################
col_amphi <- read_delim("data/COL/COL_taxonomy/COL_Amphibia_DWC_122021/NameUsage.tsv", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE) %>% 
  filter(`col:rank` == "species",
         `col:status` == "accepted") %>% 
  mutate(canonical_col = paste(`col:genericName`,`col:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()

match_amphi <- match_col(taxonomies = taxonomies,
                         col = col_amphi,
                         group = "amphibians")


### I. Compositae ##############################################################
col_compo <- read_delim("data/COL/COL_taxonomy/COL_ Asteraceae_DWC_0422022/Taxon.tsv",
                        delim = "\t", escape_double = FALSE,
                        trim_ws = TRUE) %>% 
  filter(`dwc:taxonRank` == "species",
         `dwc:taxonomicStatus` == "accepted") %>% 
  mutate(canonical_col = paste(`dwc:genericName`,`dwc:specificEpithet`, sep = " "),
         col = "COL") %>% 
  select(canonical_col, col) %>% 
  distinct()

match_compo <- match_col(taxonomies = taxonomies,
                         col = col_compo,
                         group = "daisies")


################################################################################
#### 3. SUMMARIZE INFORMATION
################################################################################

match_col_results <- rbind(match_dragonflies,
                           match_mammals,
                           match_crabs,
                           match_rept,
                           match_ants,
                           match_butterflies,
                           match_birds,
                           match_amphi,
                           match_compo)

write.csv(match_col_results, 
          file = paste0("results/match_col_results_",date,".csv"),
          row.names = F)

