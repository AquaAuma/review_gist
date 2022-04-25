################################################################################
#### Assessment of NCBI coverage per group
#### Coding and data processing: Aurore Maureaud & Emily Sandall
#### April 2022
################################################################################

rm(list = ls())

# set date
date <- '25APR2022'

# libraries
library(ggplot2)
library(tidyverse)
library(readxl)
library(rphylopic)
library(egg)
library(grid)
library(png)
library(writexl)
library(rredlist)

# load data
taxonomies <- read.csv("data/taxonomies_25APR2022.csv")


################################################################################
#### 1. MATCH FUNCTION
################################################################################
match_ncbi <- function(taxonomies, ncbi_raw, group){
  
  ## a. Canonical from NCBI without match as the species level
  ncbi_no_match <- ncbi_raw %>% 
    filter(str_detect(Canonical, pattern = " sp.") |
             str_detect(Canonical, pattern = "unclassified") |
             str_detect(Canonical, pattern = " aff. ") |
             str_detect(Canonical, pattern = " cf. ") |
             str_detect(Canonical, pattern = " sample") |
             str_detect(Canonical, pattern = " nr.") |
             str_detect(Canonical, pattern = " ssp.") |
             !str_detect(Canonical, pattern = " "))
  
  ## b. Match with all names
  ncbi <- ncbi_raw %>% 
    filter(!str_detect(Canonical, pattern = " sp."),
           !str_detect(Canonical, pattern = "unclassified"),
           !str_detect(Canonical, pattern = " aff. "),
           !str_detect(Canonical, pattern = " cf. "),
           !str_detect(Canonical, pattern = " sample"),
           !str_detect(Canonical, pattern = " nr."),
           !str_detect(Canonical, pattern = " ssp."),
           str_detect(Canonical, pattern = " "))
  
  mol_m <- taxonomies[taxonomies$group==group,] %>% 
    dplyr::select(canonical, group) %>% 
    distinct() # to remove the duplicates from different sources
  
  match_tot <- full_join(mol_m, ncbi, by = c("canonical" = "Canonical"),keep = TRUE) %>% 
    filter(!is.na(canonical)) %>% 
    mutate(ncbi = ifelse(!is.na(Canonical),"yes",NA_character_))
  
  ## c. No match
  no_match <- anti_join(ncbi, mol_m, by = c("Canonical" = "canonical"))
  
  ## d. Accepted names match
  mol_acc <- taxonomies[taxonomies$group==group,] %>% 
    filter(accid == 0)
  match_acc <- left_join(mol_acc, ncbi, by = c("canonical" = "Canonical"),keep=TRUE) %>% 
    mutate(ncbi = ifelse(!is.na(Canonical),"yes",NA_character_))
  
  ## e. Synonyms match
  mol_syn <- taxonomies[taxonomies$group==group,] %>% 
    filter(accid != 0)
  match_syn <- left_join(mol_syn, ncbi, by = c("canonical" = "Canonical"),keep=TRUE) %>% 
    filter(!is.na(Canonical))  %>% 
    mutate(ncbi_syn = "yes") %>% 
    dplyr::select(accid, ncbi_syn) %>% 
    distinct()
  
  ## f. Total accepted species match
  match_acc_syn <- left_join(match_acc, match_syn,
                             by = c("id" = "accid")) %>% 
    mutate(ncbi = ifelse(is.na(ncbi), ncbi_syn, ncbi))
  
  ## g. Summary
  prop_no_possible_match <- round(nrow(ncbi_no_match)/nrow(ncbi_raw)*100,2)
  prop_no_match <- round(nrow(no_match)/nrow(ncbi_raw)*100,2)
  prop_tot <- round(nrow(match_tot[!is.na(match_tot$ncbi),])/nrow(match_tot)*100,2)
  prop_acc <- round(nrow(match_acc[!is.na(match_acc$ncbi),])/nrow(match_acc)*100,2)
  prop_acc_syn <- round(nrow(match_acc_syn[!is.na(match_acc_syn$ncbi),])/nrow(match_acc_syn)*100,2)
  group <- group
  return(data.frame(cbind(group, prop_tot, prop_acc, prop_acc_syn,
                          prop_no_match, prop_no_possible_match)))
  
}

################################################################################
#### 2. APPLY TO GROUPS
################################################################################

### A. dragonflies #############################################################
ncbi_dragonflies <- read_csv(file = "E:/Yale data/NCBI_assessment/Perspective_NCBI_taxonomy/Odonata_NCBItaxonomy_result1121.csv")

match_dragonflies <- match_ncbi(taxonomies = taxonomies,
                                ncbi = ncbi_dragonflies,
                                group = "dragonflies")


### B. mammals #################################################################
ncbi_mammals <- read_csv(file = "E:/Yale data/NCBI_assessment/Perspective_NCBI_taxonomy/Mammalia_NCBItaxonomy_result1121.csv")

match_mammals <- match_ncbi(taxonomies = taxonomies,
                                ncbi = ncbi_mammals,
                                group = "mammals")


### C. Crabs ###################################################################
ncbi_crabs <- read_csv(file = "E:/Yale data/NCBI_assessment/Perspective_NCBI_taxonomy/Crabs_NCBItaxonomy_result1121.csv")

match_crabs <- match_ncbi(taxonomies = taxonomies,
                          ncbi = ncbi_crabs,
                          group = "crabs")


### D. Reptiles ################################################################
ncbi_reptiles <- read_csv(file = "E:/Yale data/NCBI_assessment/Perspective_NCBI_taxonomy/Reptiles_NCBItaxonomy_result1121.csv")

match_reptiles <- match_ncbi(taxonomies = taxonomies,
                             ncbi = ncbi_reptiles,
                             group = "reptiles")


### E.Ants #####################################################################
ncbi_ants <- read_csv(file = "E:/Yale data/NCBI_assessment/Perspective_NCBI_taxonomy/Formicidae_NCBItaxonomy_result1121.csv")
match_ants <- match_ncbi(taxonomies = taxonomies,
                         ncbi = ncbi_ants,
                         group = "ants")


### F. Butterflies #############################################################
ncbi_butt <- read_csv(file = "E:/Yale data/NCBI_assessment/Perspective_NCBI_taxonomy/Butterflies_NCBItaxonomy_result1121.csv")

match_butt <- match_ncbi(taxonomies = taxonomies,
                         ncbi = ncbi_butt,
                         group = "butterflies")


### G. Birds ###################################################################
ncbi_birds <- read.delim("E:/Yale data/NCBI_assessment/Perspective_NCBI_taxonomy/Aves_taxonomy_result1121.txt")

match_birds <- match_ncbi(taxonomies = taxonomies,
                         ncbi = ncbi_birds,
                         group = "birds")


### H. Amphibians ##############################################################
ncbi_amphi <- read.delim("E:/Yale data/NCBI_assessment/Perspective_NCBI_taxonomy/Amphibia_NCBItaxonomy_result1121.csv")

match_amphi <- match_ncbi(taxonomies = taxonomies,
                          ncbi = ncbi_amphi,
                          group = "amphibians")


################################################################################
#### 3. SUMMARIZE INFORMATION
################################################################################

match_ncbi_results <- rbind(match_dragonflies,
                            match_mammals,
                            match_crabs,
                            match_reptiles,
                            match_ants,
                            match_butt,
                            match_birds,
                            match_amphi)

write.csv(match_ncbi_results, 
          file = paste0("results/match_ncbi_results_",date,".csv"),
          row.names = F)







