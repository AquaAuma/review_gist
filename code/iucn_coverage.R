################################################################################
#### Assessment of IUCN coverage per group
#### Coding and data processing: Aurore Maureaud
#### November 2021
################################################################################

rm(list = ls())

# set date
date <- '15NOV2021'

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
taxonomies <- read.csv("data/taxonomies_11NOV2021.csv")


################################################################################
#### 1. MATCH FUNCTION
################################################################################

match_iucn <- function(taxonomies, tax, ass, group, families){
  
  if(group %in% c("crabs","butterflies")){
    iucn <- left_join(tax, ass, by = c("internalTaxonId","scientificName")) %>% 
      mutate(familyName = str_to_sentence(familyName)) %>%
      filter(familyName %in% families) %>% 
      dplyr::select(scientificName, redlistCategory, redlistCriteria)
  } else{
    iucn <- left_join(tax, ass, by = c("internalTaxonId","scientificName")) %>% 
      dplyr::select(scientificName, redlistCategory, redlistCriteria)
  }
  
  group <- group
  
  ## a. Match with all names
  mol_m <- taxonomies[taxonomies$group==group,] %>% 
    dplyr::select(canonical, group) %>% 
    distinct() # to remove the duplicates from different sources
  
  match_tot <- full_join(mol_m, iucn, by = c("canonical" = "scientificName"),keep = TRUE) %>% 
    filter(!is.na(canonical))
  
  ## b. Accepted names match
  mol_acc <- taxonomies[taxonomies$group==group,] %>% 
    filter(accid == 0)
  match_acc <- left_join(mol_acc, iucn, by = c("canonical" = "scientificName"))
  
  ## c. Synonyms match
  mol_syn <- taxonomies[taxonomies$group==group,] %>% 
    filter(accid != 0)
  match_syn <- left_join(mol_syn, iucn, by = c("canonical" = "scientificName")) %>% 
    filter(!is.na(redlistCategory)) %>% 
    dplyr::select(accid, redlistCategory) %>% 
    distinct() %>% 
    rename(redlistCategory_syn = redlistCategory)
  
  ## d. Total accepted species match
  match_acc_syn <- left_join(match_acc, match_syn,
                             by = c("id" = "accid")) %>% 
    mutate(redlistCategory_syn = ifelse(is.na(redlistCategory), redlistCategory_syn, redlistCategory))
  
  ## e. Summary
  prop_tot <- round(nrow(match_tot[!is.na(match_tot$redlistCategory),])/nrow(match_tot)*100,2)
  prop_acc <- round(nrow(match_acc[!is.na(match_acc$redlistCategory),])/nrow(match_acc)*100,2)
  prop_acc_syn <- round(nrow(match_acc_syn[!is.na(match_acc_syn$redlistCategory_syn),])/nrow(match_acc_syn)*100,2)
  prop_acc_syn_ass <- round(nrow(match_acc_syn[!is.na(match_acc_syn$redlistCategory_syn) & match_acc_syn$redlistCategory!="Data Deficient",])/nrow(match_acc_syn)*100,2)
  group <- group
  return(data.frame(cbind(group, prop_tot, prop_acc, prop_acc_syn, prop_acc_syn_ass)))
  
}


################################################################################
#### 2. APPLY TO GROUPS
################################################################################

### A. dragonflies #############################################################
tax_dragonflies <- read_csv("E:/Yale data/IUCN/redlist_species_data_e9cb6b0f-6979-4077-bc50-d8f32d305e87_DRAGONFLIES/taxonomy.csv")
ass_dragonflies <- read_csv("E:/Yale data/IUCN/redlist_species_data_e9cb6b0f-6979-4077-bc50-d8f32d305e87_DRAGONFLIES/assessments.csv") %>% 
  dplyr::select(internalTaxonId, scientificName, redlistCategory, redlistCriteria)

match_dragonflies <- match_iucn(taxonomies = taxonomies,
                                tax = tax_dragonflies, 
                                ass = ass_dragonflies,
                                group = "dragonflies")




### B. Mammals #################################################################
tax_mammals <- read_csv("E:/Yale data/IUCN/redlist_species_data_aa70253a-9caa-4c42-aa49-2d3a0f5ecbc3_MAMMALS/taxonomy.csv")
ass_mammals <- read_csv("E:/Yale data/IUCN/redlist_species_data_aa70253a-9caa-4c42-aa49-2d3a0f5ecbc3_MAMMALS/assessments.csv") %>% 
  dplyr::select(internalTaxonId, scientificName, redlistCategory, redlistCriteria)

match_mammals <- match_iucn(taxonomies = taxonomies,
                                tax = tax_mammals, 
                                ass = ass_mammals,
                                group = "mammals")
  
  
### C. Crabs ###################################################################
tax_decapods <- read_csv("E:/Yale data/IUCN/redlist_species_data_35ee6bb5-f5c4-431b-8d34-48c63dbc89d0_DECAPODS/taxonomy.csv")
ass_decapods <- read_csv("E:/Yale data/IUCN/redlist_species_data_35ee6bb5-f5c4-431b-8d34-48c63dbc89d0_DECAPODS/assessments.csv") %>% 
  dplyr::select(internalTaxonId, scientificName, redlistCategory, redlistCriteria)

crab_families <- taxonomies %>% 
  filter(group == "crabs") %>% 
  dplyr::select(family) %>% 
  filter(!is.na(family)) %>% 
  distinct() %>% pull()

match_crabs <- match_iucn(taxonomies = taxonomies,
                          tax = tax_decapods, 
                          ass = ass_decapods,
                          group = "crabs",
                          families = crab_families)


### D. Reptiles ################################################################
tax_rept <- read_csv("E:/Yale data/IUCN/redlist_species_data_e1397a22-f30b-4a9c-80f0-e1f66cf875a2_REPTILES/taxonomy.csv")
ass_rept <- read_csv("E:/Yale data/IUCN/redlist_species_data_e1397a22-f30b-4a9c-80f0-e1f66cf875a2_REPTILES/assessments.csv") %>% 
  dplyr::select(internalTaxonId, scientificName, redlistCategory, redlistCriteria)

match_reptiles <- match_iucn(taxonomies = taxonomies,
                            tax = tax_rept, 
                            ass = ass_rept,
                            group = "reptiles")


### E.Ants #####################################################################
tax_ants <- read_csv("E:/Yale data/IUCN/redlist_species_data_97d5d52c-af8a-4e5c-b14d-105447dcbb93_ANTS/taxonomy.csv")
ass_ants <- read_csv("E:/Yale data/IUCN/redlist_species_data_97d5d52c-af8a-4e5c-b14d-105447dcbb93_ANTS/assessments.csv") %>% 
  dplyr::select(internalTaxonId, scientificName, redlistCategory, redlistCriteria)

match_ants <- match_iucn(taxonomies = taxonomies,
                             tax = tax_ants, 
                             ass = ass_ants,
                             group = "ants")


### F. Butterflies #############################################################
tax_butt <- read_csv("E:/Yale data/IUCN/redlist_species_data_70c87c04-d4c0-4398-a692-8b809a4cf472_BUTTERFLIES/taxonomy.csv")
ass_butt <- read_csv("E:/Yale data/IUCN/redlist_species_data_70c87c04-d4c0-4398-a692-8b809a4cf472_BUTTERFLIES/assessments.csv") %>% 
  dplyr::select(internalTaxonId, scientificName, redlistCategory, redlistCriteria)

butt_families <- taxonomies %>% 
  filter(group == "butterflies") %>% 
  dplyr::select(family) %>% 
  filter(!is.na(family)) %>% 
  distinct() %>% pull()

match_butt <- match_iucn(taxonomies = taxonomies,
                         tax = tax_butt, 
                         ass = ass_butt,
                         group = "butterflies",
                         families = butt_families)


################################################################################
#### 3. SUMMARIZE INFORMATION
################################################################################

match_iucn_results <- rbind(match_dragonflies,
                            match_mammals,
                            match_crabs,
                            match_reptiles,
                            match_ants,
                            match_butt)

write.csv(match_iucn_results, 
          file = paste0("results/match_iucn_results_",date,".csv"),
          row.names = F)

