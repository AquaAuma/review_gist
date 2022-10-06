################################################################################
#### Assessment of GBIF coverage per group
#### Coding and data processing: Aurore Maureaud & Yanina Sica
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

# code option
include_harmonization <- TRUE

# load data
if(include_harmonization == TRUE){
  taxonomies <- read.csv("data/taxonomies_OCT2022.csv")
} else {
  taxonomies <- read.csv("data/taxonomies_OCT2022.csv") %>% 
    filter(source != "GBIF")
}

gbiff <- read.csv("data/GBIF/gbif202107_fossil.csv")
gbif <- read.csv("data/GBIF/gbif_unique_names.csv") %>% 
  mutate(taxa = ifelse(family == "Asteraceae", "daisies", taxa)) %>% 
  filter(!scientificname %in% gbiff$scientificname)
rm(gbiff)


################################################################################
#### 1. MATCH FUNCTION
################################################################################
match_gbif <- function(taxonomies, gbif, group) {
  
  ## subset both datasets for the taxa in focus
  mol_m <- taxonomies[taxonomies$group==group,] %>% 
    dplyr::select(canonical, family, group) %>% 
    distinct() # to remove the duplicates from different sources
  
  if(group == "crabs"){
    gbif_spp <- gbif %>% 
      filter(order == "Decapoda",
             family %in% unique(mol_m$family),
             taxonRank %in% c("SPECIES","SUBSPECIES","VARIETY","FORM")) %>% 
      select(!verbatimScientificName) %>% distinct()
  } else {
    gbif_spp <- gbif %>% 
      filter(taxa == group,
             taxonRank %in% c("SPECIES","SUBSPECIES","VARIETY","FORM")) %>% 
      select(!verbatimScientificName) %>% distinct()
  }
  
  ## a. Match with all names
  match_tot <- full_join(mol_m, gbif_spp, by = c("canonical" = "scientificname"),keep = TRUE) %>% 
    filter(!is.na(canonical)) %>% 
    mutate(gbif = ifelse(!is.na(scientificname),"yes",NA_character_))
  
  ## b. No match
  no_match <- anti_join(gbif_spp, mol_m, by = c("scientificname" = "canonical"))
  
  ## c. Accepted names match
  mol_acc <- taxonomies[taxonomies$group==group,] %>% 
    filter(accid == 0)
  match_acc <- left_join(mol_acc, gbif_spp, by = c("canonical" = "scientificname"),keep=TRUE) %>% 
    mutate(gbif = ifelse(!is.na(scientificname),"yes",NA_character_))
  
  ## d. Synonyms match
  mol_syn <- taxonomies[taxonomies$group==group,] %>% 
    filter(accid != 0)
  match_syn <- left_join(mol_syn, gbif_spp, by = c("canonical" = "scientificname"),keep=TRUE) %>% 
    filter(!is.na(scientificname))  %>% 
    mutate(gbif_syn = "yes") %>% 
    dplyr::select(accid, gbif_syn) %>% 
    distinct()
  
  ## e. Total accepted species match
  match_acc_syn <- left_join(match_acc, match_syn,
                             by = c("id" = "accid")) %>% 
    mutate(gbif = ifelse(is.na(gbif), gbif_syn, gbif))
  
  ## f. Summary
  prop_no_match <- round(nrow(no_match)/nrow(gbif_spp)*100,2)
  nbr_spp_gbif <- length(unique(gbif_spp$scientificname))
  prop_tot <- round(nrow(match_tot[!is.na(match_tot$gbif),])/nrow(match_tot)*100,2)
  prop_acc <- round(nrow(match_acc[!is.na(match_acc$gbif),])/nrow(match_acc)*100,2)
  prop_acc_syn <- round(nrow(match_acc_syn[!is.na(match_acc_syn$gbif),])/nrow(match_acc_syn)*100,2)
  group <- group
  return(data.frame(cbind(group, nbr_spp_gbif, prop_tot, prop_acc, prop_acc_syn,
                       prop_no_match)))
}


################################################################################
#### 2. APPLY TO GROUPS
################################################################################

### A. dragonflies #############################################################
match_dragonflies <- match_gbif(taxonomies = taxonomies,
                                gbif = gbif,
                                group = "dragonflies")


### B. Mammals #################################################################
match_mammals <- match_gbif(taxonomies = taxonomies,
                            gbif = gbif,
                            group = "mammals")


### C. Crabs ###################################################################
match_crabs <- match_gbif(taxonomies = taxonomies,
                          gbif = gbif,
                          group = "crabs")


### D. Reptiles ################################################################
match_reptiles <- match_gbif(taxonomies = taxonomies,
                             gbif = gbif,
                             group = "reptiles")


### E.Ants #####################################################################
match_ants <- match_gbif(taxonomies = taxonomies,
                         gbif = gbif,
                         group = "ants")


### F. Butterflies #############################################################
match_butt <- match_gbif(taxonomies = taxonomies,
                         gbif = gbif,
                         group = "butterflies")


### G. Birds ###################################################################
match_birds <- match_gbif(taxonomies = taxonomies,
                          gbif = gbif,
                          group = "birds")


### H. Amphibians ##############################################################
match_amphi <- match_gbif(taxonomies = taxonomies,
                          gbif = gbif,
                          group = "amphibians")


### I. Daisies #################################################################
match_daisies <- match_gbif(taxonomies = taxonomies,
                          gbif = gbif,
                          group = "daisies")


################################################################################
#### 3. SUMMARIZE INFORMATION
################################################################################

match_gbif_results <- rbind(match_dragonflies,
                            match_mammals,
                            match_crabs,
                            match_reptiles,
                            match_ants,
                            match_butt,
                            match_birds,
                            match_amphi,
                            match_daisies)

# load data
if(include_harmonization == TRUE){
  write.csv(match_gbif_results, 
          file = paste0("results/match_gbif_results_",date,".csv"),
          row.names = F)
} else {
  write.csv(match_gbif_results, 
            file = paste0("results/match_gbif_results_noharm_",date,".csv"),
            row.names = F)
}
