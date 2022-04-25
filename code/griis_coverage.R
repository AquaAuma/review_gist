################################################################################
#### Assessment of IUCN coverage per group
#### Coding and data processing: Maisha Lucas
#### April 2022
################################################################################

rm(list = ls())

library(tidyverse)
library(readr)

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
griis <- read.csv("data/GRIIS_2020_03_01.csv")


################################################################################
#### 1. MATCH FUNCTION
################################################################################

match_griis <- function(taxonomies, griis, group, parent, level){

  ## a. Create GRIIS filter function
  griis_tax <- griis %>% 
    filter(get(level) %in% parent)
  
  ## b. Create MOL filter function
  mol_tax <- taxonomies %>% 
    filter(group == group)
  
  group <- group
  
  ## c. Anti join to get the non-matched names
  no_match <- anti_join(griis_tax, mol_tax, by = c("sciname" = "canonical")) 
  
  nbr_spp <- nrow(griis_tax)
  prop_no_match <- round(nrow(no_match)  / nrow(griis_tax)*100,2)
  
  return(data.frame(cbind(group, nbr_spp, prop_no_match)))
  
}


################################################################################
#### 2. APPLY TO GROUPS
################################################################################

### A. dragonflies #############################################################
match_dragonflies <- match_griis(taxonomies = taxonomies,
                                 griis = griis,
                                 group = "dragonflies",
                                 parent = "Odonata",
                                 level = "accepted.order")

### B. Mammals #################################################################
match_mammals <- match_griis(taxonomies = taxonomies,
                             griis = griis,
                             group = "mammals",
                             parent = "Mammalia",
                             level = "accepted.class")


### C. Crabs ###################################################################
crab_families <- taxonomies %>%
  filter(group == "crabs") %>%
  dplyr::select(family) %>%
  filter(!is.na(family)) %>%
  distinct() %>% pull()

match_crabs <- match_griis(taxonomies = taxonomies,
                           griis = griis,
                           group = "crabs",
                           parent = crab_families,
                           level = "accepted.family")

### D. Reptiles ################################################################
match_reptiles <- match_griis(taxonomies = taxonomies,
                              griis = griis,
                              group = "reptiles",
                              parent = "Reptilia",
                              level = "accepted.class")

### E.Ants ##################################################################### 
match_ants <- match_griis(taxonomies = taxonomies,
                          griis = griis,
                          group = "ants",
                          parent = "Formicidae",
                          level = "accepted.family")

### F. Butterflies #############################################################
butterfly_families <- taxonomies %>%
  filter(group == 'butterflies') %>%
  dplyr::select(family) %>%
  filter(!is.na(family))%>%
  distinct() %>% pull()

match_butterflies <- match_griis(taxonomies = taxonomies,
                                 griis = griis,
                                 group = "butterflies",
                                 parent = butterfly_families,
                                 level = "accepted.family")

### G. Birds #############################################################
match_birds <- match_griis(taxonomies = taxonomies,
                                 griis = griis,
                                 group = "birds",
                                 parent = "Aves",
                                 level = "accepted.class")

### G. Amphibians #########################################################
match_amphi <- match_griis(taxonomies = taxonomies,
                           griis = griis,
                           group = "amphibians",
                           parent = "Amphibia",
                           level = "accepted.class")


################################################################################
#### 3. SUMMARIZE INFORMATION
################################################################################

match_griis_results <- rbind (match_dragonflies,
                              match_mammals,
                              match_crabs,
                              match_reptiles,
                              match_ants,
                              match_butterflies,
                              match_birds,
                              match_amphi)

write.csv(match_griis_results, 
          file = paste0("results/match_griis_results_",date,".csv"),
          row.names = F)
