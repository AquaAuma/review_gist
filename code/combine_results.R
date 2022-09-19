################################################################################
#### Combine results from all tables
#### Coding and data processing: Aurore Maureaud
#### September 2022
################################################################################

rm(list = ls())

# set date
date <- 'SEPT2022'

# libraries
library(tidyverse)
library(readxl)

# load results tables
completeness <- read.csv("results/completeness_SEPT2022.csv") %>% 
  dplyr::select(group, completeness_2dec)
col <- read.csv("results/match_col_results_SEPT2022.csv") %>% 
  dplyr::select(group, prop_acc_syn, prop_no_match) %>% 
  rename(COL_to_MOL = prop_acc_syn,
         COL_no_match = prop_no_match)
gbif <- read.csv("results/match_gbif_results_SEPT2022.csv") %>% 
  dplyr::select(group, prop_acc_syn) %>% 
  rename(GBIF_to_MOL = prop_acc_syn)
ncbi <- read.csv("results/match_ncbi_results_SEPT2022.csv") %>% 
  dplyr::select(group, prop_no_match, prop_acc_syn) %>% 
  rename(NCBI_to_MOL = prop_acc_syn,
         NCBI_no_match = prop_no_match)
tree <- read.csv("results/match_phylotree_results_SEPT2022.csv") %>% 
  dplyr::select(group, prop_no_match, prop_acc_syn) %>% 
  rename(TREE_to_MOL = prop_acc_syn,
         TREE_no_match = prop_no_match)
iucn <- read.csv("results/match_iucn_results_SEPT2022.csv") %>% 
  dplyr::select(group, prop_acc_syn, prop_acc_syn_ass) %>% 
  rename(IUCN_to_MOL = prop_acc_syn,
         IUCN_assessed = prop_acc_syn_ass)
griis <- read.csv("results/match_griis_results_SEPT2022.csv") %>% 
  dplyr::select(group, prop_no_match) %>% 
  rename(GRIIS_no_match = prop_no_match)

results <- left_join(gbif, completeness, by = "group")
results <- left_join(results, col, by = "group")
results <- left_join(results, ncbi, by = "group")
results <- left_join(results, tree, by = "group")
results <- left_join(results, iucn, by = "group")
results <- left_join(results, griis, by = "group") %>% 
  dplyr::select(group, completeness_2dec,COL_to_MOL, COL_no_match,NCBI_to_MOL,NCBI_no_match,
                TREE_to_MOL, TREE_no_match, GBIF_to_MOL, IUCN_to_MOL, 
                IUCN_assessed, GRIIS_no_match)

results <- t(results)
colnames(results) <- results[1,]
results <- data.frame(results)
results <- results[2:nrow(results),] %>% 
  dplyr::select(ants, butterflies, crabs, dragonflies, mammals, birds, reptiles, amphibians, daisies)

write.csv(results, file = "results/matching_results.csv", row.names = FALSE)






