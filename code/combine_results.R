################################################################################
#### Combine results from all tables
#### Coding and data processing: Aurore Maureaud
#### October 2022
################################################################################

rm(list = ls())

# set date
date <- 'OCT2022'

# libraries
library(tidyverse)
library(readxl)

# load results tables
completeness <- read.csv("results/completeness_OCT2022.csv") %>% 
  dplyr::select(group, completeness_2dec)
col <- read.csv("results/match_col_results_OCT2022.csv") %>% 
  dplyr::select(group, prop_acc_syn, prop_no_match) %>% 
  rename(COL_to_MOL = prop_acc_syn,
         COL_no_match = prop_no_match)
gbif <- read.csv("results/match_gbif_results_OCT2022.csv") %>% 
  dplyr::select(group, prop_acc_syn) %>% 
  rename(GBIF_to_MOL = prop_acc_syn)
ncbi <- read.csv("results/match_ncbi_results_OCT2022.csv") %>% 
  dplyr::select(group, prop_no_match, prop_acc_syn) %>% 
  rename(NCBI_to_MOL = prop_acc_syn,
         NCBI_no_match = prop_no_match)
tree <- read.csv("results/match_phylotree_results_OCT2022.csv") %>% 
  dplyr::select(group, prop_no_match, prop_acc_syn) %>% 
  rename(TREE_to_MOL = prop_acc_syn,
         TREE_no_match = prop_no_match)
iucn <- read.csv("results/match_iucn_results_OCT2022.csv") %>% 
  dplyr::select(group, prop_acc_syn, prop_acc_syn_ass) %>% 
  rename(IUCN_to_MOL = prop_acc_syn,
         IUCN_assessed = prop_acc_syn_ass)
griis <- read.csv("results/match_griis_results_OCT2022.csv") %>% 
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

write.csv(results, file = "results/matching_results.csv", row.names = TRUE)


# score based on results
results <- left_join(gbif, completeness, by = "group")
results <- left_join(results, col, by = "group")
results <- left_join(results, ncbi, by = "group")
results <- left_join(results, tree, by = "group")
results <- left_join(results, iucn, by = "group")
results <- left_join(results, griis, by = "group") %>% 
  dplyr::select(group, completeness_2dec,COL_to_MOL, COL_no_match,NCBI_to_MOL,NCBI_no_match,
                TREE_to_MOL, TREE_no_match, GBIF_to_MOL, IUCN_to_MOL, 
                IUCN_assessed, GRIIS_no_match)

scores <- results %>%
  mutate(score_COMPLETENESS = NA_character_,
         score_COMPLETENESS = case_when(completeness_2dec<10 & completeness_2dec>0 ~ "3",
                   completeness_2dec<15 & completeness_2dec>10 ~ "2",
                   completeness_2dec<20 & completeness_2dec>15 ~ "1",
                   completeness_2dec>20 ~ "0",
                   is.na(completeness_2dec) ~ "0*",
                   completeness_2dec==0 ~ "0*",
                   TRUE ~ NA_character_),
         score_COL_to_MOL = NA_character_,
         score_COL_to_MOL = case_when(COL_to_MOL>90 ~ "3",
                                      COL_to_MOL<90 & COL_to_MOL>70 ~ "2",
                                      COL_to_MOL>50 & COL_to_MOL<70 ~ "1",
                                      COL_to_MOL<50 ~ "0",
                                      is.na(COL_to_MOL) ~ "0*",
                                      TRUE ~ NA_character_),
         score_COL_no_match = NA_character_,
         score_COL_no_match = case_when(COL_no_match<5 ~ "3",
                                        COL_no_match<10 & COL_no_match>5 ~ "2",
                                        COL_no_match<15 & COL_no_match>10 ~ "1",
                                        COL_no_match>15 ~ "0",
                                        is.na(COL_no_match) ~ "0*",
                                        TRUE ~ NA_character_),
         score_NCBI_to_MOL = NA_character_,
         score_NCBI_to_MOL = case_when(NCBI_to_MOL>75 ~ "3",
                                       NCBI_to_MOL<75 & NCBI_to_MOL>50 ~ "2",
                                       NCBI_to_MOL<50 & NCBI_to_MOL>25 ~ "1",
                                       NCBI_to_MOL<25 ~ "0",
                                       is.na(NCBI_to_MOL) ~ "0*",
                                       TRUE ~ NA_character_),
         score_NCBI_no_match = NA_character_,
         score_NCBI_no_match = case_when(NCBI_no_match<10 ~ "3",
                                         NCBI_no_match>10 & NCBI_no_match<20 ~ "2",
                                         NCBI_no_match>20 & NCBI_no_match<30 ~ "1",
                                         NCBI_no_match>30 ~ "0",
                                         is.na(NCBI_no_match) ~ "0*",
                                         TRUE ~ NA_character_),
         score_TREE_to_MOL = NA_character_,
         score_TREE_to_MOL = case_when(TREE_to_MOL>75 ~ "3",
                                       TREE_to_MOL<75 & TREE_to_MOL>50 ~ "2",
                                       TREE_to_MOL<50 & TREE_to_MOL>25 ~ "1",
                                       TREE_to_MOL<25 ~ "0",
                                       is.na(TREE_to_MOL) ~ "0*",
                                       TRUE ~ NA_character_),
         score_TREE_no_match = NA_character_,
         score_TREE_no_match = case_when(TREE_no_match<10 ~ "3",
                                         TREE_no_match>10 & TREE_no_match<20 ~ "2",
                                         TREE_no_match>20 & TREE_no_match<30 ~ "1",
                                         TREE_no_match>30 ~ "0",
                                         is.na(TREE_no_match) ~ "0*",
                                         TRUE ~ NA_character_),
         score_GBIF = NA_character_,
         score_GBIF = case_when(GBIF_to_MOL>90 ~ "3",
                                GBIF_to_MOL>70 & GBIF_to_MOL<90 ~ "2",
                                GBIF_to_MOL>50 & GBIF_to_MOL<70 ~ "1",
                                GBIF_to_MOL<50 ~ "0",
                                is.na(GBIF_to_MOL) ~ "0*",
                                TRUE ~ NA_character_),
         score_GRIIS = NA_character_,
         score_GRIIS = case_when(GRIIS_no_match<5 ~ "3",
                                 GRIIS_no_match>5 & GRIIS_no_match<10 ~ "2",
                                 GRIIS_no_match>10 & GRIIS_no_match<15 ~ "1",
                                 GRIIS_no_match>15 ~ "0",
                                 is.na(GRIIS_no_match) ~ "0*",
                                 TRUE ~ NA_character_),
         score_IUCN_to_MOL = NA_character_,
         score_IUCN_to_MOL = case_when(IUCN_to_MOL>75 ~ "3",
                                       IUCN_to_MOL<75 & IUCN_to_MOL>50 ~ "2",
                                       IUCN_to_MOL<50 & IUCN_to_MOL>25 ~ "1",
                                       IUCN_to_MOL<25 ~ "0",
                                       is.na(IUCN_to_MOL) ~ "0*",
                                       TRUE ~ NA_character_),
         score_IUCN_assessed = NA_character_,
         score_IUCN_assessed = case_when(IUCN_assessed>75 ~ "3",
                                         IUCN_assessed<75 & IUCN_assessed>50 ~ "2",
                                         IUCN_assessed<50 & IUCN_assessed>25 ~ "1",
                                         IUCN_assessed<25 ~ "0",
                                         is.na(IUCN_assessed) ~ "0*",
                                         TRUE ~ NA_character_)) %>% 
  dplyr::select(group, completeness_2dec, score_COMPLETENESS,
                COL_to_MOL, score_COL_to_MOL,
                COL_no_match, score_COL_no_match,
                NCBI_to_MOL, score_NCBI_to_MOL,
                NCBI_no_match, score_NCBI_no_match,
                TREE_to_MOL, score_TREE_to_MOL,
                TREE_no_match, score_TREE_no_match,
                GBIF_to_MOL, score_GBIF,
                GRIIS_no_match, score_GRIIS,
                IUCN_to_MOL, score_IUCN_to_MOL,
                IUCN_assessed, score_IUCN_assessed)

write.csv(scores, file = "results/matching_results_scores.csv", row.names = TRUE)
