################################################################################
#### Assessment of OpenTree coverage per group
#### Coding and data processing: Aurore Maureaud & Emily Sandall
#### November 2021
################################################################################

rm(list = ls())

# set date
date <- '22NOV2021'

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
taxonomies <- read.csv("data/taxonomies_19NOV2021.csv")
phylotree <- read_csv(file = "E:/Yale data/OpenTreePhylogeny_assessment/OpenZoom_PhylogeneticTaxonomy1121.csv") %>% 
  select(-X1,-`...15`)


################################################################################
#### 1. MATCH FUNCTION
################################################################################
match_tree <- function(taxonomies, phylotree, group, parent) {
  uid_position <- which(phylotree$name %in% parent)
  sub_tree <- phylotree[uid_position,]
  tree <- phylotree[uid_position,]
  rank <- sub_tree$rank
  higher_ranks <- c("genus","phylum","class","order","family","subfamily",
                    "subgenus","superfamily","tribe","suborder","subphylum",
                    "subclass","subkingdom","infrakingdom","superorder","superclass",
                    "infraphylum","infraorder","kingdom","subdivision","section",
                    "subtribe","subsection","superphylum","parvorder","infraclass",
                    "supertribe","subterclass","cohort","subcohort")
  
  while(any(rank %in% higher_ranks)){
    # get data with higher ranks
    sub_tree <- sub_tree %>% 
      filter(rank %in% higher_ranks)
    # get position of lower level & uids
    uid_positions <- which(phylotree$parent_uid %in% sub_tree$uid)
    uid_children <- phylotree$uid[uid_positions]
    # test new ranks
    rank <- unique(phylotree[uid_positions,]$rank)
    # add data of the group
    tree <- rbind(tree, phylotree[uid_positions,])
    sub_tree <- phylotree[uid_positions,]
  }
  
  tree <- tree %>% 
    filter(rank !="no rank",
           rank == "species")
  
  ## a. Canonical from NCBI without match as the species level
  tree_no_match <- tree %>% 
    filter(str_detect(name, pattern = " sp.") | str_detect(name, pattern = " sample"))
  
  ## b. Match with all names
  tree_spp <- tree %>% 
    filter(!str_detect(name, pattern = " sp.",),
           !str_detect(name, pattern = " sample"))
  
  mol_m <- taxonomies[taxonomies$group==group,] %>% 
    dplyr::select(canonical, group) %>% 
    distinct() # to remove the duplicates from different sources
  
  match_tot <- full_join(mol_m, tree_spp, by = c("canonical" = "name"),keep = TRUE) %>% 
    filter(!is.na(canonical)) %>% 
    mutate(phylo = ifelse(!is.na(name),"yes",NA_character_))
  
  ## c. No match
  no_match <- anti_join(tree_spp, mol_m, by = c("name" = "canonical"))
  
  ## d. Accepted names match
  mol_acc <- taxonomies[taxonomies$group==group,] %>% 
    filter(accid == 0)
  match_acc <- left_join(mol_acc, tree_spp, by = c("canonical" = "name"),keep=TRUE) %>% 
    mutate(phylo = ifelse(!is.na(name),"yes",NA_character_))
  
  ## e. Synonyms match
  mol_syn <- taxonomies[taxonomies$group==group,] %>% 
    filter(accid != 0)
  match_syn <- left_join(mol_syn, tree_spp, by = c("canonical" = "name"),keep=TRUE) %>% 
    filter(!is.na(name))  %>% 
    mutate(phylo_syn = "yes") %>% 
    dplyr::select(accid, phylo_syn) %>% 
    distinct()
  
  ## f. Total accepted species match
  match_acc_syn <- left_join(match_acc, match_syn,
                             by = c("id" = "accid")) %>% 
    mutate(phylo = ifelse(is.na(phylo), phylo_syn, phylo))
  
  ## g. Summary
  prop_no_possible_match <- round(nrow(tree_no_match)/nrow(tree)*100,2)
  prop_no_match <- round(nrow(no_match)/nrow(tree)*100,2)
  prop_tot <- round(nrow(match_tot[!is.na(match_tot$phylo),])/nrow(match_tot)*100,2)
  prop_acc <- round(nrow(match_acc[!is.na(match_acc$phylo),])/nrow(match_acc)*100,2)
  prop_acc_syn <- round(nrow(match_acc_syn[!is.na(match_acc_syn$phylo),])/nrow(match_acc_syn)*100,2)
  group <- group
  return(data.frame(cbind(group, prop_tot, prop_acc, prop_acc_syn,
                          prop_no_match, prop_no_possible_match)))
}


################################################################################
#### 2. APPLY TO GROUPS
################################################################################

### A. dragonflies #############################################################
match_dragonflies <- match_tree(taxonomies = taxonomies,
                                phylotree = phylotree,
                                group = "dragonflies",
                                parent = "Odonata")


### B. mammals #################################################################
match_mammals <- match_tree(taxonomies = taxonomies,
                            phylotree = phylotree,
                            group = "mammals",
                            parent = "Mammalia")


### C. Crabs ###################################################################
crab_families <- taxonomies %>% 
  filter(group == "crabs") %>% 
  dplyr::select(family) %>% 
  filter(!is.na(family)) %>% 
  distinct() %>% pull()

match_crabs <- match_tree(taxonomies = taxonomies,
                          phylotree = phylotree,
                          group = "crabs",
                          parent = crab_families)


### D. Reptiles ################################################################
match_reptiles <- match_tree(taxonomies = taxonomies,
                             phylotree = phylotree,
                             group = "reptiles",
                             parent = c("Lepidosauria","Testudines","Crocodylia"))


### E.Ants #####################################################################
match_ants <- match_tree(taxonomies = taxonomies,
                         phylotree = phylotree,
                         group = "ants",
                         parent = "Formicidae")


### F. Butterflies #############################################################
butt_families <- taxonomies %>% 
  filter(group == "butterflies") %>% 
  dplyr::select(family) %>% 
  filter(!is.na(family)) %>% 
  distinct() %>% pull()

match_butt <- match_tree(taxonomies = taxonomies,
                         phylotree = phylotree,
                         group = "butterflies",
                         parent = butt_families)


### G. Birds ###################################################################
match_birds <- match_tree(taxonomies = taxonomies,
                          phylotree = phylotree,
                          group = "birds",
                          parent = "Aves")


################################################################################
#### 3. SUMMARIZE INFORMATION
################################################################################

match_phylotree_results <- rbind(match_dragonflies,
                            match_mammals,
                            match_crabs,
                            match_reptiles,
                            match_ants,
                            match_butt,
                            match_birds)

write.csv(match_phylotree_results, 
          file = paste0("results/match_phylotree_results_",date,".csv"),
          row.names = F)

