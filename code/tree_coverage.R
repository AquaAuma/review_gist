################################################################################
#### Assessment of IUCN coverage per group
#### Coding and data processing: Aurore Maureaud & Emily Sandall
#### November 2021
################################################################################

rm(list = ls())

# set date
date <- '19NOV2021'

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
phylotree <- read_csv(file = "C:/Users/auror/Downloads/OpenTreePhylogeny_assessment/OpenZoom_PhylogeneticTaxonomy1121.csv")


################################################################################
#### 1. MATCH FUNCTION
################################################################################
match_tree <- function(taxonomies, ncbi_raw, group) {
  
}


################################################################################
#### 2. APPLY TO GROUPS
################################################################################

### A. dragonflies #############################################################

uid_position <- which(phylotree$name == "Odonata")
uid_dragonflies <- phylotree$uid[uid_position]
tree_dragonflies <- phylotree[uid_position,]

while(rank!="species"){
  uid_positions <- which(phylotree$parent_uid == uid_dragonflies)
  uid_children <- phylotree$uid[uid_positions]
  tree_dragonflies <- rbind(tree_dragonflies, phylotree[uid_positions,])
  
}