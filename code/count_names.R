################################################################################
#### Assessment names from MOL taxonomies and make taxonomies file
#### Coding and data processing: Aurore Maureaud
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

get_counts <- function (dat, group = group) {
  # count number of accepted species
  species <- length(dat[dat$accid==0 & is.na(dat$subspecies),]$canonical)
  # count number of unique accepted species
  species_uni <- length(unique(dat[dat$accid==0 & is.na(dat$subspecies),]$canonical))

  # number of accepted subspecies
  subspecies <- length(dat[dat$accid==0 & !is.na(dat$subspecies),]$canonical)
  # number of unique accepted subspecies
  subspecies_uni <- length(unique(dat[dat$accid==0 & !is.na(dat$subspecies),]$canonical))
  
  # number of synonyms
  syn_spp <- length(dat[dat$accid!=0,]$canonical)
  # number of unique synonyms
  syn_spp_uni <- length(unique(dat[dat$accid!=0,]$canonical))

  return(data.frame(cbind(species, species_uni, subspecies, subspecies_uni, syn_spp, syn_spp_uni, group)))
}


################################################################################
### 1. Load data
################################################################################

ants <- read_csv("E:/Yale data/MOL/taxonomy/MOL_AntsTaxonomy_v2.1.csv") %>% 
  mutate(group = "ants") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(ants, group = "ants")

butterflies <- read_csv("E:/Yale data/MOL/taxonomy/MOL_ButterfliesTaxonomy_v3.csv",
                        col_types = list(flag = col_character())) %>% 
  mutate(order = NA_character_,
         group = "butterflies") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(butterflies, group = "butterflies")

anomura <- read_csv("E:/Yale data/MOL/taxonomy/MOL_AnomuransTaxonomy_v1.0.csv")
brachyura <- read_csv("E:/Yale data/MOL/taxonomy/MOL_BrachyuransTaxonomy_v1.0.csv")
crabs <- rbind(anomura, brachyura) %>% 
  mutate(subspecies = NA_character_,
         group = "crabs") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(crabs, group = "crabs")

mammals <- read_csv("E:/Yale data/MOL/taxonomy/MOL_MammaliaTaxonomy_v2.1.csv",
                    col_types = list(flag = col_character(),
                                     subspecies = col_character())) %>% 
  mutate(group = "mammals") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(mammals, group = "mammals")

dragonflies <- read_excel("E:/Yale data/MOL/taxonomy/MOL_OdonataTaxonomy_v3.0.xlsx") %>% 
  rename(subspecies = Subspecies,
         canonical = Canonical,
         genus = Genus,
         species = Species,
         authorship = Authorship) %>% 
  mutate(group = "dragonflies",
         subspecies = ifelse(subspecies == "NA",NA_character_,subspecies)) %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(dragonflies, group = "dragonflies")

reptiles <- read_csv("E:/Yale data/MOL/taxonomy/MOL_ReptiliaTaxonomy_v2.0_noOddChr.csv",
                     col_types = list(flag = col_character(),
                                      subspecies = col_character())) %>% 
  mutate(group = "reptiles") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(reptiles, group = "reptiles")

birds <- read_csv("E:/Yale data/MOL/taxonomy/MOL_AvesTaxonomy_v2.1.csv",
                  col_types = list(authorship = col_character())) %>% 
  mutate(group = "birds") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(birds, group = "birds")


################################################################################
### 2. Merge taxonomies
################################################################################
taxonomies <- rbind(dragonflies, ants, mammals, crabs, butterflies, reptiles, birds)
write.csv(taxonomies, file = paste0("data/taxonomies_",date,".csv"), row.names = F)


################################################################################
### 2. Count names
################################################################################
numbers <- rbind(get_counts(ants, group = "ants"),
                 get_counts(butterflies, group = "butterflies"),
                 get_counts(mammals, group = "mammals"),
                 get_counts(crabs, group = "crabs"),
                 get_counts(dragonflies, group = "dragonflies"),
                 get_counts(reptiles, group = "reptiles"),
                 get_counts(birds, group = "birds"))
write.csv(numbers, file = paste0("results/numbers_",date,".csv"), row.names = F)
