################################################################################
#### Assessment names from MOL taxonomies and make taxonomies file
#### Coding and data processing: Aurore Maureaud
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

ants <- read_csv("E:/Yale data/MOL/taxonomy/MOL_AntsTaxonomy_v2.1.csv")
syn_sub <- subset(ants, accid==0 & !is.na(subspecies))$id
ants <- ants %>% 
  mutate(group = "ants") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship) %>% 
  filter(!accid %in% syn_sub,
         !id %in% syn_sub)
get_counts(ants, group = "ants")

butterflies <- read_csv("E:/Yale data/MOL/taxonomy/MOL_ButterfliesTaxonomy_v3.csv",
                        col_types = list(flag = col_character()))
syn_sub <- subset(butterflies, accid==0 & !is.na(subspecies))$id
butterflies <- butterflies %>% 
  mutate(order = NA_character_,
         group = "butterflies") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship) %>% 
  filter(!accid %in% syn_sub,
         !id %in% syn_sub)
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

dragonflies <- read_excel("E:/Yale data/MOL/taxonomy/MOL_OdonataTaxonomy_v3.0.xlsx",
                          col_types = c(rep("text", times = 3),rep("numeric",times = 2),
                                        rep("text", times = 15))) %>% 
  rename(subspecies = Subspecies,
         canonical = Canonical,
         genus = Genus,
         species = Species) %>% 
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

amphi <- read_csv("E:/Yale data/MOL/taxonomy/MOL_AmphibiaTaxonomy_v2.0.csv") %>% 
  mutate(group = "amphibians") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(amphi, group = "amphibians")

trees <- read_csv("E:/Yale data/MOL/taxonomy/NA_Trees_canonical.csv")
syn_sub <- subset(trees, AccId==0 & taxon_rank != "species")$Id
trees <- trees %>% 
  filter(!AccId %in% syn_sub,
         !Id %in% syn_sub) %>% 
  rename(accid = AccId,
         id = Id,) %>% 
  mutate(group = "trees",
         order = NA_character_,
         family = NA_character_,
         genus = ,
         species = ,
         subspecies = ,
         authorship = 
         ) %>% 
  dplyr::select()

plants <- read_delim("E:/Yale data/MOL/taxonomy/Vascular.Plants.Master.Taxonomyv6.1-31.01.2022.txt", 
                     delim = "|", escape_double = FALSE, trim_ws = TRUE)
  

################################################################################
### 2. Merge taxonomies
################################################################################
taxonomies <- rbind(dragonflies, ants, mammals, crabs, butterflies, reptiles, birds, amphi)
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
                 get_counts(birds, group = "birds"),
                 get_counts(amphi, group = "amphibians"))
write.csv(numbers, file = paste0("results/numbers_",date,".csv"), row.names = F)

