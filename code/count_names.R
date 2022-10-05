################################################################################
#### Assessment names from MOL taxonomies and make taxonomies file
#### Coding and data processing: Aurore Maureaud
#### October 2022
################################################################################

rm(list = ls())

# set date
date <- 'OCT2022'

# libraries
library(tidyverse)
library(readxl)

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

### A. dragonflies #############################################################
dragonflies <- read_excel("data/MOL/Copy of MOL_OdonataTaxonomy_v3.1.xlsx") %>% 
  rename(subspecies = Subspecies,
         canonical = Canonical,
         genus = Genus,
         species = Species,
         authorship = Authorship) %>% 
  mutate(group = "dragonflies",
         subspecies = ifelse(subspecies == "NA",NA_character_,subspecies)) %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(dragonflies, group = "dragonflies")


### B. Mammals #################################################################
mammals <- read_csv("data/MOL/Copy of MOL_MammaliaTaxonomy_v2.3_complete.csv") %>% 
  rename(subspecies = infraspecies) %>% 
  mutate(group = "mammals") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(mammals, group = "mammals")


### C. Crabs ###################################################################
# anomura <- read_csv("E:/Yale data/MOL/taxonomy/MOL_AnomuransTaxonomy_v1.0.csv")
# brachyura <- read_csv("E:/Yale data/MOL/taxonomy/MOL_BrachyuransTaxonomy_v1.0.csv")
# crabs <- rbind(anomura, brachyura) %>% 
#   mutate(subspecies = NA_character_,
#          group = "crabs") %>% 
#   dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
# get_counts(crabs, group = "crabs")


### D. Reptiles ################################################################
reptiles <- read_csv("data/MOL/Copy of MOL_ReptilesTaxonomy_v2.3_complete.csv",
                     col_types = list(flags = col_character(),
                                      infraspecies = col_character())) %>% 
  rename(subspecies = infraspecies) %>% 
  mutate(group = "reptiles") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(reptiles, group = "reptiles")


### E.Ants #####################################################################
ants <- read_csv("data/MOL/Copy of MOL_AntsTaxonomy_v3.1_noinfrasubsp.csv")
#syn_sub <- subset(ants, accid==0 & !is.na(subspecies))$id
ants <- ants %>% 
  mutate(group = "ants") %>% 
  rename(subspecies = infraspecies) %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
  # filter(!accid %in% syn_sub,
  #        !id %in% syn_sub)
get_counts(ants, group = "ants")


### F. Butterflies #############################################################
butterflies <- read_csv("data/MOL/Copy of MOL_ButterfliesTaxonomy_v4.1.csv",
                        col_types = list(flags = col_character())) %>% 
  rename(subspecies = infraspecies)
#syn_sub <- subset(butterflies, accid==0 & !is.na(subspecies))$id
butterflies <- butterflies %>% 
  mutate(order = NA_character_,
         group = "butterflies") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
  # filter(!accid %in% syn_sub,
  #        !id %in% syn_sub)
get_counts(butterflies, group = "butterflies")


### G. Birds ###################################################################
birds <- read_csv("data/MOL/Copy of MOL_AvesTaxonomy_v2.3_complete.csv") %>%
  mutate(group = "birds") %>%
  rename(subspecies = infraspecies) %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(birds, group = "birds")


### H. Amphibians ##############################################################
amphi <- read_csv("data/MOL/Copy of MOL_AmphibiaTaxonomy_v2.2_complete.csv") %>% 
  rename(subspecies = infraspecies) %>% 
  mutate(group = "amphibians") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(amphi, group = "amphibians")


### I. Plants ##################################################################
compo <- read_csv("data/MOL/MOL_AsteraceaeTaxonomy_v6.1.csv") %>% 
  rename(subspecies = infraspecies) %>% 
  mutate(group = "daisies") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(compo, group = "daisies")


################################################################################
### 2. Merge taxonomies
################################################################################
taxonomies <- rbind(dragonflies, 
                    ants, 
                    mammals, 
                    #crabs, 
                    butterflies, 
                    reptiles, 
                    birds, 
                    amphi, 
                    compo
                    )
write.csv(taxonomies, file = paste0("data/taxonomies_",date,".csv"), row.names = F)


################################################################################
### 3. Count names
################################################################################
numbers <- rbind(get_counts(ants, group = "ants"),
                 get_counts(butterflies, group = "butterflies"),
                 get_counts(mammals, group = "mammals"),
                 #get_counts(crabs, group = "crabs"),
                 get_counts(dragonflies, group = "dragonflies"),
                 get_counts(reptiles, group = "reptiles"),
                 get_counts(birds, group = "birds"),
                 get_counts(amphi, group = "amphibians"),
                 get_counts(compo, group = "daisies")
                 )
write.csv(numbers, file = paste0("results/numbers_",date,".csv"), row.names = F)

