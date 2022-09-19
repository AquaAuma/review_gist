################################################################################
#### Assessment names from MOL taxonomies and make taxonomies file
#### Coding and data processing: Aurore Maureaud
#### September 2022
################################################################################

rm(list = ls())

# set date
date <- 'SEPT2022'

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
mammals <- read_csv("data/MOL/Copy of MOL_MammaliaTaxonomy_v2.2.csv",
                    col_types = list(flag = col_character(),
                                     subspecies = col_character())) %>% 
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
reptiles <- read_csv("data/MOL/Copy of MOL_Reptilia_taxonomy_v2.2_Complete.csv",
                     col_types = list(flags = col_character(),
                                      infraspecies = col_character())) %>% 
  rename(subspecies = infraspecies) %>% 
  mutate(group = "reptiles") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(reptiles, group = "reptiles")


### E.Ants #####################################################################
ants <- read_csv("data/MOL/Copy of MOL_AntsTaxonomy_v3.1.csv")
syn_sub <- subset(ants, accid==0 & !is.na(subspecies))$id
ants <- ants %>% 
  mutate(group = "ants") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship) %>% 
  filter(!accid %in% syn_sub,
         !id %in% syn_sub)
get_counts(ants, group = "ants")


### F. Butterflies #############################################################
butterflies <- read_csv("data/MOL/Copy of MOL_ButterfliesTaxonomy_v4.csv",
                        col_types = list(flags = col_character())) %>% 
  rename(subspecies = infraspecies)
syn_sub <- subset(butterflies, accid==0 & !is.na(subspecies))$id
butterflies <- butterflies %>% 
  mutate(order = NA_character_,
         group = "butterflies") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship) %>% 
  filter(!accid %in% syn_sub,
         !id %in% syn_sub)
get_counts(butterflies, group = "butterflies")


### G. Birds ###################################################################
# birds <- read_csv("data/MOL/MOL_AvesTaxonomy_v2.1.csv",
#                   col_types = list(authorship = col_character())) %>% 
#   mutate(group = "birds") %>% 
#   dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
# get_counts(birds, group = "birds")


### H. Amphibians ##############################################################
amphi <- read_csv("data/MOL/Copy of MOL_AmphibiaTaxonomy_v2.1.csv") %>% 
  rename(subspecies = infraspecies) %>% 
  mutate(group = "amphibians") %>% 
  dplyr::select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)
get_counts(amphi, group = "amphibians")


### J. Plants ##################################################################
plants <- read_delim("E:/Yale data/MOL/taxonomy/Vascular.Plants.Master.Taxonomyv6.1-31.01.2022.txt", 
                     delim = "|", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(order = Accepted_Name_Order,
         family = Accepted_Name_Family,
         authorship = auth_name,
         authorship_year = Publication_Year,
         canonical = nom_nude, 
         subspecies = infrasp_name,
         status = Status,
         rank = Accepted_Taxon_Level,
         accepted_name = Accepted_Nom_Nude_Binomial)%>% 
  dplyr::select(canonical, order, family, genus, species, subspecies, authorship,
                authorship_year, status, rank, accepted_name)

## Compositae
compo <- plants %>% 
  mutate(group = "daisies") %>% 
  filter(family == "Asteraceae")
compo$id <- 1:nrow(compo)
compo <- compo %>% 
  mutate(accid = ifelse(status == "accepted", 0, NA))

# remove synonyms not matched to accepted names
for(i in 1:nrow(compo)){
  if(is.na(compo$accid[i])){
    j <- which(compo$canonical == compo$accepted_name[i])
    sub_j <- compo[j,]
    sub_j <- subset(sub_j, status == "accepted")
    
    if(nrow(sub_j)==1){
      compo$accid[i] <- sub_j$id
    }
    if(nrow(sub_j)>1){
      print(paste0("More than one accepted name for i=",i))
    }
    if(nrow(sub_j)==0){
      print(paste0("No corresponding accepted name for i=",i))
    }
    rm(j, sub_j)
  }
}

# remove infraspecies and synonyms considered accepted in the mastertaxonomy
syn_sub <- subset(compo, accid==0 & !is.na(subspecies))$id
syn_sub2 <- subset(compo, accid==0 & rank!="species")$id
syn_sub <- unique(c(syn_sub, syn_sub2))

compo <- compo %>% 
  filter(!accid %in% syn_sub,
         !id %in% syn_sub,
         !is.na(accid)) %>% 
  mutate(authorship = ifelse(!is.na(authorship_year),
                             paste(authorship, authorship_year, sep = " "),
                             authorship)) %>% 
  select(id, accid, canonical, order, family, genus, species, subspecies, group, authorship)


get_counts(compo, group = "daisies")


################################################################################
### 2. Merge taxonomies
################################################################################
taxonomies <- rbind(dragonflies, ants, mammals, crabs, butterflies, reptiles, 
                    birds, amphi, conifers, palms, cacti, compo)
write.csv(taxonomies, file = paste0("data/taxonomies_",date,".csv"), row.names = F)


################################################################################
### 3. Count names
################################################################################
numbers <- rbind(get_counts(ants, group = "ants"),
                 get_counts(butterflies, group = "butterflies"),
                 get_counts(mammals, group = "mammals"),
                 get_counts(crabs, group = "crabs"),
                 get_counts(dragonflies, group = "dragonflies"),
                 get_counts(reptiles, group = "reptiles"),
                 get_counts(birds, group = "birds"),
                 get_counts(amphi, group = "amphibians"),
                 #get_counts(bees, group = "bees"),
                 get_counts(compo, group = "daisies"),
                 get_counts(palms, group = "palms"),
                 get_counts(cacti, group = "cacti"),
                 get_counts(conifers, group = "conifers"))
write.csv(numbers, file = paste0("results/numbers_",date,".csv"), row.names = F)

