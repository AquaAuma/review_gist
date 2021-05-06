###############################################################################
#### Figures showing the taxonomic descriptions through time across taxa
#### Coding and data processing: Aurore Maureaud
#### May 2021
################################################################################

rm(list = ls())

# set date
date <- '6MAY2021'

# libraries
library(ggplot2)
library(tidyverse)
library(readxl)


### 1. Load data
# Dragonflies
odonata <- read_excel("data/Odonata.xlsx")
unique(odonata$accid) #check accid, should be 0
unique(odonata$taxonRank) # check rank, should be species/subspecies or alike
unique(odonata$status) # check status, should be accepted

# Crabs
anomura <- read_excel("data/Anomura.xlsx")
unique(anomura$accid) 
unique(anomura$taxonRank)
unique(anomura$status)
brachyura <- read_excel("data/Brachyura.xlsx")
unique(brachyura$accid)
unique(brachyura$taxonRank)
unique(brachyura$status)


### 2. Merge taxonomies
identical(names(odonata),names(brachyura))
taxonomies <- rbind(odonata, brachyura)
identical(names(taxonomies),names(anomura))
taxonomies <- rbind(taxonomies, anomura)

unique(taxonomies$group)


### 3. Extract authorship year
taxonomies <- taxonomies %>% 
  mutate(year = str_extract(authorship, "[0-9]+"),
         year = as.numeric(year))
sort(unique(taxonomies$year)) # problem with years for 5 odonate spp


### 4. Count number of description / year / group
descriptions <- taxonomies %>% 
  group_by(group, year) %>% 
  summarize(nbr_description = length(canonical))

### 4. Make figure
ggplot() + geom_bar(data = descriptions, aes(y = nbr_description, x = as.factor(year)),
                    stat = "identity") +
  facet_wrap( ~ group, nrow=1) +
  xlab("Year of description") + ylab("Number of descriptions") +
  scale_x_discrete(breaks = c(1750,1800,1850,1900,1950,2000), 
                   labels = c(1750,1800,1850,1900,1950,2000)) +
  theme_bw()






