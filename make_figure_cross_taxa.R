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
library(rphylopic)


################################################################################
### 1. Load data
################################################################################

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


################################################################################
### 2. Merge taxonomies
################################################################################

identical(names(odonata),names(brachyura))
taxonomies <- rbind(odonata, brachyura)
identical(names(taxonomies),names(anomura))
taxonomies <- rbind(taxonomies, anomura)

unique(taxonomies$group)


################################################################################
### 3. Extract authorship year
################################################################################

taxonomies <- taxonomies %>% 
  mutate(year = str_extract(authorship, "[0-9]+"),
         year = as.numeric(year),
         taxa = ifelse(group %in% c("anomurans","brachyurans"),"crabs",group))
sort(unique(taxonomies$year)) # problem with years for 5 odonate spp


################################################################################
### 4.Number of descriptions / year / group
################################################################################

# make summary of data
descriptions <- taxonomies %>% 
  group_by(group, taxa, year) %>% 
  summarize(nbr_description = length(canonical))

# make figure
ggplot() + geom_bar(data = descriptions, aes(y = nbr_description, x = as.factor(year)),
                    stat = "identity") +
  facet_wrap( ~ taxa, nrow=1) +
  xlab("Year of description") + ylab("Number of descriptions") +
  scale_x_discrete(breaks = c(1750,1800,1850,1900,1950,2000), 
                   labels = c(1750,1800,1850,1900,1950,2000)) +
  theme_bw()


################################################################################
### 5.Cumulative descriptions / year / group
################################################################################

# make summary of data with yearly accumulation
cumul_desc <- taxonomies %>% 
  group_by(taxa, year) %>% 
  summarize(nbr_description = length(canonical)) %>% 
  mutate(temp_sum=cumsum(nbr_description))

# make figure
ggplot(data = cumul_desc, aes(y = temp_sum, x = year)) + 
  geom_line(lwd=1.5, color="black") + #geom_point(pch=1, size=0.5) + 
  facet_wrap( ~ taxa, nrow=1, scales = "free_y") +
  xlab("Year of description") + ylab("Cumulative number of descriptions") +
  scale_x_continuous(breaks = c(1750,1800,1850,1900,1950,2000), 
                   labels = c(1750,1800,1850,1900,1950,2000)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, size=10, hjust=1),
        axis.ticks.length.x = unit(.25, "cm"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  add_phylopic(img = crab_img, alpha=1, color="black")


# get silhouettes
crab_uuid <- image_get(uuid = "01dd976b-f6e9-4204-bae1-c15a32234f73")
crab_img <- image_data(crab_uuid$uid, size = "512")[[1]]

dragon_uuid <- image_get(uuid = "8af9c80d-92f9-4ba8-87c0-8ffa026c770c")
dragon_img <- image_data(dragon_uuid$uid, size="512")[[1]]

ggplot(data = cumul_desc[cumul_desc$taxa=="crabs",], aes(y=temp_sum, x=year)) +
  geom_line(lwd=1.5, color="black") +
  xlab("Year of description") + ylab("Cumulative number of descriptions") +
  scale_x_continuous(breaks = c(1750,1800,1850,1900,1950,2000), 
                     labels = c(1750,1800,1850,1900,1950,2000)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, size=10, hjust=1),
        axis.ticks.length.x = unit(.25, "cm"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  add_phylopic(img = crab_img, alpha=1, color="black", y = 7500, x= 1780, ysize = 0.8)
