################################################################################
#### Assessment names from MOL taxonomies and make taxonomies file
#### Coding and data processing: Aurore Maureaud
#### May 2022
################################################################################

rm(list = ls())

# set date
date <- '16MAY2022'

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
taxonomies <- read.csv("data/taxonomies_16MAY2022.csv") %>% 
  mutate(year = as.numeric(str_extract(authorship, "[0-9]+")),
         year = ifelse(year<1693 | year>2021 & length(str_extract_all(authorship,"[0-9]+")[[1]])>1,
                       str_extract_all(authorship, "[0-9]+")[[1]][2],year),
         year = ifelse(year<1693 | year>2021 & length(str_extract_all(authorship,"[0-9]+")[[1]])>2,
                       str_extract_all(authorship,"[0-9]+")[[1]][3],year),
         year = ifelse(year<1693, NA, year),
         year = ifelse(year>2021, NA, year))


problems <- taxonomies %>% 
  filter(year<1693 | year>2021)
write.csv(problems, file = "data/problems_year.csv", row.names = FALSE)

missing_year <- taxonomies %>% 
  filter(is.na(year),
         accid ==0) %>% 
  group_by(group) %>% 
  summarize(missing_year = length(authorship))


################################################################################
### 1. Completeness assessment
################################################################################

# a. total number of accepted species names
spp_nbr <- taxonomies %>% 
  filter(accid == 0,
         is.na(subspecies)) %>% # select species only
  group_by(group) %>% 
  summarize(spp_nbr = length(canonical))

# b. total number of species since 2000
spp_nbr_2000 <- taxonomies %>% 
  filter(year>2000,
         accid == 0,
         is.na(subspecies)) %>% # select species only
  group_by(group) %>% 
  summarize(spp_nbr_2000 = length(canonical))

# c. total number of species since 2010
spp_nbr_2010 <- taxonomies %>% 
  filter(year>2010,
         accid == 0,
         is.na(subspecies)) %>% # select species only
  group_by(group) %>% 
  summarize(spp_nbr_2010 = length(canonical))

completeness <- left_join(spp_nbr, spp_nbr_2000, by="group")
completeness <- left_join(completeness, spp_nbr_2010, by="group") %>% 
  mutate(completeness_2dec = round(spp_nbr_2000/spp_nbr*100,0),
         completeness_1dec = round(spp_nbr_2010/spp_nbr*100,0))



################################################################################
#### 3. SUMMARIZE INFORMATION
################################################################################

write.csv(completeness, 
          file = paste0("results/completeness_",date,".csv"),
          row.names = F)

