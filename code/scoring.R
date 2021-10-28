###############################################################################
#### Figures showing the taxonomic descriptions through time across taxa
#### Coding and data processing: Aurore A. Maureaud
#### June 2021
################################################################################

rm(list = ls())

# set date
date <- '29JUN2021'

# libraries
library(ggplot2)
library(tidyverse)
library(readxl)
library(rphylopic)
library(egg)
library(grid)
library(png)
library(writexl)
library(readr)
library(RColorBrewer)
library(egg)


################################################################################
### 1. Load taxo data
################################################################################

# Dragonflies
# /!\ 4 species had authorship problems with missing part of it with the year and had to be corrected by hand
# /!\ one species was present twice and was removed
# /!\ Paracercion ambiguum Kompier & Yu in Ning, Kompier, Yu & Bu, 2016
# Palaiargia susannae Kovács & Theischinger in Kovács, Theischinger, Juhász & Danyik, 2015
# Drepanosticta batanta Kovács & Theischinger in Kovács, Theischinger, Juhász & Danyik, 2015
# Diplacina olahi Theischinger & Kovács in Kovács, Theischinger, Juhász & Danyik, 2015

odonata <- read_excel("data/Odonata.xlsx")
unique(odonata$accid) #check accid, should be 0
unique(odonata$taxonRank) # check rank, should be species/subspecies or alike
unique(odonata$status) # check status, should be accepted

# Crabs
# /!\ 2 species with missing years that had to be re-entered manually:
# /!\ Arcotheres ocularius & Cymonomus tesseris
anomura <- read_excel("data/Anomura.xlsx")
unique(anomura$accid) 
unique(anomura$taxonRank)
unique(anomura$status)
brachyura <- read_excel("data/Brachyura.xlsx")
unique(brachyura$accid)
unique(brachyura$taxonRank)
unique(brachyura$status)

# Butterlfies
lepidoptera <- read.csv("data/SpList_Lepi(overview,110521).csv") %>% 
  mutate(taxonRank = case_when(!is.na(Subspecies) ~ "subspecies",
                               is.na(Subspecies) ~ "species"),
         group = "butterflies",
         status = "accepted",
         accid = 0) %>%
  filter(!is.na(Year)) %>% # 6 few species don't have authorship, so they should be removed for now, then checked with Stefan
  rename(canonical = ValidBinomial,
         authorship = Year,
         id = X) %>% 
  select(accid,canonical,taxonRank,status,authorship,id,group)

unique(lepidoptera$taxonRank)
unique(lepidoptera$accid)
unique(lepidoptera$status) # assumed accepted because not specified in file provided

# save correct excel file
#write_xlsx(lepidoptera, path = "data/Lepidoptera.xlsx")

# Ants
formicidae <- read_excel("data/Formicidae.xlsx")

unique(formicidae$taxonRank)
unique(formicidae$accid)
unique(formicidae$status) # assumed accepted because not specified in file provided

# Terrestrial mammals
mammalia <- read_excel("data/mammals_MDD_May2021.xlsx") %>% 
  select(accid, canonical, taxonRank, status, authorship, id, group)

unique(mammalia$taxonRank)
unique(mammalia$accid)
unique(mammalia$status) # all ok
length(unique(mammalia$canonical)) # all ok


# Plants
tracheophyta <- read_delim(file = "data/Vascular.Plants.Master.Taxonomyv5.5-10.06.2021.txt",
                     delim = "|") %>% 
  rename(canonical = Accepted_Name,
         taxonRank = Accepted_Taxon_level,
         authorship = Publication_Year,
         status = Status) %>% 
  mutate(group = "plants",
         taxonRank = ifelse(taxonRank == "Hybrid", "hybrid", taxonRank),
         accid = 0,
         id = NA_integer_) %>% 
  filter(!is.na(authorship),
         authorship > 1700, # there is one year that is 188, should be a mistake
         authorship < 2022, # there are years higher than 2022, up to 9192
         status == "accepted") %>% 
  select(accid, canonical, taxonRank, status, authorship, id, group) %>% 
  distinct()


################################################################################
### 2. Merge taxonomies
################################################################################

identical(names(odonata),names(brachyura))
taxonomies <- rbind(odonata, brachyura)
identical(names(taxonomies),names(anomura))
taxonomies <- rbind(taxonomies, anomura)
identical(names(taxonomies),names(lepidoptera))
taxonomies <- rbind(taxonomies,lepidoptera)
identical(names(taxonomies),names(formicidae))
taxonomies <- rbind(taxonomies,formicidae)
identical(names(taxonomies),names(mammalia))
taxonomies <- rbind(taxonomies, mammalia)
identical(names(taxonomies),names(tracheophyta))
taxonomies <- rbind(taxonomies, tracheophyta)
unique(taxonomies$group)

taxonomies <- taxonomies %>% 
  mutate(year = str_extract(authorship, "[0-9]+"),
         year = as.numeric(year),
         taxa = group,
         taxa = case_when(group %in% c("anomurans","brachyurans") ~ "crabs",
                          group == "odonates" ~ "dragonflies",
                          group == "butterflies" ~ "butterflies",
                          group == "ants" ~ "ants",
                          group == "mammals" ~ "mammals",
                          group == "plants" ~ "plants",
                          TRUE ~ NA_character_))
sort(unique(taxonomies$year))
summary(taxonomies$year) 


################################################################################
### 3. Load silhouettes
################################################################################


mammal_img <- readPNG("figures/silhouette_mammals.png")
ant_img <- readPNG("figures/silhouette_ants.png")
butter_img <- readPNG("figures/silhouette_butterflies.png")
dragon_img <- readPNG("figures/silhouette_odonates.png")
crab_img <- readPNG("figures/silhouette_crabs.png")
plant_img <- readPNG("figures/silhouette_plants.png")


################################################################################
### 4. Taxonomic completeness
################################################################################

completeness_a <- taxonomies %>% 
  filter(taxonRank == "species") %>% # select species only
  group_by(taxa) %>% 
  summarize(names_all = length(canonical))

completeness_b <- taxonomies %>% 
  filter(year>2000,
         taxonRank == "species") %>% # select species only
  group_by(taxa) %>% 
  summarize(names_2dec = length(canonical))

completeness <- left_join(completeness_a, completeness_b, by="taxa") %>% 
  mutate(completeness_2 = names_2dec/names_all)

completeness_c <- taxonomies %>% 
  filter(year>2010,
         taxonRank == "species") %>% # select species only
  group_by(taxa) %>% 
  summarize(names_dec = length(canonical))

completeness <- left_join(completeness, completeness_c, by="taxa") %>% 
  mutate(completeness_1 = names_dec/names_all)


################################################################################
### 5. Load scores
################################################################################

scores <- read_excel("data/scoring.xlsx")

# nb_spp <- taxonomies %>% 
#   group_by(taxa) %>% 
#   summarise(nbr_spp = length(canonical))
# 
# scores <- data.frame(left_join(scores, nb_spp, by=c("group"="taxa")))

level_order <-c("family","infraorder","suborder","order","class","kingdom")
# 
# scores2 <- scores %>% 
#   filter(!is.na(nbr_spp)) %>% 
#   mutate(level = as.factor(level),
#          realm = as.factor(realm))

plot_scoring <- ggplot(scores[scores$group %in% c("dragonflies","plants","ants","butterflies",
                                                  "crabs","mammals"),]) + geom_point(aes(x = total, y = level, 
                                                 size = nbr_spp, color = realm), alpha=0.8) +
  theme_bw() +
  scale_y_discrete(limits = level_order) +
  scale_color_manual(values = c("lightblue","darkseagreen")) +
  scale_size_continuous(range = c(5,50), trans = "log10") +
  ylab("Taxonomic level") + xlab("Score") +
  geom_text(aes(x = total, y = level,
                label = group), size=5) +
  scale_x_continuous(limits = c(0,8)) +
  theme(text = element_text(size=25),
        axis.text = element_text(size=25))

# save plot
ppi <- 300
png("figures/scoring_plot.png", width = 15*ppi, height = 10*ppi, res=ppi)
print(plot_scoring)
dev.off()


################################################################################
### 6. Load scores & make version 2 figures: boxplots & total scores
################################################################################
scores <- read_excel("data/scoring.xlsx") %>% 
  filter(group %in% unique(taxonomies$taxa)) %>% 
  pivot_longer(cols = 2:9, names_to = "category", values_to = "scores")

# boxplot of scores per group and per category
# x axis are the group and taxa
grades_per_taxa <- ggplot(scores) + geom_boxplot(aes(x = group, y = scores)) + 
  geom_point(aes(x = group, y = scores, color = category), 
             position=position_dodge(width = 0.45), alpha = 0.5,
             size = 8) +
  scale_color_manual(values = rainbow(8)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=24, angle = 45, vjust = 0.75, hjust = 0.75),
        axis.title = element_text(size = 28),
        axis.text.y = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24)) +
  xlab("") + ylab("")

# x axis is the category evaluated
grades_per_category <- ggplot(scores) + geom_boxplot(aes(x = category, y = scores)) + 
  geom_point(aes(x = category, y = scores, shape = group), 
             position=position_dodge(width = 0.45), alpha = 0.5,
             size = 8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=24, angle = 45, vjust = 0.75, hjust = 0.75),
        axis.title = element_text(size = 28),
        axis.text.y = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24)) +
  xlab("") + ylab("")

# save plot
ppi <- 300
png("figures/scoring_boxplots.png", width = 25*ppi, height = 15*ppi, res=ppi)
print(egg::ggarrange(grades_per_taxa, grades_per_category, 
                     nrow = 2, labels = c("","")))
dev.off()


# make summary plot of the total score
total_scores <- ggplot(scores) +
  geom_bar()
  coord_flip()
