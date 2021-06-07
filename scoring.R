###############################################################################
#### Figures showing the taxonomic descriptions through time across taxa
#### Coding and data processing: Aurore Maureaud
#### June 2021
################################################################################

rm(list = ls())

# set date
date <- '7JUN2021'

# libraries
library(ggplot2)
library(tidyverse)
library(readxl)
library(rphylopic)
library(egg)
library(grid)
library(png)
library(writexl)


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
                          TRUE ~ NA_character_))
sort(unique(taxonomies$year)) # problem with years for 5 odonate spp
summary(taxonomies$year) 


################################################################################
### 3. Load silhouettes
################################################################################


mammal_img <- readPNG("figures/silhouette_mammals.png")
ant_img <- readPNG("figures/silhouette_ants.png")
butter_img <- readPNG("figures/silhouette_butterflies.png")
dragon_img <- readPNG("figures/silhouette_odonates.png")
crab_img <- readPNG("figures/silhouette_crabs.png")


################################################################################
### 4. Load scores
################################################################################

scores <- read_excel("data/scoring.xlsx")

nb_spp <- taxonomies %>% 
  group_by(taxa) %>% 
  summarise(nbr_spp = length(canonical))

scores <- data.frame(left_join(scores, nb_spp, by=c("group"="taxa")))

level_order <-c("family","infraorder","suborder","order","class")

scores2 <- scores %>% 
  filter(!is.na(nbr_spp)) %>% 
  mutate(level = as.factor(level),
         realm = as.factor(realm))

plot_scoring <- ggplot(scores2) + geom_point(aes(x = total, y = fct_reorder(level,level_order,.desc=TRUE), 
                                                 size = nbr_spp, color = realm), alpha=0.8) +
  theme_bw() +
  #scale_y_discrete(limits = level_order) +
  scale_color_manual(values = c("lightblue","darkseagreen")) +
  scale_size_continuous(range = c(5,30)) +
  ylab("Taxonomic level") + xlab("Score") +
  geom_text(data = scores2, aes(x = total, y = fct_reorder(level,level_order,.desc=TRUE),
                label = group), size=5) +
  scale_x_continuous(limits = c(0,8)) +
  theme(text = element_text(size=25),
        axis.text = element_text(size=25))

# taxa <- unique(taxonomies$taxa)
# for(i in 1:length(taxa)){
#   # add silhouette
#   silhouette <- readPNG(paste0("figures/silhouette_",taxa[i],".png"))
#   silhouette <- rasterGrob(silhouette, interpolate = TRUE)
#   scoring_plot <- scoring_plot +
#     annotation_custom(silhouette)
# }

# save plot
ppi <- 300
png("figures/scoring_plot.png", width = 15*ppi, height = 10*ppi, res=ppi)
print(plot_scoring)
dev.off()
