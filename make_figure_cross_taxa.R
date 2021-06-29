###############################################################################
#### Figures showing the taxonomic descriptions through time across taxa
#### Coding and data processing: Aurore Maureaud
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


################################################################################
### 1. Load data
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


################################################################################
### 3. Extract authorship year
################################################################################

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
# verify there is no NA, there are 4 odonates spp with NA and 2 brachyuran spp with NA, fixed by hand
# no problem for butterflies and ants


################################################################################
### 4.Number of descriptions / year / group
################################################################################

# make summary of data
descriptions <- taxonomies %>% 
  group_by(group, taxa, year) %>% 
  summarize(nbr_description = length(canonical))

# make figure
desc_year_plot <- ggplot() + geom_bar(data = descriptions, aes(y = nbr_description, x = as.factor(year)),
                    stat = "identity") +
  facet_wrap( ~ taxa, nrow=1) +
  xlab("Year of description") + ylab("Number of descriptions") +
  scale_x_discrete(breaks = c(1750,1800,1850,1900,1950,2000), 
                   labels = c(1750,1800,1850,1900,1950,2000)) +
  theme_bw()

# save main plot
ppi <- 300
png(paste0("figures/taxa_plot_desc_per_year_",date,".png"),
    width = 20*ppi, height = 7.5*ppi, res=ppi)
print(desc_year_plot)
dev.off()




################################################################################
### 5.Cumulative descriptions / year / group
################################################################################

# make summary of data with yearly accumulation
cumul_desc <- taxonomies %>% 
  group_by(taxa, taxonRank, year) %>% 
  summarize(nbr_description = length(canonical)) %>% 
  mutate(temp_sum=cumsum(nbr_description)) %>% 
  filter(taxonRank == "species")
cumul_desc <- data.frame(cumul_desc)

# make figure
taxa <- unique(taxonomies$taxa)
plots_cumul <- list()

for(i in 1:length(taxa)){

    dat <- cumul_desc[cumul_desc$taxa == taxa[i],]
    
    p <- ggplot(data = dat, aes(y=temp_sum, x=year, group=taxonRank)) +
    geom_line(lwd=1.5, color="black", aes(linetype = taxonRank)) +
    xlab("Year of description") + ylab("Number of descriptions") +
    scale_x_continuous(breaks = c(1750,1800,1850,1900,1950,2000), 
                       labels = c(1750,1800,1850,1900,1950,2000)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, size=25, hjust=1),
          axis.ticks.length.x = unit(.25, "cm"),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          text = element_text(size=25),
          axis.text = element_text(size=25),
          legend.position = "none") 
    
    # add silhouette
    silhouette <- readPNG(paste0("figures/silhouette_",taxa[i],".png"))
    silhouette <- rasterGrob(silhouette, interpolate = TRUE)
    min_y <- max(dat$temp_sum)-0.1*max(dat$temp_sum)
    p <- p +
      annotation_custom(silhouette, ymin = min_y, xmin = 1750, xmax = 1800)
    
    if(i !=1){p <- p + ylab("")}
    if(i %in% c(1,2,3)){p <- p + xlab("") + theme(axis.text.x = element_blank())}
    if(i==4){p <- p + xlab("")}
    
    plots_cumul[[length(plots_cumul)+1]] <- p
    rm(p)
}

# save main plot
ppi <- 300
png(paste0("figures/taxa_plot_",date,".png"),
    width = 20*ppi, height = 10*ppi, res=ppi)
print(ggarrange(plots = plots_cumul, nrow=2, ncol=3))
dev.off()
