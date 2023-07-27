library(tidyverse)

butterflies <- read_csv("data/MOL/Copy of MOL_ButterfliesTaxonomy_v4.1.csv") %>% 
  filter(accid==0)

write.csv(butterflies, file = "data/MOL/Copy of MOL_ButterfliesTaxonomy_v4.1_no_synonyms.csv",
          row.names = F)
