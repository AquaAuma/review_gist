################################################################################
#### Assessment of IUCN coverage per group
#### Coding and data processing: Aurore Maureaud & Emily Sandall
#### November 2021
################################################################################

rm(list = ls())

# set date
date <- '19NOV2021'

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
taxonomies <- read.csv("data/taxonomies_11NOV2021.csv")

