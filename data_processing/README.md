# Data synthesis and processing

## Higher ranking selection
For each of the nine taxonomic groups considered at MOL, there is a corresponding highest taxonomic rank. We used that highest taxonomic tank to retrieve names from the following sources, unless that rank is not recognized in the data source, in which case we used a combination of lower ranks.

## Global species list
COL is a biodiversity data aggregator with the more recent aim of proposing one species of the world list (COL, https://www.catalogueoflife.org/). COL data were accessed per taxonomic group in December 2021 [maybe explain how]

## Genetics
The genetic data hosted by the National Center for Biotechnology Information (NCBI, https://www.ncbi.nlm.nih.gov/) offers the most public genetic information across taxonomic groups worldwide. The NCBI Taxonomy comprises names and ranks for all organisms represented by genetic sequence data within the NCBI database. We downloaded the relevant taxonomies in November 2021. To do this, we downloaded the entire taxonomy for the taxonomic id number of the highest taxonomic rank encompassing each of our nine groups.

## Phylogenetics
The phylogenetic tree hosted by OneZoom on their Tree of Life Explorer (http://www.onezoom.org/) covers all taxa selected for the analysis. We downloaded v.3.3 of this taxonomy (Open Tree of Life reference taxonomy version 3.3).

## Global spatial data
The Global Biodiversity Information Facility was selected to reflect spatial occurrence point data (GBIF, https://www.gbif.org/). For each taxonomic group, we queried a list of species names with occurrence data. This search excluded fossil species. [add date]

## Invasive species
To evaluate the match between our taxonomies and the list of invasive species, we used the most comprehensive data from the Global Register of Introduced and Invasive Species (GRIIS, http://www.griis.org/about.php). GRIIS was curated using the GBIF taxonomic backbone and consists of country lists detailing names of invasive and introduced species. We worked with the entire database downloaded from the GRIIS website.

## Species theat status
The species information gathered and distributed by the IUCN Red List of Threatened Species (https://www.iucnredlist.org/) is widely used in conservation science and practice. For each taxonomic group, we downloaded the IUCN names existing in the website by using the online query system in the `Taxonomy` section.