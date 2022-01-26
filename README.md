## Why taxonomy has a much larger impact on biodiversity and conservation science than you think

- Code to produce figures for the manuscript
- Contributors to analyses: A.A. Maureaud, M. Lucas, Y. Sica, E.L. Sandall

## Code

- *count_names.R*: create the master taxonomy file for all MOL group taxonomies, including dragonflies, crabs, ants, plants, mammals, reptiles, amphibians, birds, butterflies
- *get_silhouettes.R*: for each taxonomic group, download and store a silhouette use for Figure 2 from the rphylopic package
- *count_names.R*: get simple summary of number of names per taxonomic group
- *taxonomic_completeness.R*: assess number of names added to master taxonomies on the last 2 decades
- *col_coverage.R*: assess taxonomic match with Catalogue of Life
- *gbif_coverage.R*: assess taxonomic match and data gaps with GBIF
- *griis_coverage.R*: assess taxonomic mismatch with GRIIS
- *iucn_coverage.R*: assess taxonomic match and data gaps with IUCN taxonomy and red list
- *ncbi_coverage.R*: assess taxonomic match and genetic data coverage with NCBI
- *tree_coverage.R*: assess taxonomic match and phylogenetic affiliation with TreeOfLife

## Results
- Tables with coverage and data gaps results per taxonomic group and database (either COL, GBIF, GRIIS, IUCM, NCBI, TreeOfLife).
- Table with completeness results
- Table with number of names

## Figures
- All silhouettes dowloaded from rphylopic
- Temporal addition of accepted names per taxonomic groups use for taxonomic completeness