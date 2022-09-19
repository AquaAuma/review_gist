## A Global Integrated Structure of Taxonomy supporting biodiversity

- Code to produce analysis of Box 1 and 2
- Contributors to analyses: A.A. Maureaud, M. Lucas, Y.V. Sica, E.L. Sandall
- Contributors to MOL taxonomies: Y.V. Sica, M.S Rogan, D.B. Booher, R. Edwards, M. Lucas, S. Pinkert, A. Ranipeta, A.A. Maureaud, E.L. Sandall

## Code

- *count_names.R*: create the taxonomy file for all MOL groups, including dragonflies, crabs, ants, daisies, mammals, reptiles, amphibians, birds, butterflies, and get simple summary of number of names per taxonomic group. Methods for compiling taxonomies are available in Text S1 and Table S3.
- *get_silhouettes.R*: for each taxonomic group, download and store a silhouette use for Box 1 and 2 from the rphylopic package http://phylopic.org/
- *taxonomic_completeness.R*: assess number of names added to master taxonomies on the last 2 decades
- *col_coverage.R*: assess taxonomic match with Catalogue of Life https://www.catalogueoflife.org/
- *gbif_coverage.R*: assess taxonomic match and data gaps with GBIF https://www.gbif.org/
- *griis_coverage.R*: assess taxonomic mismatch with GRIIS https://griis.org/
- *iucn_coverage.R*: assess taxonomic match and data gaps with IUCN taxonomy and red list https://www.iucnredlist.org/
- *ncbi_coverage.R*: assess taxonomic match and genetic data coverage with NCBI https://www.ncbi.nlm.nih.gov/
- *tree_coverage.R*: assess taxonomic match and phylogenetic affiliation with TreeOfLife http://www.onezoom.org/

## Results
- `match_XX`: tables with coverage and data gaps results per taxonomic group and database (either COL, GBIF, GRIIS, IUCN, NCBI, TreeOfLife)
- `completeness`: table with completeness results
- `numbers`: table with number of names
- `matching_results.csv`: table with matching results from all data sources, detailed in Table S5 and Text S2
- `matching_results_scores.csv`: table with matching results from all data sources and associated scores, detailed in Table S5

## Figures
- All silhouettes downloaded from rphylopic