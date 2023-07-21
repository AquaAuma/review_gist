## A Global Integrated Structure of Taxonomy supporting biodiversity science and conservation

OSF related project and underlying data [![DOI](https://osf.io/9rptq/)](https://osf.io/9rptq/)

- Code to produce analysis of Box 1 and 2
- Contributors to analyses: A.A. Maureaud, M. Lucas, Y.V. Sica, E.L. Sandall
- Contributors to MOL taxonomies: Y.V. Sica, M.S Rogan, D.B. Booher, R. Edwards, M. Lucas, S. Pinkert, A. Ranipeta, A.A. Maureaud, E.L. Sandall

## Data
- `MOL/`: taxonomies from Map of Life (MOL) https://mol.org/
- `COL/`: downloaded data from Catalogue of Life (COL) for selected groups https://www.catalogueoflife.org/
- `GBIF/`: downloaded data from the Global Biodiversity Information Facility (GBIF) https://www.gbif.org/
- `GRIIS/`: downloaded data from the Global Register of Introduced and Invasive Species (GRIIS) https://griis.org/
- `IUCN/`: downloaded data from the International Union for the Conservation of Nature (IUCN) https://www.iucnredlist.org/
- `NCBI/`: downloaded data from the National Center for Biotechnology Information https://www.ncbi.nlm.nih.gov/
- `OpenTreePhylogeny`: downloaded data from the Tree Of Life http://www.onezoom.org/
- `taxonomies.csv`: outputs from *count_names.R*

## Metadata
Description of the column names in the MOL taxonomy files:
- `id` unique name id per `group`
- `accid` value is 0 is name is considered accepted, `id` if synonym (or alternative name)
- `canonical` combination of `genus`, `species`, and `subspecies` if relevant
- `order` follows DwC standard terms (https://dwc.tdwg.org/terms/)
- `family` follows DwC standard terms
- `genus` follows DwC standard terms
- `species` follows DwC standard terms
- `subspecies` correspond to `infraspecificEpithet` according to DwC standard terms
- `group` taxonomic group considered
- `authorship` correspond to `scientificNameAuthorship` according to DwC standard terms
- `source` data source used to access the names

## Code
- *count_names.R*: create the taxonomy file for all MOL groups, including dragonflies, crabs, ants, daisies, mammals, reptiles, amphibians, birds, butterflies, and get simple summary of number of names per taxonomic group. Methods for compiling taxonomies are available in Text S1 and Table S3.
- *get_silhouettes.R*: for each taxonomic group, download and store a silhouette use for Box 1 and 2 from the rphylopic package http://phylopic.org/
- *taxonomic_completeness.R*: assess number of names added to master taxonomies on the last 2 decades
- *col_coverage.R*: assess taxonomic match with Catalogue of Life
- *gbif_coverage.R*: assess taxonomic match and data gaps with GBIF
- *griis_coverage.R*: assess taxonomic mismatch with GRIIS
- *iucn_coverage.R*: assess taxonomic match and data gaps with IUCN taxonomy and red list
- *ncbi_coverage.R*: assess taxonomic match and genetic data coverage with NCBI
- *tree_coverage.R*: assess taxonomic match and phylogenetic affiliation with TreeOfLife

## Results
- `match_XX_results.csv`: tables with coverage and data gaps results per taxonomic group and database (either COL, GBIF, GRIIS, IUCN, NCBI, TreeOfLife)
- `completeness.csv`: table with completeness results
- `numbers.csv`: table with number of names
- `matching_results.csv`: table with matching results from all data sources, grading detailed in Table S5 and Text S2
- `matching_results_scores.csv`: table with matching results from all data sources and associated scores, grading detailed in Table S5

## Figures
- All silhouettes downloaded from rphylopic
