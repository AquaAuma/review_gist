###############################################################################
#### Get the silhouette for each group
#### Coding and data processing: Aurore Maureaud
#### January 2022
################################################################################

rm(list = ls())
library(rphylopic)

# set date
date <- '6JAN2022'
# silhouette uuid are found online on this website: http://phylopic.org/
# uuid in the webpage url

# get silhouettes & save as png files
crab_uuid <- image_get(uuid = "01dd976b-f6e9-4204-bae1-c15a32234f73")
crab_img <- image_data(crab_uuid$uid, size = "512")[[1]]
save_png(crab_img, target = "figures/silhouette_crabs.png")

dragon_uuid <- image_get(uuid = "8af9c80d-92f9-4ba8-87c0-8ffa026c770c")
dragon_img <- image_data(dragon_uuid$uid, size="512")[[1]]
save_png(dragon_img, target = "figures/silhouette_odonates.png")

butter_uuid <- image_get(uuid = "ab6182d2-5093-444b-92e5-84468218ebf0")
butter_img <- image_data(butter_uuid$uid, size="512")[[1]]
save_png(butter_img, target = "figures/silhouette_butterflies.png")

ant_uuid <- image_get(uuid = "f4d28481-3b28-4cdb-9877-9b9d4f07e5f2")
ant_img <- image_data(ant_uuid$uid, size="512")[[1]]
save_png(ant_img, target = "figures/silhouette_ants.png")

mammal_uuid <- image_get(uuid = "baa41c61-362b-45e0-a3be-b5db2891226f")
mammal_img <- image_data(mammal_uuid, size="512")[[1]]
save_png(mammal_img, target = "figures/silhouette_mammals.png")

plant_uuid <- image_get(uuid = "39335c0c-f879-4df6-90d5-e4f65d90d04e")
plant_img <- image_data(plant_uuid, size = "512")[[1]]
save_png(plant_img, target = "figures/silhouette_plants.png")

reptile_uuid <- image_get(uuid = "7dee5849-e764-4694-abf7-d0ae4cc8cabe")
reptile_img <- image_data(reptile_uuid, size = "512")[[1]]
save_png(reptile_img, target = "figures/silhouette_reptiles.png")

birds_uuid <- image_get(uuid = "5bddf6be-2335-4ad9-9524-db7cf5d2eb1a")
birds_img <- image_data(birds_uuid, size = "512")[[1]]
save_png(birds_img, target = "figures/silhouette_birds.png")

amphi_uuid <- image_get(uuid = "79a65a4a-abf2-4bff-9410-934042affef1")
amphi_img <- image_data(amphi_uuid, size = "512")[[1]]
save_png(amphi_img, target = "figures/silhouette_amphibians.png")

fish_uuid <- image_get(uuid = "4c2220b9-68a8-4594-8dd8-229643e11c84")
fish_img <- image_data(fish_uuid, size = "512")[[1]]
save_png(fish_img, target = "figures/silhouette_fish.png")

