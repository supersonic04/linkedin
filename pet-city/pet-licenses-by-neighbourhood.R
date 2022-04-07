library(leaflet)
library(sf)
library(rgdal)
library(dplyr)
library(mapview)

# importing data ----------------------------------------------------------

# unzipping neighbourhood and pet license zip files 

unzip("pet-city/City of Edmonton - Neighbourhoods.zip", exdir = "pet-city/neighbourhoods")

unzip("pet-city/Pet Licenses by Neighbourhood.zip", exdir = "pet-city/pet-licenses")

# reading neighbourhood shapefile

edm.nbr <- readOGR("pet-city/neighbourhoods/geo_export_422d038a-fd86-4b7d-8a05-031f6f0858e5.shp")

# reading pet licenses shapefile

edm.pets <- readOGR("pet-city/pet-licenses/geo_export_a6270651-b32a-42cf-81ff-bbb79a025723.shp")


# transforming data -------------------------------------------------------

# dropping geometry from the pet license data
edm.pets <- st_as_sf(edm.pets) %>% st_drop_geometry()

# renaming id column
edm.pets <- edm.pets %>% 
  rename_at(7, ~"ngh_id")

# changing id column type to numeric
edm.pets$ngh_id <- as.numeric(edm.pets$ngh_id)

# summarising pet data by neighbourhood and pet type

pet.summary <- edm.pets %>% 
  filter(year == 2021) %>%
  group_by(ngh_id, pet_type) %>% 
  summarise(count = n())

pet.summary.dominant <- pet.summary %>%
  group_by(ngh_id) %>%
  summarize(dominant_pet = pet_type[which.max(count)])

pet.summary <- inner_join(pet.summary, pet.summary.dominant)

# spreading value across columns

pet.summary <- spread(pet.summary, pet_type, count)

# finding the dominant pet type in each neighbourhood



# converting neighbourhood data from sp to sf
edm.nbr <- st_as_sf(edm.nbr)

# renaming id column
edm.nbr <- edm.nbr %>% 
  rename_at(2, ~"ngh_id")


# joing neighbourhood boundaries with license data

combined.data <- inner_join(edm.nbr, pet.summary)

# keeping name and pet type , year and month column

combined.data <- combined.data %>% 
  select(name, ngh_id, Cat, Dog, Pigeons, dominant_pet)

mapview(combined.data, zcol = "dominant_pet", layer.name = "Dominant pet in the Edmonton", col.regions = c("red", "#0095ff"))

# 