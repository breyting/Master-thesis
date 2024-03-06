library(sf) #Simple Features. This is the map-maker
library(tidyverse) #Collection of packages in the tidyverse (see https://www.tidyverse.org/)
library(readxl) #Package to read excel files
library(viridis)  #Nicer colors
library(ggthemes) #Nice theme for maps

shpfile <- "/home/matias/Documents/1ULB/Cours 2023-2024/Master sciences et techno de l'infocom/Mémoire/Jeux de données/Shapefile/31370" #path to file 
emptymap <- st_read(dsn = shpfile) #read the shp format into R

plot(emptymap$geometry) #plot the geometry column

gmlfile <- "/home/matias/Documents/1ULB/Cours 2023-2024/Master sciences et techno de l'infocom/Mémoire/Jeux de données/Répartition_population_par_commune/BE_SB_TF_PD_MUN_2021_all.gml"
emptymap2 <- st_read(dsn = gmlfile)

plot(emptymap2$geometry)
  #doit rajouter les données par communes pour la population
