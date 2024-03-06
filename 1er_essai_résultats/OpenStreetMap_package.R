library(tidyverse)
library(OpenStreetMap)
library(ggvoronoi)
library(ggplot2)


#chargement des fichiers. /!\ le fichier flandre emet des erreurs avec les quotes encore à résoudre
wallonie <- read.csv(file = "Jeux_de_données/signaletique-fase_FWB.csv",
                     header = TRUE,
                     sep = ";")
flandre <- read.csv(file = "Jeux_de_données/Onderwijs_CSV-Flamand-bx/POI_Onderwijs.csv",
                    header = TRUE,
                    sep = ";",
                    quote = "")


#définit les coordonnées et les bornes de la carte de la Belgique
lat1 <- 49.496982; lat2 <- 51.550781; lon1 <- 2.388914; lon2 <- 6.408097
sa_map <- openmap(c(lat2, lon1), c(lat1, lon2), zoom = 9,
                  type = "esri-topo", mergeTiles = TRUE)
sa_map2 <- openproj(sa_map)


#subsets pour le secondaire
wallonie_secondaire = subset(wallonie, Niveau == "Secondaire" & Latitude != "NA")


#affichage de la carte avec les points et Voronoi pour secondaire. /!\ doit prendre les points distincts car y a des doublons dans lattitude et longitude.
wallonie_sec_distinct <- distinct(wallonie_secondaire, Latitude, Longitude, .keep_all = TRUE)

sa_map2_plt <- OpenStreetMap::autoplot.OpenStreetMap(sa_map2)+
  geom_point(data = wallonie_secondaire,
             aes(x = Longitude, y = Latitude),
             colour = "red",
             size = 1)+
  xlab("Longitude (°E)") + ylab("Latitude (°N)")+
  stat_voronoi(data = wallonie_sec_distinct, aes(x=Longitude, y=Latitude), geom = "path")

sa_map2_plt





# #code du Voronoi sans la map 
# Voronoi_wallonie <- ggplot(wallonie_sec_distinct, aes(x=Longitude, y=Latitude))+
#   geom_point(data = wallonie_secondaire,
#              aes(x = Longitude, y = Latitude),
#              colour = "red",
#              size = 1)+
#   stat_voronoi(geom = "path")+
#   xlab("Longitude (°E)") + ylab("Latitude (°S)")
# 
# Voronoi_wallonie
# 
# #Façon de trouver les valeurs NA pour les latitudes/longitudes (ou autre)
# na_rows <- wallonie[is.na(wallonie$Latitude), ]
