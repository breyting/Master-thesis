library(sf)
library(mapview)
library(dplyr)
library(units)
library(tidyverse)

ecole_wallonie = st_read('Jeux_de_données/Signaletique-fase-shapefile/signaletique-fase.shp', quiet = TRUE)
ecole_flandre = st_read('Jeux_de_données/Onderwijsaanbod_in_Vlaanderen_en_Brussel_via_POI_service/Shapefile/POI_Onderwijs.shp', quiet = TRUE)
frontiere_Belgique = st_read("Jeux_de_données/Frontière belgique/belgianterritory_4326.shp", quiet = TRUE)

ecole_wallonie_BD72 = st_transform(ecole_wallonie, 31370)
ecole_flandre = st_transform(ecole_flandre, 31370)

frontiere_Belgique_BD72 = st_transform(frontiere_Belgique, 31370)
frontiere_Belgique_BD72 = st_zm(frontiere_Belgique_BD72, drop = TRUE, what = "ZM")

ecole_wallonie_BD72_secondaire = ecole_wallonie_BD72[ecole_wallonie_BD72$niveau == "Secondaire", ]
ecole_flandre_secondaire = ecole_flandre[ecole_flandre$CATEGORIE == "Secundair onderwijs",]

ecole_wallonie_BD72_secondaire_buffer_20 = st_buffer(ecole_wallonie_BD72_secondaire, dist =  set_units(20, 'km'))
ecole_flandre_secondaire_buffer_20 = st_buffer(ecole_flandre_secondaire, dist = set_units(20, 'km'))

mapview(ecole_flandre_secondaire_buffer_20, col.region = 'transparent')+
  mapview(ecole_wallonie_BD72_secondaire_buffer_20, col.region = 'transparent')

ecole_wallonie_BD72_secondaire_buffer_20_dans_belgique = st_intersection(ecole_wallonie_BD72_secondaire_buffer_20, st_geometry(frontiere_Belgique_BD72))
ecole_flandre_secondaire_buffer_20_dans_belgique = st_intersection(ecole_flandre_secondaire_buffer_20, st_geometry(frontiere_Belgique_BD72))


mapview(ecole_flandre_secondaire_buffer_20_dans_belgique, col.region = 'transparent')+
mapview(ecole_wallonie_BD72_secondaire_buffer_20_dans_belgique, col.region = 'transparent')

#intersection_flandre_buffer = st_intersection(ecole_flandre_secondaire_buffer_20_dans_belgique, ecole_flandre_secondaire_buffer_20_dans_belgique)



