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




ecole_wallonie_BD72_secondaire_buffer_20 = st_buffer(ecole_wallonie_BD72_secondaire, dist =  set_units(10, 'km'))
ecole_flandre_secondaire_buffer_20 = st_buffer(ecole_flandre_secondaire, dist = set_units(10, 'km'))

mapview(ecole_flandre_secondaire_buffer_20, col.region = 'transparent')+
  mapview(ecole_wallonie_BD72_secondaire_buffer_20, col.region = 'transparent')

ecole_wallonie_BD72_secondaire_buffer_20_dans_belgique = st_intersection(ecole_wallonie_BD72_secondaire_buffer_20, st_geometry(frontiere_Belgique_BD72))
ecole_flandre_secondaire_buffer_20_dans_belgique = st_intersection(ecole_flandre_secondaire_buffer_20, st_geometry(frontiere_Belgique_BD72))


mapview(ecole_flandre_secondaire_buffer_20_dans_belgique, col.region = 'transparent')+
mapview(ecole_wallonie_BD72_secondaire_buffer_20_dans_belgique, col.region = 'transparent')

intersection_flandre_buffer = st_intersects(ecole_flandre_secondaire_buffer_20_dans_belgique, ecole_flandre_secondaire_buffer_20_dans_belgique)

#intersection_flandre_buffer_geom = st_intersection(ecole_flandre_secondaire_buffer_20_dans_belgique, ecole_flandre_secondaire_buffer_20_dans_belgique)

#st_write(st_geometry(intersection_flandre_buffer_geom), 'Jeux_de_données/Homemade_test/intersection_flandre_buffer_geom.shp', delete_dsn = TRUE)
#view(st_read("Jeux_de_données/Homemade_test/intersection_flandre_buffer_geom2.shp"))

overlaps_buffer = st_overlaps(ecole_flandre_secondaire_buffer_20_dans_belgique)










# test plus petit pour comparaison 
ecole_wallonie_BD72_secondaire_buffer_5 = st_buffer(ecole_wallonie_BD72_secondaire, dist =  set_units(5, 'km'))
ecole_flandre_secondaire_buffer_5 = st_buffer(ecole_flandre_secondaire, dist = set_units(5, 'km'))

mapview(ecole_flandre_secondaire_buffer_5, col.region = 'transparent')+
  mapview(ecole_wallonie_BD72_secondaire_buffer_5, col.region = 'transparent')

ecole_wallonie_BD72_secondaire_buffer_5_dans_belgique = st_intersection(ecole_wallonie_BD72_secondaire_buffer_5, st_geometry(frontiere_Belgique_BD72))
ecole_flandre_secondaire_buffer_5_dans_belgique = st_intersection(ecole_flandre_secondaire_buffer_5, st_geometry(frontiere_Belgique_BD72))

overlaps_buffer_flandre_5 = st_overlaps(ecole_flandre_secondaire_buffer_5_dans_belgique)

segmentize_buffer_fladnre_5 = st_segmentize(ecole_flandre_secondaire_buffer_5_dans_belgique)

  (ecole_flandre_secondaire_buffer_5_dans_belgique)
mapview(unary_unions_buffer_flandre_5)
polygonize_buffer_flandre_5 = geos::geos_polygonize(unary_unions_buffer_flandre_5)







# faire à l'envers. On fait une grille qui représente plusieurs points, et on va voir s'il y a des écoles dans la zone de x km.
# on compte le nombre d'école et on a le nombre d'école qui touche cette zone. Répéter sur les différents set de données.

population_grid = st_transform(st_read("Jeux_de_données/Homemade/population_grid_2023.shp"), 31370)

mapview(population_grid_within_5km)

population_grid_within_5km = st_is_within_distance(population_grid, ecole_flandre_secondaire, set_units(5, 'km'))
population_grid_within_5km_wallonie = st_is_within_distance(population_grid, ecole_wallonie_BD72_secondaire, set_units(5, 'km'))

for (ligne in 1:nrow(population_grid)) {
  population_grid$nbr_ecole_5km[ligne] = length(population_grid_within_5km[[ligne]]) + length(population_grid_within_5km_wallonie[[ligne]])
}

mapview(population_grid, zcol = "nbr_ecole_5km")


# pareil mais avec 10km

population_grid_within_10km_flandre_secondaire = st_is_within_distance(population_grid, ecole_flandre_secondaire, set_units(10, 'km'))
population_grid_within_10km_wallonie_secondaire = st_is_within_distance(population_grid, ecole_wallonie_BD72_secondaire, set_units(10, 'km'))

for (ligne in 1:nrow(population_grid)) {
  population_grid$nbr_ecole_10km_sec[ligne] = length(population_grid_within_10km_flandre_secondaire[[ligne]]) + length(population_grid_within_10km_wallonie_secondaire[[ligne]])
}

mapview(population_grid, zcol = "nbr_ecole_10km_sec")



# calcule de densité par école de la population

for (ligne in 1:nrow(population_grid)) {
  if (!is.na(population_grid$g_12_18[ligne]) & (population_grid$nbr_ecole_10km_sec[ligne] != 0)) {
      population_grid$nbr_eleve_par_ecole_sec_10km[ligne] = round(population_grid$g_12_18[ligne] / population_grid$nbr_ecole_10km_sec[ligne])
  } else {
    population_grid$nbr_eleve_par_ecole_sec_10km[ligne] = NA
  }
}

mapview(population_grid, zcol = "nbr_eleve_par_ecole_sec_10km")
