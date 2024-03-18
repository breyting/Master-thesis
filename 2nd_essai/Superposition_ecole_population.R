library(sf)
library(mapview)
library(dplyr)
library(units)
library(tidyverse)
library(utils)

population_grid = st_transform(st_read("Jeux_de_données/Homemade/population_grid_2023.shp"), 31370)
repartition_pop = st_read("Jeux_de_données/Homemade/repartition_pop_2023.shp")
voronoi_belgique_secondaire = st_read("Jeux_de_données/Homemade/Voronoi_belgique_secondaire.shp")

mapview(voronoi_belgique_secondaire)
mapview(population_grid, zcol = "g_12_18")


grid_dans_voronoi = st_intersects(voronoi_belgique_secondaire, population_grid)


# ajoute la population que chaque tuile de voronoi recouvre
# /!\ attention, comment sont traité les grid qui sont recouverte 2 fois ?
for (grid in 1:length(grid_dans_voronoi)) {
  # set up la colonne
  voronoi_belgique_secondaire$group_12_18[grid] = 0
  for (tuile in grid_dans_voronoi[[grid]]) {
    # vérifie si la valeur n'est pas NA pour ne pas avoir de valeur inexistante, et donc avoir un résultat NA
    if (!is.na(population_grid$g_12_18[tuile])) {
      voronoi_belgique_secondaire$group_12_18[grid] = voronoi_belgique_secondaire$group_12_18[grid] + population_grid$g_12_18[tuile]
    }
  }
}

mapview(voronoi_belgique_secondaire, zcol = "group_12_18")



voronoi_belgique_secondaire_test = st_read("Jeux_de_données/Homemade/Voronoi_belgique_secondaire.shp")


for (grid in 1:length(grid_dans_voronoi)) {
  # set up la colonne
  voronoi_belgique_secondaire_test$group_12_18[grid] = 0
  
  for (tuile in grid_dans_voronoi[[grid]]) {
    
    # vérifie si la valeur n'est pas NA pour ne pas avoir de valeur inexistante, et donc avoir un résultat NA
    if (!is.na(population_grid$g_12_18[tuile])) {
      
      test_covered = st_covered_by(population_grid$geometry[tuile], voronoi_belgique_secondaire_test$geometry[grid])
      
      # cas grid totalement recouvert par le pavage de voronoi
      if (!is_empty(test_covered[[1]])) {
        voronoi_belgique_secondaire_test$group_12_18[grid] = voronoi_belgique_secondaire_test$group_12_18[grid] + population_grid$g_12_18[tuile]
      
      # cas où pas totalement recouvert mais en fait partie
      } else {
        # calcule l'air de la portion partagée avec le pavage
        area_intersection = st_area(st_intersection(population_grid$geometry[tuile], voronoi_belgique_secondaire_test$geometry[grid]))
        # calcule le % de la grid qui est recouvert par le pavage
        pourcentage_area_recovered = as.numeric((area_intersection/st_area(population_grid$geometry[tuile])))
        voronoi_belgique_secondaire_test$group_12_18[grid] = voronoi_belgique_secondaire_test$group_12_18[grid] + round(population_grid$g_12_18[tuile]*pourcentage_area_recovered)
      }
    }
  }
}

mapview(voronoi_belgique_secondaire_test, zcol = "group_12_18")
