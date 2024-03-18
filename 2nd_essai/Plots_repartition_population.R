library(sf)
library(mapview)
library(dplyr)
library(units)
library(tidyverse)

repartition_shp = st_read("Jeux_de_données/Homemade/repartition_pop_2023.shp")

#########################  CHOROPLETH MAPS  ####################################

mapview(repartition_shp, zcol = "g_18_25")
mapview(repartition_shp, zcol = "gr_3_12")
mapview(repartition_shp, zcol = "g_12_18")

repartition_shp = repartition_shp %>% select(-Shp_Lng, -Shap_Ar)


repartition_shp$area = set_units(st_area(repartition_shp), "km^2")
repartition_shp$pop_per_km2_gr_3_12 = repartition_shp$gr_3_12 / repartition_shp$area
repartition_shp$pop_per_km2_gr_12_18 = repartition_shp$g_12_18 / repartition_shp$area
repartition_shp$pop_per_km2_gr_18_25 = repartition_shp$g_18_25 / repartition_shp$area


mapview(repartition_shp, zcol = "pop_per_km2_gr_3_12")
mapview(repartition_shp, zcol = "pop_per_km2_gr_12_18")
mapview(repartition_shp, zcol = "pop_per_km2_gr_18_25")



########################  DOT MAPS #############################################

dot_density <- function(colonne) {
  num_dots <- ceiling(dplyr::select(as.data.frame(repartition_shp), colonne)/100)
  deu_dots <- map_df(
    names(num_dots),
    ~ sf::st_sample(repartition_shp, size = num_dots[, .x], type = "random") |>
      sf::st_cast("POINT") |>
      sf::st_coordinates() |>
      as_tibble() |>
      setNames(c("long", "lat"))
  )
  return(deu_dots)
}

dot_density_3_12 <- dot_density("gr_3_12")

ggplot(dot_density_3_12) +
  geom_sf(
    data = repartition_shp, fill = "transparent",
    color = "grey20", size = .1
  )+
  geom_point(
    data = dot_density_3_12, aes(x = long, y = lat),
    color = "red", size = .3, shape = 19, alpha = .2
  )


mapview(geom_sf(
  data = repartition_shp, fill = "transparent",
  color = "grey20", size = .1
))+
  mapview(geom_point(
    data = dot_density_3_12, aes(x = long, y = lat),
    color = "red", size = .3, shape = 19, alpha = .2
  ))





##############################  GRID MAP  ######################################


population_grid = st_read("Jeux_de_données/ea430cb8-2805-4c67-8392-9e2a47c1ef55_geopackage+sqlite3_3035/BE_SB_TF_PD_1km2_GRID_2021.gpkg")

mapview(population_grid, zcol = "Pourcentage_TOTAL")

#On réalise un % de la population total dans la grille
population_grid$Pourcentage_TOTAL = (population_grid$TOTAL/sum(population_grid$TOTAL, na.rm = TRUE))*100
sum(population_grid$TOTAL, na.rm = TRUE)

# On va chercher le total de la population de chaque groupe 
total_3_12 = sum(repartition_shp$gr_3_12)
total_12_18 = sum(repartition_shp$g_12_18)
total_18_25 = sum(repartition_shp$g_18_25)


# On arrondi mais on a une petite 
population_grid$group_3_12 = round((total_3_12 * population_grid$Pourcentage_TOTAL) /100)
population_grid$group_12_18 = round((total_12_18 * population_grid$Pourcentage_TOTAL) /100)
population_grid$group_18_25 = round((total_18_25 * population_grid$Pourcentage_TOTAL) /100)

# ajout de 3 pour le group 3_12
sum(population_grid$group_3_12, na.rm = TRUE)
# diminution de 155 group 12_18
sum(population_grid$group_12_18, na.rm = TRUE)
# diminution de 21 group 18_25
sum(population_grid$group_18_25, na.rm = TRUE)


mapview(population_grid, zcol = "group_3_12")
mapview(population_grid, zcol = "group_12_18")
mapview(population_grid, zcol = "group_18_25")

population_grid_clean = population_grid %>% select(GRD_NEWID, group_3_12, group_12_18, group_18_25)

st_write(population_grid_clean, "Jeux_de_données/Homemade/population_grid_2023.shp")
