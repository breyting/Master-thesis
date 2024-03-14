library(sf)
library(mapview)
library(dplyr)
library(units)


repartition_shp = st_read("Jeux_de_données/Homemade/repartition_pop_2023.shp")

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
