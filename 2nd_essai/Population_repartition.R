library(sf)
library(mapview)
library(openxlsx)


repartition_belgique_gpkg = st_read("Jeux_de_données/Répartition_population_par_commune/BE_SB_TF_PD_MUN2021.gpkg")

structure_population_age = read.xlsx("Jeux_de_données/Répartition_population_par_commune/TF_SOC_POP_STRUCT_2023.xlsx")


#enlève les infos inutiles pour ne garder que le nom de la commune, l'age et le nombre de personne de cette age
data_grouped <- structure_population_age %>%
  group_by(TX_DESCR_FR, CD_AGE) %>%
  summarise(total_MS_POPULATION = sum(MS_POPULATION))

head(data_grouped)

# ensuite, pour chaque commune, on additionne le nombres de personne selon les tranches d'ages estimées pertinentes.
data_grouped_2 <- data_grouped %>%
  mutate(
    group_3_12 = sum(ifelse(CD_AGE >= 3 & CD_AGE <= 12, total_MS_POPULATION, 0)),
    group_12_18 = sum(ifelse(CD_AGE > 12 & CD_AGE <= 18, total_MS_POPULATION, 0)),
    group_18_25 = sum(ifelse(CD_AGE > 18 & CD_AGE <= 25, total_MS_POPULATION, 0))
  ) %>%
  distinct(TX_DESCR_FR, .keep_all = TRUE) %>%
  select(-CD_AGE, -total_MS_POPULATION)

# pour l'historique des problèmes
#data_grouped_2$TX_DESCR_FR = toupper(data_grouped_2$TX_DESCR_FR)
data_grouped_2$TX_DESCR_FR = stringi::stri_trans_general(toupper(data_grouped_2$TX_DESCR_FR), "Latin-ASCII")

head(data_grouped_2)

repartition_population_Belgique_tranche_age <- dplyr::left_join(repartition_belgique_gpkg, data_grouped_2, by = c("T_MUN_FR" = "TX_DESCR_FR"))

# clean le doc pour faciliter l'utilisation
repartition_population_Belgique_tranche_age = repartition_population_Belgique_tranche_age %>% select(REFNIS, T_MUN_FR, T_MUN_NL, Shape_Leng, Shape_Area, group_3_12, group_18_25, group_12_18, geom)

# Test de débugage
table(is.na(repartition_population_Belgique_tranche_age$group_12_18))
sort(repartition_population_Belgique_tranche_age$T_MUN_FR[is.na(repartition_population_Belgique_tranche_age$group_12_18)])
data_grouped_2$TX_DESCR_FR[!(data_grouped_2$TX_DESCR_FR %in% repartition_population_Belgique_tranche_age$T_MUN_FR)]

view(subset(repartition_population_Belgique_tranche_age, is.na(repartition_population_Belgique_tranche_age$group_3_12)))
view(subset(data_grouped_2, !(data_grouped_2$TX_DESCR_FR %in% repartition_population_Belgique_tranche_age$T_MUN_FR)))



# Compléter les 20 derniers cas particuliers

cas_particulier = function(nom_polygon, nom_age){
  repartition_population_Belgique_tranche_age$group_3_12[repartition_population_Belgique_tranche_age$T_MUN_FR == nom_polygon] <<- data_grouped_2$group_3_12[data_grouped_2$TX_DESCR_FR == nom_age]
  repartition_population_Belgique_tranche_age$group_12_18[repartition_population_Belgique_tranche_age$T_MUN_FR == nom_polygon] <<- data_grouped_2$group_12_18[data_grouped_2$TX_DESCR_FR == nom_age]
  repartition_population_Belgique_tranche_age$group_18_25[repartition_population_Belgique_tranche_age$T_MUN_FR == nom_polygon] <<- data_grouped_2$group_18_25[data_grouped_2$TX_DESCR_FR == nom_age]
  
}
  
cas_particulier("ALOST", "ALOST (ALOST)")  
cas_particulier("BEVEREN", "BEVEREN (SAINT-NICOLAS)")
cas_particulier("CELLES", "CELLES (TOURNAI)")
cas_particulier("FOREST", "FOREST (BRUXELLES-CAPITALE)")
cas_particulier("HAL", "HAL (HAL-VILVORDE)")
cas_particulier("HAMME", "HAMME (TERMONDE)")
cas_particulier("HOVE", "HOVE (ANVERS)")
cas_particulier("KAPELLEN", "KAPELLEN (ANVERS)")
cas_particulier("LE COQ", "DE HAAN")
cas_particulier("MACHELEN", "MACHELEN (HAL-VILVORDE)")
cas_particulier("MOERBEKE", "MOERBEKE (GAND)")
cas_particulier("NEUFCHATEAU", "NEUFCHATEAU (NEUFCHATEAU)")
cas_particulier("NIEUWERKERKEN", "NIEUWERKERKEN (HASSELT)")
cas_particulier("PERWEZ", "PERWEZ (NIVELLES)")
cas_particulier("SAINT-LEGER", "SAINT-LEGER (VIRTON)")
cas_particulier("TIELT", "TIELT (TIELT)")
cas_particulier("WAVRE-SAINTE-CATHERINE", "SINT-KATELIJNE-WAVER")
cas_particulier("ZWALM", "ZWALIN")

cas_particulier_NL = function(nom_polygon, nom_age){
  repartition_population_Belgique_tranche_age$group_3_12[repartition_population_Belgique_tranche_age$T_MUN_NL == nom_polygon] <<- data_grouped_2$group_3_12[data_grouped_2$TX_DESCR_FR == nom_age]
  repartition_population_Belgique_tranche_age$group_12_18[repartition_population_Belgique_tranche_age$T_MUN_NL == nom_polygon] <<- data_grouped_2$group_12_18[data_grouped_2$TX_DESCR_FR == nom_age]
  repartition_population_Belgique_tranche_age$group_18_25[repartition_population_Belgique_tranche_age$T_MUN_NL == nom_polygon] <<- data_grouped_2$group_18_25[data_grouped_2$TX_DESCR_FR == nom_age]
}

cas_particulier_NL("SINT-NIKLAAS", "SAINT-NICOLAS (SAINT-NICOLAS)")
cas_particulier_NL("SAINT-NICOLAS", "SAINT-NICOLAS (LIEGE)")


# écrire tout dans un nouveau shapefile
st_write(repartition_population_Belgique_tranche_age, "Jeux_de_données/Homemade/repartition_pop_2023.shp", delete_dsn = TRUE)

repartition_shp = st_read("Jeux_de_données/Homemade/repartition_pop_2023.shp")
mapview(repartition_shp, zcol = "g_18_25")


