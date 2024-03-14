library(sf)
library(mapview)


############### Ajout des jeux de données
ecole_wallonie = st_read('Jeux_de_données/Signaletique-fase-shapefile/signaletique-fase.shp', quiet = TRUE)
ecole_flandre = st_read('Jeux_de_données/Onderwijsaanbod_in_Vlaanderen_en_Brussel_via_POI_service/Shapefile/POI_Onderwijs.shp', quiet = TRUE)
frontiere_Belgique = st_read("Jeux_de_données/Frontière belgique/belgianterritory_4326.shp", quiet = TRUE)

############### Tests pour voir ce qu'il y a dedans
ecole_wallonie
ecole_flandre

############### Transformation du CRS pour correspondre au CRS flamand
# choisi ça car c'est une projection et c'est mieux pour calculer les distances
ecole_wallonie_BD72 = st_transform(ecole_wallonie, 31370)
ecole_wallonie_BD72
ecole_flandre = st_transform(ecole_flandre, 31370)

############### Tentative au niveau de la frontière
frontiere_Belgique_BD72 = st_transform(frontiere_Belgique, 31370)
frontiere_Belgique_BD72 = st_zm(frontiere_Belgique_BD72, drop = TRUE, what = "ZM")
frontiere_Belgique_BD72

mapview(frontiere_Belgique_BD72)

############### ecoles secondaire uniquement
ecole_wallonie_BD72_secondaire = ecole_wallonie_BD72[ecole_wallonie_BD72$niveau == "Secondaire", ]
ecole_flandre_secondaire = ecole_flandre[ecole_flandre$CATEGORIE == "Secundair onderwijs",]

############### Affichage des écoles en Belgique
mapview(ecole_wallonie_BD72_secondaire)+
mapview(ecole_flandre_secondaire, col.regions = "red")

############### Tentative de méthode de voronoi FLANDRE #####################
ecole_flandre_multipoint = st_union(ecole_flandre_secondaire)
ecole_flandre_secondaire_voronoi = st_voronoi(ecole_flandre_multipoint, st_geometry(frontiere_Belgique_BD72))


ecole_flandre_multipoint
ecole_flandre_secondaire_voronoi

# POUR MAPVIEW, IL FAUT QUE CE SOIT UN SF OBJECT !!!!!!!!!!!
flandre_voronoi_multipolygon = st_collection_extract(st_geometry(ecole_flandre_secondaire_voronoi), type = "POLYGON")
flandre_voronoi_multipolygon_sf = st_sf(c('Voronoi_flandre'), flandre_voronoi_multipolygon)

# fais l'intersection entre la frontière belge et les limites de Voronoi pour ne pas avoir de frontières "infinies" ou du moins qui n'ont pas de sens
voronoi_flandre_dans_Belgique = st_intersection(st_transform(flandre_voronoi_multipolygon, 31370), frontiere_Belgique_BD72)
voronoi_flandre_dans_Belgique_sf = st_sf(c('Voronoi_flandre'), voronoi_flandre_dans_Belgique)

mapview(flandre_voronoi_multipolygon_sf)+
  mapview(ecole_flandre_secondaire, col.regions = "red")

###################   FONCTIONNE POUR LA FLANDRE YOUHOUUUUUUUU ####################
mapview(voronoi_flandre_dans_Belgique_sf, col.regions = 0)+
  mapview(ecole_flandre_secondaire, col.regions = "red", cex = 4)
###################################################################################






############### Tentative de méthode de voronoi Belgique #####################

ecole_Belgique_secondaire_multipoint = st_union(c(st_union(ecole_flandre_secondaire), st_union(ecole_wallonie_BD72_secondaire)))
ecole_Belgique_secondaire_voronoi = st_voronoi(ecole_Belgique_secondaire_multipoint, st_geometry(frontiere_Belgique_BD72))

Belgique_voronoi_multipolygon = st_collection_extract(st_geometry(ecole_Belgique_secondaire_voronoi), type = "POLYGON")
Belgique_voronoi_multipolygon_sf = st_sf(c('Voronoi_Belgique'), Belgique_voronoi_multipolygon)

voronoi_complet_dans_Belgique = st_intersection(st_transform(Belgique_voronoi_multipolygon, 31370), frontiere_Belgique_BD72)
voronoi_complet_dans_Belgique_sf = st_sf(c('Voronoi_Belgique'), voronoi_complet_dans_Belgique)


mapview(voronoi_complet_dans_Belgique_sf, col.regions = "transparent")+ 
  mapview(ecole_wallonie_BD72_secondaire, col.regions = "darkgreen")+
  mapview(ecole_flandre_secondaire, col.regions = "red")
 
