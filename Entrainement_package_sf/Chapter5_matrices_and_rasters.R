library(stars)
s = read_stars('Entrainement_package_sf/data/dem.tif')
r = read_stars('Entrainement_package_sf/data/MOD13A3_2000_2019.tif')

plot(s)
plot(r)
plot(s, text_values = TRUE, axes = TRUE, col = terrain.colors(10))

plot(s, text_values = TRUE, col = terrain.colors(10), breaks = 'equal')
plot(s, text_values = TRUE, col = terrain.colors(3), breaks = c(0, 100, 300, 400))

library(mapview)
mapview(r[,,,1])

library(cubeview)
cubeview(r)
