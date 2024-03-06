#####################          7.2.2 GEOMETRY (sfg)       #######################################

library(sf)
pnt1 = st_point(c(34.798443, 31.243288))
pnt1
## POINT (34.79844 31.24329)
class(pnt1)
## [1] "XY"    "POINT" "sfg"

a = st_polygon(list(cbind(c(0,0,7.5,7.5,0),c(0,-1,-1,0,0))))
a
## POLYGON ((0 0, 0 -1, 7.5 -1, 7.5 0, 0 0))
class(a)
## [1] "XY"      "POLYGON" "sfg"
plot(a, border = 'blue', col = '#0000FF33', lwd = 2)

b = st_polygon(list(cbind(c(0,1,2,3,4,5,6,7,7,0),c(1,0,0.5,0,0,0.5,-0.5,-0.5,1,1))))
b
## POLYGON ((0 1, 1 0, 2 0.5, 3 0, 4 0, 5 0.5, 6 -0.5, 7 -0.5, 7 1, 0 1))
class(b)
## [1] "XY"      "POLYGON" "sfg"
plot(b, border = 'red', col = '#FF000033', lwd = 2)

ab = c(a, b)
ab
## MULTIPOLYGON (((0 0, 0 -1, 7.5 -1, 7.5 0, 0 0)), ((0 1, 1 0, 2 0.5, 3 0, 4 0, 5 0.5, 6 -0.5, 7 -0.5, 7 1, 0 1)))
class(ab)
## [1] "XY"           "MULTIPOLYGON" "sfg"
plot(ab, border = 'darkgreen', col = '#00FF0033', lwd = 2)


i = st_intersection(a, b)
i
## GEOMETRYCOLLECTION (POLYGON ((7 0, 7 -0.5, 6 -0.5, 5.5 0, 7 0)), LINESTRING (4 0, 3 0), POINT (1 0))
class(i)
## [1] "XY"                 "GEOMETRYCOLLECTION" "sfg"
plot(i, border = 'black', col = 'darkgrey', lwd = 2)





#####################          7.2.3 Geometry column(sfc)       #######################################

pnt2 = st_point(c(34.812831, 31.260284))
pnt3 = st_point(c(35.011635, 31.068616))

geom = st_sfc(pnt1, pnt2, pnt3, crs = 4326)
geom
## Geometry set for 3 features 
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: 34.79844 ymin: 31.06862 xmax: 35.01164 ymax: 31.26028
## Geodetic CRS:  WGS 84
## POINT (34.79844 31.24329)
## POINT (34.81283 31.26028)
## POINT (35.01163 31.06862)
plot(geom)



#####################          7.2.4 Layer (sf)       #######################################

name = c('Beer-Sheva Center', 'Beer-Sheva University', 'Dimona')
city = c('Beer-Sheva', 'Beer-Sheva', 'Dimona')
lines = c(4, 5, 1)
piano = c(FALSE, TRUE, FALSE)
dat = data.frame(name, city, lines, piano)
dat
##                    name       city lines piano
## 1     Beer-Sheva Center Beer-Sheva     4 FALSE
## 2 Beer-Sheva University Beer-Sheva     5  TRUE
## 3                Dimona     Dimona     1 FALSE

layer = st_sf(dat, geom)
layer
## Simple feature collection with 3 features and 4 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: 34.79844 ymin: 31.06862 xmax: 35.01164 ymax: 31.26028
## Geodetic CRS:  WGS 84
##                    name       city lines piano                      geom
## 1     Beer-Sheva Center Beer-Sheva     4 FALSE POINT (34.79844 31.24329)
## 2 Beer-Sheva University Beer-Sheva     5  TRUE POINT (34.81283 31.26028)
## 3                Dimona     Dimona     1 FALSE POINT (35.01163 31.06862)




#####################          7.2.5 Interactive mapping with mapview      #######################################

library(mapview)
mapview(layer)




#####################          7.3 Extracting layer components     #######################################

st_geometry(layer)
## Geometry set for 3 features 
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: 34.79844 ymin: 31.06862 xmax: 35.01164 ymax: 31.26028
## Geodetic CRS:  WGS 84
## POINT (34.79844 31.24329)
## POINT (34.81283 31.26028)
## POINT (35.01163 31.06862)

st_drop_geometry(layer)
##                    name       city lines piano
## 1     Beer-Sheva Center Beer-Sheva     4 FALSE
## 2 Beer-Sheva University Beer-Sheva     5  TRUE
## 3                Dimona     Dimona     1 FALSE

st_coordinates(layer)
##             X        Y
## [1,] 34.79844 31.24329
## [2,] 34.81283 31.26028
## [3,] 35.01163 31.06862





#####################          7.4 Creating point layer from table   #######################################

rainfall = read.csv('Entrainement_package_sf/data/rainfall.csv')
head(rainfall)
##      num altitude sep oct nov dec jan feb mar apr may              name
## 1 110050       30 1.2  33  90 117 135 102  61  20 6.7 Kfar Rosh Hanikra
## 2 110351       35 2.3  34  86 121 144 106  62  23 4.5              Saar
## 3 110502       20 2.7  29  89 131 158 109  62  24 3.8             Evron
## 4 111001       10 2.9  32  91 137 152 113  61  21 4.8       Kfar Masrik
## 5 111650       25 1.0  27  78 128 136 108  59  21 4.7     Kfar Hamakabi
## 6 120202        5 1.5  27  80 127 136  95  49  19 2.7        Haifa Port
##      x_utm   y_utm
## 1 696533.1 3660837
## 2 697119.1 3656748
## 3 696509.3 3652434
## 4 696541.7 3641332
## 5 697875.3 3630156
## 6 687006.2 3633330

rainfall = st_as_sf(rainfall, coords = c('x_utm', 'y_utm'), crs = 32636)
rainfall
## Simple feature collection with 169 features and 12 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: 629301.4 ymin: 3270290 xmax: 761589.2 ymax: 3681163
## Projected CRS: WGS 84 / UTM zone 36N
## First 10 features:
##       num altitude sep oct nov dec jan feb mar apr may              name
## 1  110050       30 1.2  33  90 117 135 102  61  20 6.7 Kfar Rosh Hanikra
## 2  110351       35 2.3  34  86 121 144 106  62  23 4.5              Saar
## 3  110502       20 2.7  29  89 131 158 109  62  24 3.8             Evron
## 4  111001       10 2.9  32  91 137 152 113  61  21 4.8       Kfar Masrik
## 5  111650       25 1.0  27  78 128 136 108  59  21 4.7     Kfar Hamakabi
## 6  120202        5 1.5  27  80 127 136  95  49  19 2.7        Haifa Port
## 7  120630      450 1.9  36  93 161 166 128  71  21 4.9  Haifa University
## 8  120750       30 1.6  31  91 163 170 146  76  22 4.9             Yagur
## 9  120870      210 1.1  32  93 147 147 109  61  16 4.3        Nir Etzyon
## 10 121051       20 1.8  32  85 147 142 102  56  13 4.5         En Carmel
##                    geometry
## 1  POINT (696533.1 3660837)
## 2  POINT (697119.1 3656748)
## 3  POINT (696509.3 3652434)
## 4  POINT (696541.7 3641332)
## 5  POINT (697875.3 3630156)
## 6  POINT (687006.2 3633330)
## 7  POINT (689553.7 3626282)
## 8  POINT (694694.5 3624388)
## 9  POINT (686489.5 3619716)
## 10 POINT (683148.4 3616846)

mapview(rainfall, zcol = 'sep')






#######################################################################################################
#####################          7.5 sf layer properties          #######################################
#######################################################################################################


#####################   7.5.1 Dimensions  #######################################

nrow(rainfall)
## [1] 169

ncol(rainfall)
## [1] 13

dim(rainfall)
## [1] 169  13

st_geometry(rainfall)
st_drop_geometry(rainfall)


#####################   7.5.2 Spatial properties  #######################################

st_bbox(rainfall)
##      xmin      ymin      xmax      ymax 
##  629301.4 3270290.2  761589.2 3681162.7

st_crs(rainfall)
plot(st_geometry(rainfall))
plot(st_coordinates(rainfall)[, 1], st_coordinates(rainfall)[, 2])


#####################   7.6 Subsetting based on attributes  #######################################

rainfall[1:10, ]
rainfall[rainfall$jan > 100, ]

plot(st_geometry(rainfall[1:10, ]), main = 'rainfall[1:10, ]')
plot(st_geometry(rainfall[rainfall$jan > 100, ]), main = 'rainfall[rainfall$jan > 100, ]')

rainfall[1:5, c('jan', 'feb')]
## Simple feature collection with 5 features and 2 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: 696509.3 ymin: 3630156 xmax: 697875.3 ymax: 3660837
## Projected CRS: WGS 84 / UTM zone 36N
##   jan feb                 geometry
## 1 135 102 POINT (696533.1 3660837)
## 2 144 106 POINT (697119.1 3656748)
## 3 158 109 POINT (696509.3 3652434)
## 4 152 113 POINT (696541.7 3641332)
## 5 136 108 POINT (697875.3 3630156)

st_drop_geometry(rainfall[1:5, c('jan', 'feb')])
##   jan feb
## 1 135 102
## 2 144 106
## 3 158 109
## 4 152 113
## 5 136 108


#####################   7.7 Reading vector layers  #######################################

county = st_read('Entrainement_package_sf/data/USA_2_GADM_fips.shp', quiet = TRUE)
county

airports = st_read('Entrainement_package_sf/data/airports.geojson', quiet = TRUE)
airports

plot(county)
plot(county[, 'TYPE_2'], key.width = lcm(5), key.pos = 4)
plot(st_geometry(county))
plot(st_geometry(county), border = 'grey')

plot(st_geometry(county), border = 'grey')
plot(st_geometry(airports), col = 'red', add = TRUE)


library(stars)
r = read_stars('Entrainement_package_sf/data/rainfall.tif')
names(r) = 'rainfall (mm)'

plot(r, breaks = 'equal', col = hcl.colors(11, 'Spectral'), reset = FALSE)
plot(st_geometry(rainfall), add = TRUE)


#####################   7.10  Writing vector layers  #######################################

m = c('sep', 'oct', 'nov', 'dec', 'jan', 'feb', 'mar', 'apr', 'may')
rainfall$annual = apply(st_drop_geometry(rainfall[, m]), 1, sum)

st_write(rainfall, 'Entrainement_package_sf/data/rainfall_pnt.shp')
