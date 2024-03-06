

############################    8.1 Join by location      ##################################

library(sf)
airports = st_read('Entrainement_package_sf/data/airports.geojson', quiet = TRUE)
county = st_read('Entrainement_package_sf/data/USA_2_GADM_fips.shp', quiet = TRUE)

airports = st_transform(airports, 9311)
county = st_transform(county, 9311)

nm = county[county$NAME_1 == 'New Mexico', ]
plot(st_geometry(nm))
plot(st_geometry(airports), col = 'red', pch = 16, cex = 2, add = TRUE)

st_join(airports, nm)



############################    8.2 Subsetting by location ############################    

nm1 = nm[airports, ]
nm1

plot(st_geometry(nm))
plot(st_geometry(nm1), col = 'lightblue', add = TRUE)
plot(st_geometry(airports), col = 'red', pch = 16, cex = 2, add = TRUE)


##########################################################################################
############################    8.3 Geometric calculations    ############################    
##########################################################################################


############################    8.3.2.2 Area   ############################

county$area = st_area(county)
county$area[1:6]
## Units: [m^2]
## [1]  2455464324  1943957904  1079396694  1352486963 16439911090  2630547610

class(county$area)
## [1] "units"

library(units)
county$area = set_units(county$area, 'km^2')
county$area[1:6]
## Units: [km^2]
## [1]  2455.464  1943.958  1079.397  1352.487 16439.911  2630.548

as.numeric(county$area[1:6])
## [1]  2455.464  1943.958  1079.397  1352.487 16439.911  2630.548

plot(county[, 'area'], pal = function(n) hcl.colors(n, 'Spectral', rev = TRUE))





############################    8.3.2.3 Distance   ############################

d = st_distance(airports, nm)
d[, 1:6]

d = set_units(d, 'km')
d[, 1:6]

rownames(d) = airports$name
colnames(d) = nm$NAME_2
d[, 1:6]

d['Santa Fe Municipal', 'Santa Fe', drop = FALSE]



###########################   8.3.3 Logical geometric calculations #############

int = st_intersects(nm, nm, sparse = FALSE)
int[1:4, 1:4]


