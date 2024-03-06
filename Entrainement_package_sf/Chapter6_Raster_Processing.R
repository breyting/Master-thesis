library(stars)
r = read_stars('Entrainement_package_sf/data/MOD13A3_2000_2019.tif')
names(r) = 'NDVI'

v = r[[1]][50, 220, ]
class(v)
## [1] "numeric"

plot(v, type = 'o')

dates = read.csv('Entrainement_package_sf/data/MOD13A3_2000_2019_dates2.csv')
dates$date = as.Date(dates$date)

plot(dates$date, v, type = 'o', xlab = 'Time', ylab = 'NDVI')

seasons = c('winter', 'spring', 'summer', 'fall')
cols = c('blue', 'purple', 'red', 'yellow')

plot(dates$date, v, type = 'l', xlab = 'Time', ylab = 'NDVI', col = 'grey')
for(i in 1:length(seasons)) {
  tmp = v
  tmp[dates$season != seasons[i]] = NA
  lines(dates$date, tmp, col = cols[i], type = 'o')
}