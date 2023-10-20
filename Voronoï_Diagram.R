library(ggplot2)
library(ggvoronoi)
set.seed(45056)
x <- sample(1:200,100)
y <- sample(1:200,100)
points <- data.frame(x, y,
                     distance = sqrt((x-100)^2 + (y-100)^2))
circle <- data.frame(x = 100*(1+cos(seq(0, 2*pi, length.out = 2500))),
                     y = 100*(1+sin(seq(0, 2*pi, length.out = 2500))),
                     group = rep(1,2500))

ggplot(points) +
  geom_voronoi(aes(x,y,fill=distance))

ggplot(points,aes(x,y)) +
  stat_voronoi(geom="path") +
  geom_point()

ggplot(data=points, aes(x=x, y=y, fill=distance)) + 
  geom_voronoi(outline = circle)
