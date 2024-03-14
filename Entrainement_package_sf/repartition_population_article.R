libs <- c(
  "tidyverse", "sf", "classInt", 
  "cartogram", "rayshader",
  "giscoR", "eurostat"
)

installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

invisible(lapply(libs, library, character.only = T))

# CONSTANTS
#------------
# define projections
# longlat
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# Lambert
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"


# ggplot2 theme
theme_for_the_win <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = "Montserrat"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_blank(),
      legend.position = c(.5, .99),
      legend.text = element_text(size = 10, color = "grey20"),
      legend.title = element_text(size = 15, color = "grey20"),
      legend.spacing.y = unit(0.25, "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = unit(
        c(t = 0, r = 0, b = 0, l = 0), "lines"
      ),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.border = element_blank(),
    )
}

# colors
cols <- rev(c(
  "#140e26", "#451a40",
  "#7d1d53", "#b32957", "#ccaead",
  "#eb804e", "#ffdc58"
))

# get NUTS3 shapefile
deu_nuts3 <- giscoR::gisco_get_nuts(
  year = "2021",
  epsg = "4326",
  resolution = "3",
  nuts_level = "3",
  country = "DE"
)

# get population data
pop_df <- eurostat::get_eurostat("demo_r_pjangrp3",
                                 time_format = "num"
) |>
  dplyr::filter(
    sex == "T" &
      unit == "NR" &
      age == "TOTAL" &
      grepl("DE", geo) &
      TIME_PERIOD == 2021
  ) |>
  dplyr::select(geo, values)

names(pop_df)[1] <- "NUTS_ID"

# merge shp and data.frame
df <- deu_nuts3 |>
  left_join(pop_df, by = "NUTS_ID")


####################################### CHOROPLETH MAP #######################################

# 1. POLYGONS
#------------
get_polygon <- function() {
  # st_area returns square meters so we get square km by dividing the result by 1000
  df$area_sqkm <- as.numeric(sf::st_area(df) / 1000000)
  
  deu_polygon <- df |>
    dplyr::mutate(pop_per_sqr_km = values / area_sqkm)
  
  return(deu_polygon)
}

deu_polygon <- get_polygon()


# min/max values
vmin <- min(deu_polygon$pop_per_sqr_km, na.rm = T)
vmax <- max(deu_polygon$pop_per_sqr_km, na.rm = T)

# bins
brk <- round(classIntervals(deu_polygon$pop_per_sqr_km,
                            n = 6,
                            style = "equal"
)$brks, 0) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)

# breaks
breaks <- c(vmin, brk)

make_polygon_map <- function() {
  p1 <-
    ggplot(deu_polygon) +
    geom_sf(aes(fill = pop_per_sqr_km),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = cols,
      breaks = breaks,
      labels = round(breaks, 0),
      limits = c(vmin, vmax)
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.15, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position.inside = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p1)
}

map1 <- make_polygon_map()
map1




######################### Bubble map   ###########################################

# 2. POINTS
#-----------
# normalize population size
df$pop_1000s <- df$values / 1000

# min/max values
vmin <- min(df$pop_1000s, na.rm = T)
vmax <- max(df$pop_1000s, na.rm = T)

# bins
brk <- round(classIntervals(df$pop_1000s,
                            n = 6,
                            style = "equal"
)$brks, 0) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)

# breaks
breaks <- c(vmin, brk)

deu_points <- df |>
  sf::st_centroid()

deu_coords <- deu_points |>
  dplyr::mutate(
    long = unlist(map(geometry, 1)),
    lat = unlist(map(geometry, 2))
  ) |>
  dplyr::select(NAME_LATN, long, lat, pop_1000s) |>
  sf::st_drop_geometry() |>
  dplyr::as_tibble() |>
  dplyr::arrange(desc(pop_1000s))

label_regions <- function() {
  ggrepel::geom_text_repel(deu_coords[1:5, ],
                           mapping = aes(x = long, y = lat, label = NAME_LATN),
                           colour = "grey20",
                           family = "Montserrat",
                           size = 20,
                           segment.colour = "grey20",
                           segment.alpha = .9,
                           segment.linetype = 3,
                           segment.size = .25,
                           nudge_x = .95,
                           nudge_y = .15,
                           direction = "x"
  )
}

make_point_map <- function() {
  p2 <-
    ggplot() +
    geom_sf(
      data = deu_polygon,
      fill = "transparent",
      color = "grey20",
      size = .1
    ) +
    geom_sf(
      data = deu_points,
      mapping = aes(
        size = pop_1000s,
        geometry = geometry
      ), color = cols[5],
      alpha = .5
    ) +
    label_regions() +
    scale_size(
      breaks = breaks,
      range = c(1, 10),
      labels = round(breaks, 0),
      limits = c(vmin, vmax),
      name = ""
    ) +
    guides(
      color = "none",
      size = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        label.hjust = 0,
        nrow = 1,
        byrow = F,
        reverse = F,
        label.position = "bottom"
      )
    ) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p2)
}

map2 <- make_point_map()
map2


################################# Cartogram   ####################################


# 3. CARTOGRAM
#-------------

get_cartogram <- function() {
  deu_cart <- df |>
    sf::st_transform(crs = crsLAEA) |>
    cartogram::cartogram_cont(
      weight = "pop_1000s",
      itermax = 5
    ) |>
    sf::st_transform(crs = crsLONGLAT)
  return(deu_cart)
}

deu_cart <- get_cartogram()


make_cartogram <- function() {
  p3a <-
    ggplot(deu_cart) +
    geom_sf(aes(fill = pop_1000s),
            color = "grey20",
            size = .1
    ) +
    label_regions() +
    scale_fill_gradientn(
      name = "",
      colours = cols,
      breaks = breaks,
      labels = round(breaks, 0),
      limits = c(vmin, vmax)
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.15, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p3a)
}

map3a <- make_cartogram()
map3a



#########################################   DOT DENSITY MAP  ###################################

get_dot_density <- function() {
  num_dots <- ceiling(dplyr::select(as.data.frame(df), pop_1000s))
  deu_dots <- map_df(
    names(num_dots),
    ~ sf::st_sample(df, size = num_dots[, .x], type = "random") |>
      sf::st_cast("POINT") |>
      sf::st_coordinates() |>
      as_tibble() |>
      setNames(c("long", "lat"))
  )
  return(deu_dots)
}

deu_dots <- get_dot_density()


make_dot_density_map <- function() {
  p4 <-
    ggplot(deu_dots) +
    geom_sf(
      data = deu_nuts3, fill = "transparent",
      color = "grey20", size = .1
    ) +
    geom_point(
      data = deu_dots, aes(x = long, y = lat),
      color = cols[5], size = .1, shape = 19, alpha = .2
    ) +
    #label_regions() +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p4)
}

map4 <- make_dot_density_map()
map4
