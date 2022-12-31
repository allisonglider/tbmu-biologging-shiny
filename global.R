library(dplyr)
library(ggplot2)
library(shiny)
theme_set(theme_linedraw())
#devtools::install_github("allisonglider/seabiRds")

world <- readRDS('world.RDS')
bath <- readRDS('bathymetry.RDS')

deployments <- readRDS('deployments.RDS')
data <- readRDS('processed_data.RDS')

# -----

map_track <- function(data, basemap, b = bath, colony_loc, start_time, end_time) {

  colony_sf <- sf::st_as_sf(data.frame(lon = colony_loc[1], lat = colony_loc[2]),
                            coords = c('lon','lat'), crs = 4326)

  locs_sf <- sf::st_as_sf(data, coords = c('lon','lat'), crs = 4326)
  crop_bath <- seabiRds::bbox_at_zoom(locs_sf, zoom_level = 6)
  b <- raster::crop(b, raster::extent(crop_bath[1], crop_bath[3], crop_bath[2], crop_bath[4]))

  b <- data.frame(
    raster::coordinates(b),
    values = raster::values(b)
  )

  tracks_sf <- locs_sf %>%
    group_by(dep_id) %>%
    summarize(
      n = n(),
      do_union=FALSE) %>%
    sf::st_cast(to = 'LINESTRING')
  
  sub_tracks_sf <- locs_sf |> 
    group_by(dep_id) |> 
    dplyr::filter(time >= start_time, time <=end_time) |> 
    summarize(
      n = n(),
      do_union=FALSE) |> 
    sf::st_cast(to = 'LINESTRING')

  bb <- seabiRds::bbox_at_zoom(locs = locs_sf)
  
  ggplot() +
    geom_contour_filled(data = b, aes(x = x, y = y, z = values),
                        breaks = seq(0, -1500, by = -200)) +
    geom_sf(data = basemap, fill = grey (0.9), size = 0.1) +
    geom_sf(data = tracks_sf, col = 'yellow', size = 0.5, linetype = 2) +
    geom_sf(data = sub_tracks_sf, col = 'yellow', size = 0.75) +
    geom_sf(data = colony_sf, col = 'black', size = 3) +
    coord_sf(xlim = bb[c(1,3)], ylim = bb[c(2,4)]) +
    scale_fill_viridis_d(na.value=grey(0.8), end = 0.4, direction = -1, drop = FALSE) +
    guides(fill = 'none') +
    labs(x = '', y = '') +
    theme(
      text = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
    )
}

plot_profile <- function(data, start_time, end_time) {

  data |>
    dplyr::mutate(
      depth_m = depth_m * -1,
    ) |>
    dplyr::select(dep_id, time, coldist, depth_m, wbf) |>
    tidyr::pivot_longer(cols = c('coldist','depth_m', 'wbf')) |>
    dplyr::mutate(name = factor(name, labels = c("Distance (km)", "Depth (m)", "Wing beat frequency (Hz)"))) |>
    dplyr::filter(time >= start_time, time <=end_time) |> 
    ggplot(aes(x = time, y = value)) +
    geom_line() +
    scale_x_datetime(date_labels = '%b-%d %H:%M') +
    facet_grid(rows = vars(name), scales = 'free')  +
    labs(x = 'Time (UTC)', y = '') +
    theme(
      text = element_text(size = 14),
      strip.background = element_rect(fill = grey(0.8)),
      strip.text = element_text(color = 'black', size = 8),
    )
  
}
