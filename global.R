library(shiny)
#devtools::install_github("allisonglider/seabiRds")

world <- readRDS('world.RDS')
bath <- readRDS('bathymetry.RDS')

# con <- DBI::dbConnect(RPostgres::Postgres(), 
#                       host = config::get()$host_db, 
#                       dbname = config::get()$db, 
#                       port = config::get()$db_port, 
#                       user = config::get()$db_user, 
#                       password = config::get()$db_password)

Sys.setenv('AWS_ACCESS_KEY_ID' = config::get()$aws_key,
           'AWS_SECRET_ACCESS_KEY' = config::get()$aws_secret,
           'AWS_DEFAULT_REGION' = config::get()$aws_region)

gps <- arrow::open_dataset('s3://arcticecology-biologging/gps')
tdr <- arrow::open_dataset('s3://arcticecology-biologging/tdr')
# acc <- arrow::open_dataset('s3://arcticecology-biologging/acc')

# deployments <- con |> 
#   dplyr::tbl("deployments")  |> 
#   dplyr::filter(
#     site %in% c('Coats', 'CGM'), 
#     species == 'TBMU',
#     time_released > as.POSIXct('2022-01-01'), 
#     time_recaptured < as.POSIXct('2023-01-01'),
#     status_on %in% c('E', 'C'),
#     !is.na(acc_id)
#   ) |> 
#   dplyr::select(species, metal_band, dep_id, site, nest, 
#          dep_lon,dep_lat, time_released, time_recaptured, 
#          status_on, status_off, mass_on, mass_off, gps_id)|> 
#   dplyr::collect() |>  
#   dplyr::mutate(
#     year = as.numeric(strftime(time_released, '%Y')),
#     status_on = ifelse(status_on == 'E', 'Incubation','Chick-rearing')
#   ) 

deployments <- readRDS('deployments.RDS')

# -----

map_track <- function(data, basemap, b = bath, colony_loc) {

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

  bb <- seabiRds::bbox_at_zoom(locs = locs_sf)

  ggplot() +
    geom_contour_filled(data = b, aes(x = x, y = y, z = values),
                        breaks = seq(0, -1500, by = -200)) +
    geom_sf(data = basemap, fill = grey (0.9), size = 0.1) +
    geom_sf(data = tracks_sf, col = 'yellow', size = 0.75) +
    geom_sf(data = colony_sf, col = 'black', size = 3) +
    coord_sf(xlim = bb[c(1,3)], ylim = bb[c(2,4)]) +
    scale_fill_viridis_d(na.value=grey(0.8), end = 0.4, direction = -1, drop = FALSE) +
    guides(fill = 'none') +
    theme_light() +
    theme(
      text = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      ) +
    labs(x = '', y = '')
}

plot_profile <- function(data) {

  data |>
    dplyr::mutate(
      depth_m = depth_m * -1,
    ) |>
    dplyr::select(dep_id, time, coldist, depth_m, 'speed') |>
    tidyr::pivot_longer(cols = c('coldist','depth_m', 'speed')) |>
    dplyr::mutate(name = factor(name, labels = c("Distance (km)", "Depth (m)", "Speed (km/hr)"))) |>
    ggplot(aes(x = time, y = value)) +
    geom_line() +
    scale_x_datetime(date_labels = '%b-%d %H:%M') +
    facet_grid(rows = vars(name), scales = 'free')  +
    labs(x = 'Time (UTC)', y = '') +
    theme_light() +
    theme(
      text = element_text(size = 14),
      #axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_rect(fill = 'transparent'),
      strip.text = element_text(color = 'black'))
}
