
data <- readRDS('D:/tbmu-mass-change/processed_data.RDS') |> 
  dplyr::filter(!is.na(depth_m))

data |> 
  dplyr::select(dep_id, time, lon, lat, depth_m, coldist, wbf) |> 
  dplyr::slice(seq(1, dplyr::n(), 10)) |> 
  saveRDS('processed_data.RDS')

dives <- data |> 
  dplyr::group_by(dep_id) |> 
  dplyr::select(dep_id, time, lon, lat, depth_m) |> 
  dplyr::mutate(
    diving = ifelse(depth_m > 3, 1, 0),
    dive_id = seabiRds::getSessions(diving)
  ) |> 
  dplyr::group_by(dep_id, dive_id) |>
  dplyr::mutate(
    duration = dplyr::n()
  ) |> 
  dplyr::group_by(dep_id) |>
  dplyr::mutate(
    diving = ifelse(diving == 1 & duration <= 3 , 0, diving),
    dive_id = seabiRds::getSessions(diving)
  )|> 
  dplyr::group_by(dep_id, dive_id) |>
  dplyr::mutate(
    duration = dplyr::n()
  )

dives |> 
  dplyr::group_by(dep_id) |>
  dplyr::mutate(
    bout_id = seabiRds::getSessions(duration < 600, ignore = T, ignoreValue = F)
  ) |> dplyr::filter(!is.na(bout_id), diving == 1) |> 
  dplyr::group_by(dep_id, dive_id, bout_id) |>
  dplyr::summarize(
    time = time[1],
    lon = lon[1],
    lat = lat[1],
    duration = duration[1],
    max_depth = max(depth_m)
  )|> 
  saveRDS('dive_data.RDS')
