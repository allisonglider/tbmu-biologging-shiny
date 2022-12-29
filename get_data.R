
data <- readRDS('D:/tbmu-mass-change/processed_data.RDS')

data |> 
  dplyr::select(dep_id, time, lon, lat, depth_m, coldist, wbf) |> 
  dplyr::slice(seq(1, n(), 10)) |> 
  saveRDS('processed_data.RDS')


