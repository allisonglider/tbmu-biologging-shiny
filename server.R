
library(shiny)

shinyServer(function(input, output, session) {

    output$depSelect <- renderUI({

        dd <- unique(deployments$dep_id[deployments$site == input$site & 
                                          deployments$status_on == input$stage])

        selectInput("deployment", "Select deployment to show:",
                    choices = dd, selected = dd[1])
    })
    
    output$timeSelect <- renderUI({
      dd <- input$deployment
      
      gps_data <- gps |>
        filter(
          dep_id == dd,
          deployed == 1
        ) |>
        select(dep_id, time, lon, lat) |>
        collect()
      
      validate(
        need(nrow(gps_data) > 0, ''),
      )
      
      sliderInput("time", "Select time range:", 
                  value = c(lubridate::round_date(as.POSIXct(min(gps_data$time), tz = 'UTC'), unit = '30 minutes'), 
                            lubridate::round_date(as.POSIXct(max(gps_data$time), tz = 'UTC'), unit = '30 minutes')), 
                  min = lubridate::round_date(as.POSIXct(min(gps_data$time), tz = 'UTC'), unit = '30 minutes'),
                  max = lubridate::round_date(as.POSIXct(max(gps_data$time), tz = 'UTC'), unit = '30 minutes'),
                  step = 60*30,
                  timeFormat = '%b-%d %H:%M', timezone = "+0000")
    })
    
    output$map_track <- renderPlot({

      if (!is.null(input$site) & !is.null(input$stage) & !is.null(input$deployment)) {
        
        dd <- input$deployment
        dep_lon <- deployments$dep_lon[deployments$dep_id == dd]
        dep_lat <- deployments$dep_lat[deployments$dep_id == dd]

        gps_data <- gps |>
          filter(
            dep_id == dd,
            deployed == 1
          ) |>
          select(dep_id, time, lon, lat) |>
          collect()

        gps_data <- seabiRds::filterSpeed(data.frame(gps_data), threshold = 100)

        tdr_data <- tdr |>
          filter(
            dep_id == dd,
            deployed == 1
          ) |>
          select(dep_id, time, temperature_c, depth_m, wet) |>
          collect() |>
          arrange(dep_id, time)
        
        # acc_data <- acc |>
        #   filter(
        #     dep_id == dd,
        #     deployed == 1
        #   ) |>
        #   select(dep_id, time, z) |>
        #   collect() |>
        #   arrange(dep_id, time) |> 
        #   mutate(
        #     wbf = seabiRds::getPeakFrequency(data = z, time = time, method = 'fft', 
        #                                      window = 60, threshold = 0.1, sample = 6, maxfreq = 12),
        #   ) |> filter(!is.na(wbf))
        
        validate(
          need(nrow(gps_data) > 0, message = 'Retrieving data...'),
        )

        all_data <- gps_data|>
          dplyr::full_join(tdr_data, by = c("time", "dep_id")) |>
          # dplyr::inner_join(acc_data, by = c("time", "dep_id") ) |> 
          dplyr::arrange(dep_id, time) |>
          dplyr::mutate(
            lon = imputeTS::na_interpolation(lon),
            lat = imputeTS::na_interpolation(lat),
            dist = seabiRds::getDist(lon, lat),
            dt = seabiRds::getDT(time),
            speed = dist/dt,
            coldist = seabiRds::getColDist(lon, lat, dep_lon, dep_lat)
          )

        validate(
          need(nrow(gps_data) > 0, ''),
        )
        m <- map_track(data = all_data, basemap = world, colony_loc = c(dep_lon, dep_lat),
                       start_time = input$time[1], end_time = input$time[2])

        p <- plot_profile(data = all_data, start_time = input$time[1], end_time = input$time[2])

        cowplot::plot_grid(p, m, nrow = 1)

      }
    })
    
    output$mass_on <- renderText(paste('Bird mass at start:',
                                       deployments$mass_on[deployments$dep_id == input$deployment]))
    
    output$mass_off <- renderText(paste('Bird mass at end:',
                                       deployments$mass_off[deployments$dep_id == input$deployment]))
    
})
