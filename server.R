
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
      
      select_data <- data |> 
        filter(dep_id == dd)
      
      validate(
        need(nrow(select_data) > 0, ''),
      )

      sliderInput("time", "Select time range:", 
                  value = c(lubridate::round_date(as.POSIXct(min(select_data$time) + 30, tz = 'UTC'), unit = '30 minutes'), 
                            lubridate::round_date(as.POSIXct(max(select_data$time) - 30, tz = 'UTC'), unit = '30 minutes')), 
                  min = lubridate::round_date(as.POSIXct(min(select_data$time), tz = 'UTC'), unit = '30 minutes'),
                  max = lubridate::round_date(as.POSIXct(max(select_data$time), tz = 'UTC'), unit = '30 minutes'),
                  step = 60*30,
                  timeFormat = '%b-%d %H:%M', timezone = "+0000")
    })
    
    output$map_track <- renderPlot({
      
      if (!is.null(input$site) & !is.null(input$stage) & !is.null(input$deployment)) {
        
        dd <- input$deployment
        dep_lon <- deployments$dep_lon[deployments$dep_id == dd]
        dep_lat <- deployments$dep_lat[deployments$dep_id == dd]
        
        select_data <- data |> 
          filter(dep_id == dd)
        
        if (input$time[1] > max(select_data$time)) start_time <- min(select_data$time) else start_time <- input$time[1] 
        if (input$time[2] < min(select_data$time)) end_time <- max(select_data$time) else end_time <- input$time[2]
        
        
        if (nrow(select_data) > 10) {
          
          m <- map_track(data = select_data, basemap = world, colony_loc = c(dep_lon, dep_lat),
                         start_time = input$time[1], end_time = input$time[2])
          
          p <- plot_profile(data = select_data, start_time = start_time, end_time = end_time)
          
          cowplot::plot_grid(p, m, nrow = 1)
        } else print('Retrieving data')
        
      }
    })
    
    output$mass_on <- renderText(paste('Bird mass at start:',
                                       deployments$mass_on[deployments$dep_id == input$deployment]))
    
    output$mass_off <- renderText(paste('Bird mass at end:',
                                       deployments$mass_off[deployments$dep_id == input$deployment]))
    
})
