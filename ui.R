library(shiny)

shinyUI(fluidPage(
  
  titlePanel("TBMU Biologging Data"),
  
  sidebarLayout(
    sidebarPanel(
      h6('This app explores some of the biologging data available for breeding 
               thick-billed murres within the Arctic Ecology Lab database. Only data 
               from 2022 are dipslayed here, although data are available for other years.
               Tracks were collected with Technosmart Axytrek loggers programmed to collect 
               GPS locations at 1 or 3 min intervals, depth and temperature at 1 sec intervals, and
               acceleration in 3 axes at 50 Hz.'),
      
      selectInput(inputId = 'site', label = 'Select colony:', choices = unique(deployments$site),
                  selected = 'CGM', multiple = FALSE
      ),
      
      selectInput(inputId = 'stage', label = 'Select breeding stage:', choices = unique(deployments$status_on),
                  selected = 'Incubation', multiple = FALSE
      ),
      
      uiOutput('depSelect'),
      
      h6('Use the slider to zoom in to a particular time range'),
      uiOutput('timeSelect'),
      
      h6(''),
      
      textOutput("mass_on"),
      textOutput("mass_off"),
      
      h6(''),
      
      h6('\n\n\n Created by Allison Patterson'),
      
    ),
    
    mainPanel(
      
      fluidRow(
        plotOutput('map_track')
      ), 
      h6('Profile plots (left) show distance from colony, dive activity, and wing beat frequency for a deployment within the selected time range.The map (right) shows the foraging movements for the selected deployment (yellow line). 
         The solid line corresponds to the time period selected with the time slider, and the dashed lines shows the complete track. Orange points indicate dive locations. 
         The black point shows the location of the colony. Grey areas are land, blue shading indicates 200 m depth contours.'),
      
    )
    
  )
))
