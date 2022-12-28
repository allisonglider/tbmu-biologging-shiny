library(shiny)

shinyUI(fluidPage(

    titlePanel("TBMU Biologging Data"),

    sidebarLayout(
        sidebarPanel(
            h6('This app explores the biologging data available for breeding thick-billed murres within the Arctic Ecology Lab database'),

            selectInput(inputId = 'site', label = 'Select colony:', choices = unique(deployments$site),
                        selected = 'CGM', multiple = FALSE
            ),
            
            selectInput(inputId = 'stage', label = 'Select breeding stage:', choices = unique(deployments$status_on),
                        selected = 'Incubation', multiple = FALSE
            ),
            
            uiOutput('depSelect'),

            h6('\n Created by Allison Patterson @allisonglider'),

        ),

        mainPanel(
          fluidRow(
            plotOutput('map_track')
          ), 

        )
       
    )
))
