# app.R — the main file which runs the app.

source("globals.R") # contains loading data, constants, themes, layout functions
source("ui.R") # contains the r shiny ui object
source("server.R") # contains r shiny server logic

shinyApp(ui, server)
