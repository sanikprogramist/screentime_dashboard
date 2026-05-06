# app.R — the main file which runs the app.

# data_preparation.R contains scripts to convert the ActivityWatch SQL database to usable .rds files
source("globals.R") # contains loading data, constants, themes, layout functions
source("ui.R") # contains the r shiny ui object
source("server.R") # contains r shiny server logic

shinyApp(ui, server)
