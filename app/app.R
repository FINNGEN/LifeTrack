#
# app.R  - LifeTrack
#

source("ui.R")
source("server.R")

shinyApp(ui, server, port = 8559)
