#
# ui  - LifeTrack
#

library(shinydashboard)
library(shinyWidgets)
library(shinybusy)
library(shinyjs)
library(lubridate)
library(ggiraph)

# css ####
css <- "
.multicol{font-size:12px;
  height:auto;
  -webkit-column-count: 2;
  -moz-column-count: 2;
  column-count: 2;
}

div.checkbox {margin-top: -20px;}
<!-- #info{color:red; max-width: 150px; max-height: 25px; margin: 15px; padding: 2px;} -->

body{
    min-height: 611px;
    height: auto;
    max-width: 800px;
    margin: auto;
}
  
.sidebar {
    color: #FFF;
    position: fixed;
    width: 280px;
    white-space: nowrap;
    overflow: visible;
}

.main-header {
  position: fixed;
  width: 100%;
}

.content {
  margin-top: 50px;
}

tvt_slider_class.js-range-slider {
   background-color: grey;
   text-color: grey;
}

.irs-grid-pol.small {height: 0px;},

"

# style for horizontal rules in Files
hr_style = "margin-top: 2px; margin-bottom: 2px; border-top: 1px solid gray;"

# shinyUI ####
shinyUI(
  dashboardPage(title = "LifeTrack",
                
                dashboardHeader(title = "LifeTrack", 
                                titleWidth = 280,
                                disable = FALSE),
                
                dashboardSidebar(
                  tags$head(tags$style(css)), # tags$script(HTML(html)
                  width = 280,
                  shinyWidgets::chooseSliderSkin("Flat", "green"),
                  
                  sidebarMenu(
                    div(style = "margin-left: 15px; margin-top:10px;",
                        textOutput("version")
                    ),
                    
                    div(style = "margin-bottom: -15px;",
                        uiOutput('upload_cohort_ui')
                    ),
                    div(style = "margin-top:-30px; margin-bottom: -5px; ",
                        selectizeInput("person", label = "Person", width = "100%",
                                       choices = NULL,
                                       options = list(multiple = FALSE,
                                                      placeholder = 'Select ID',
                                                      closeAfterSelect = TRUE,
                                                      maxOptions = 100000L
                                       )
                        )
                        # uiOutput("select_person_ui")
                    ),
                    div(style = "margin-top: +15px; margin-bottom: -25px; display: flex; ",
                        checkboxInput("hide_visited", label = "Hide visited", value = FALSE)
                    ),
                    
                    hr(),
                    
                    div(style = "margin-top: -20px; margin-left:20px; margin-right:20px;",
                        sliderInput("date_range",
                                    "Date range", 
                                    min = ymd("2018-01-01"),
                                    max = ymd("2021-02-01"),
                                    value = c(ymd("2018-01-01"), ymd("2021-02-01"))
                        )
                    ),
                    
                    div(style = "margin-top:-20px; margin-bottom: -0px; ",
                        textInput("class_regexp", "Category filter (regex)", value = "")
                    ),
                    div(style = "margin-top:-20px; margin-bottom: -0px; ",
                        textInput("entry_regexp", "Entry highlight filter (regex)", value = "")
                    ),
                    div(style = "margin-top: -20px; margin-left:20px; margin-right:20px;",
                        sliderInput("category_range",
                                    "Category range", 
                                    min = 1,
                                    max = 50,
                                    value = c(1, 50)
                        )
                    ),
                    
                    div(style = "margin-top:-5px; margin-bottom: -0px; display: flex; ",
                        div(actionButton("filter_data", "Filter")),
                        div(actionButton("reset_filter", "Reset"))
                    ),
                    
                    hr(),
                    
                    # div(style = "margin-top: -20px; margin-left:20px; margin-right:20px;",
                    #     sliderInput("prevalence",
                    #                 "Prevalence", 
                    #                 min = 0.0,
                    #                 max = 100.0,
                    #                 value = c(0.0, 100.0),
                    #                 step = 0.1,
                    #                 post = "%",
                    #                 round = -1
                    #     ),
                    #     div(style = "margin-top:-5px; margin-bottom: -0px; display: flex; ",
                    #         div(actionButton("show_prevalence", "Show")),
                    #         div(actionButton("reset_prevalence", "Reset")),
                    #     )
                    # ),

                    div(style = "margin-top: +15px; margin-bottom: -25px; display: flex; ",
                        checkboxInput("prevalence", label = "Prevalence", value = FALSE)
                    ),
                    
                    hr(),
                    
                    div(style = "margin-top: -20px; margin-left:20px; margin-right:20px;",
                        sliderInput("selection_extension",
                                    "Click extension (months +/-)", 
                                    min = 0,
                                    max = 24,
                                    value = 0
                        )
                    ),
                    
                    hr(),
                    
                    menuItem('Plot Settings', tabName = 'plot_settings', startExpanded = FALSE, width = "350px;", icon = icon("gear"),
                             div(style = "margin-top:-10px; margin-bottom: -10px; ",
                                 sliderInput(
                                   "x_factor",
                                   "Height",
                                   min = 0.25,
                                   max = 4,
                                   value = 1,
                                   step = 0.25
                                 )
                             ),
                             
                             div(style = "margin-top:-20px; margin-bottom: -0px; ",
                                 sliderInput(
                                   "dotsize",
                                   "dotsize",
                                   min = -5,
                                   max = 5,
                                   value = 0,
                                   step = 0.1
                                 )
                             ),
                             
                             div(style = "margin-top:-20px; margin-bottom: -0px; ",
                                 sliderInput(
                                   "stackratio",
                                   "stackratio",
                                   min = 0.25,
                                   max = 5,
                                   value = 1.8,
                                   step = 0.1
                                 )
                             ),
                             
                             div(style = "margin-top:-20px; margin-bottom: -0px; ",
                                 sliderInput(
                                   "binwidth",
                                   "binwidth",
                                   min = 1,
                                   max = 100,
                                   value = 26,
                                   step = 1
                                 )
                             )
                    ),
                    
                    menuItem('Settings', tabName = 'settings', startExpanded = FALSE, width = "350px;", icon = icon("gear"),
                             
                             div(style = "margin-top:-30px; margin-bottom: -20px; ",
                                 selectInput(
                                   "method",
                                   label = "Method",
                                   choices = c(
                                     'dotdensity',
                                     'histodot'
                                   )
                                 )
                             ),
                             
                             div(style = "margin-top:-30px; margin-bottom: -20px; ",
                                 selectInput(
                                   "position",
                                   label = "Position",
                                   choices = c(
                                     'identity',
                                     'jitter',
                                     'dodge',
                                     'stack',
                                     'fill'
                                   )
                                 )
                             ),
                             
                             div(style = "margin-top:-30px; margin-bottom: -20px; ",
                                 selectInput(
                                   "stackdir",
                                   label = "stackdir",
                                   choices = c(
                                     'center',
                                     'up',
                                     'down',
                                     'centerwhole'
                                   )
                                 )
                             )
                    )
                    
                  )
                ),
                
                # dashboard body ####
                dashboardBody(
                  shinyjs::useShinyjs(),
                  shinycssloaders::withSpinner(
                    girafeOutput("mainplot", width = "100%", height = "100%"),
                    proxy.height = "500px",
                    size = 2
                  )
                )
  )
)

