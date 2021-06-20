library(shiny)
library(shinyjs)

ui <- shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    tags$script(
      HTML(
        'setInterval(function(){ $("#hiddenButton").click(); }, 1000*30);'
      )
    ),
    tags$footer(shinyjs::hidden(
      actionButton(inputId = "hiddenButton", label = "hidden")
    )),
    
    titlePanel("Protein-Protein Interaction Network (STRING Database)"),
    
    sidebarPanel(
      h4('Options'),
      h5('Plot'),
      sliderInput("plotWidth", "Plot width (px)", 200, 2000, 500),
      sliderInput("plotHeight", "Plot height (px)", 200, 2000, 500),
      h5('Network'),
      sliderInput('scoreThres', 'Score threshold', 1, 1000, 400),
    ),
    
    mainPanel(plotOutput("plot1"))
    
  )
)