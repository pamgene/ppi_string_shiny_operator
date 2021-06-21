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
      h3('Options'),
      h4('Plot'),
      sliderInput("plotWidth", "Plot width (px)", 200, 2000, 500),
      sliderInput("plotHeight", "Plot height (px)", 200, 2000, 500),
      h4('Network'),
      sliderInput('scoreThres', 'Score threshold', 1, 1000, 400),
      selectInput(
        'networkEdge',
        'Edges',
        choices = c(
          'Evidence' = 'evidence',
          'Confidence' = 'confidence',
          'Actions' = 'actions'
        ),
        selected = 'Evidence'
      ),
      h4('Legend and Labels'),
      checkboxInput("applyLabels", "Show legend", FALSE),
      selectInput(
        'labelType',
        'Label type',
        choices = c("Discrete" = "discrete",
                    "Continious" = "cont"),
        selected = "Discrete"
      ),
      downloadButton('downloadPlot', 'Download Plot')
    ),
    
    mainPanel(uiOutput("ppiPlot"))
    
  )
)