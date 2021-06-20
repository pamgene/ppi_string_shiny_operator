library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(STRINGdb)

############################################
# http://localhost:5402/admin/w/73f2fb84b22bb5542af0c50c9c09f92b/ds/a242a591-99ed-431c-b99a-6ec6a577ade4
# http://localhost:5402/admin/w/73f2fb84b22bb5542af0c50c9c09f92b/ds/a242a591-99ed-431c-b99a-6ec6a577ade4
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "a242a591-99ed-431c-b99a-6ec6a577ade4",
                   workflowId = "73f2fb84b22bb5542af0c50c9c09f92b")
  return(ctx)
}
####
############################################

ui <- shinyUI(fluidPage(
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
  
))

server <- shinyServer(function(input, output, session) {
  dataInput <- reactive({
    getValues(session)
  })
  
  
  output$reacOut <- renderUI({
    plotOutput("main.plot",
               height = input$plotHeight,
               width = input$plotWidth)
  })
  
  output$plot1 <- renderImage({
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext = '.png')
    
    data_obj <- dataInput()
    
    matrix = data_obj$string_db$get_png(
      string_ids = data_obj$string_db_map$STRING_id,
      payload_id = data_obj$payload_id
    )
    
    writePNG(matrix, outfile)
    
    # Return a list
    list(
      src = outfile,
      alt = "Protein-Protein Network",
      height = input$plotHeight,
      width = input$plotWidth
    )
  }, deleteFile = TRUE)
  
  
})

getValues <- function(session) {
  ctx <- getCtx(session)
  
  df = ctx %>% rselect()
  
  colors <- 0
  if (length(ctx$colors)) {
    colorsdf = ctx$select(c('.ri', ctx$colors[[1]]))
    colors = colorsdf %>%
      group_by(.ri) %>%
      slice(1) %>%
      pull(ctx$colors[[1]])
  }
  
  if (!all(is.numeric(colors)))
    colors = as.factor(colors)
  
  df = ctx %>%
    select(.y, .ci, .ri) %>%
    pivot_wider(
      values_from = .y,
      names_from = .ci,
      values_fn = mean,
      names_sort =
        TRUE
    ) %>%
    mutate(id = ctx$rselect()[[1]]) %>%
    mutate(color = colors)
  
  
  string_db <- STRINGdb$new(version = "11.0", species = 9606)
  string_db_map <- string_db$map(data.frame(df),
                                 colnames(df)[1],
                                 removeUnmappedRows = TRUE)
  
  ii <- cut(df$color,
            breaks = seq(min(df$color),
                         max(df$color),
                         len = 100),
            include.lowest = TRUE)
  
  colors_plot <- colorRampPalette(c("red", "blue"))(99)[ii]
  
  payload_id = string_db$post_payload(string_db_map$STRING_id,
                                      colors = colors_plot)
  
  values = list(
    string_db = string_db,
    string_db_map = string_db_map,
    payload_id = payload_id,
    df = df
  )
  
  return(values)
}

runApp(shinyApp(ui, server))
