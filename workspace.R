library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(STRINGdb)
library(RColorBrewer)
library(plyr)

############################################
# http://localhost:5402/admin/w/73f2fb84b22bb5542af0c50c9c09f92b/ds/a242a591-99ed-431c-b99a-6ec6a577ade4
# http://localhost:5402/admin/w/73f2fb84b22bb5542af0c50c9c09f92b/ds/a242a591-99ed-431c-b99a-6ec6a577ade4
# http://localhost:5402/admin/w/73f2fb84b22bb5542af0c50c9c11a566/ds/44860242-b484-4ea8-872e-527192e8aca1
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "44860242-b484-4ea8-872e-527192e8aca1",
                   workflowId = "73f2fb84b22bb5542af0c50c9c11a566")
  return(ctx)
}
####
############################################

ui <- shinyUI(fluidPage(
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
  
))

server <- shinyServer(function(input, output, session) {
  dataInput <- reactive({
    getValues(session)
  })
  
  
  output$ppiPlot <- renderUI({
    plotOutput("plot1",
               height = input$plotHeight,
               width = input$plotWidth)
  })
  
  output$plot1 <- renderPlot({
    p()
  })
  
  p <- function() {
    data_obj <- dataInput()
    
    par(mar = c(1, 1, 1, 1))
    
    data_obj$string_db$plot_network(
      string_ids = data_obj$string_db_map$STRING_id,
      payload_id = data_obj$payload_id,
      required_score = input$scoreThres,
      add_summary = FALSE,
      network_flavor = input$networkEdge,
    )
    
    if (input$applyLabels) {
      if (input$labelType == "discrete" &
          !is.numeric(data_obj$df$color)) {
        legend(
          "bottomleft",
          bg = "transparent",
          legend = data_obj$legend$labels,
          col = data_obj$legend$colors,
          pch = c(19),
          bty = "n",
          pt.cex = 2,
          cex = 1.0,
          text.col = "black",
          horiz = F
        )
      } else {
        lgd_ = rep(NA, 11)
        lgd_[c(1, 6, 11)] = c(min(data_obj$df$color),
                              mean(data_obj$df$color),
                              max(data_obj$df$color))
        legend(
          "bottomleft",
          bg = "transparent",
          legend = lgd_,
          fill = colorRampPalette(c("red", "blue"))(11),
          border = NA,
          y.intersp = 0.5,
          cex = 1,
          pt.cex = 2,
          bty = "n",
        )
      }
    }
    
    
  }
  
  output$downloadPlot <- downloadHandler(
    file = "save.png" ,
    # variable with filename
    content = function(file) {
      graphics.off()
      par(mar = c(1, 1, 1, 1))
      png(
        file = file,
        res = 100,
        width = input$plotWidth,
        height = input$plotHeight
      )
      p()
      dev.off()
    }
  )
  
  
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
  
  string_db <- STRINGdb$new(version = "11.0",
                            species = 9606,
                            score_threshold = 400) # standard
  
  string_db_map <- string_db$map(data.frame(df),
                                 "id",
                                 removeUnmappedRows = TRUE)
  
  legend_df = data.frame()
  
  print(length(unique(df$color)))
  print(length(unique(df$color)) <= 30 & is.numeric(df$color))
  
  if (colors != 0) {
    if (length(unique(df$color)) <= 30 & !is.numeric(df$color)) {
      qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
      col_vector = unlist(mapply(
        brewer.pal,
        qual_col_pals$maxcolors,
        rownames(qual_col_pals)
      ))
      
      values = sort(unique(df$color))
      colors = sample(col_vector, length(unique(df$color)))
      
      colors_plot = mapvalues(df$color, from = values, to = colors)
      
      df$color_hex = colors_plot
      
      legend_df = data.frame(labels = values, colors = colors)
      
    } else {
      ii <- cut(df$color,
                breaks = seq(min(df$color),
                             max(df$color),
                             len = 11),
                include.lowest = TRUE)
      
      colors_plot <- colorRampPalette(c("red", "blue"))(11)[ii]
      df$color_hex = colors_plot
    }
  }
  
  if (colors == 0) {
    payload_id = string_db$post_payload(string_db_map$STRING_id)
  } else {
    payload_id = string_db$post_payload(string_db_map$STRING_id,
                                        colors = df$color_hex)
  }
  
  values = list(
    string_db = string_db,
    string_db_map = string_db_map,
    payload_id = payload_id,
    df = df,
    legend = legend_df
  )
  
  return(values)
}

runApp(shinyApp(ui, server))
