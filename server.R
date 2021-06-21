library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(STRINGdb)
library(png)
library(RColorBrewer)
library(plyr)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

shinyServer(function(input, output, session) {
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
    )
    
    legend(
      "bottomleft",
      title = 'Colors',
      bg = "transparent",
      legend = data_obj$legend$labels,
      col = data_obj$legend$colors,
      pch = c(19),
      pt.cex = 2,
      cex = 1.0,
      text.col = "black",
      horiz = F
    )
    
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
  
  legend_df = data.frame()
  
  print(unique(df$color))
  
  if (length(unique(df$color)) <= 10) {
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
                           len = 100),
              include.lowest = TRUE)
    
    colors_plot <- colorRampPalette(c("red", "blue"))(99)[ii]
    df$color_hex = colors_plot
  }
  
  
  payload_id = string_db$post_payload(string_db_map$STRING_id,
                                      colors = df$color_hex)
  
  values = list(
    string_db = string_db,
    string_db_map = string_db_map,
    payload_id = payload_id,
    df = df,
    legend = legend_df
  )
  
  return(values)
}
