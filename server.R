library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(STRINGdb)
library(png)

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
