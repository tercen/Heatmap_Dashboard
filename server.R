library(shiny)
library(tercen)
library(tidyverse)
library(ComplexHeatmap)
library(reshape2)

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

server <- shinyServer(function(input, output, session) {
  
  dataIn = reactive({
    getValues(session)
  })
  
  rowData = reactive({
    getRows(session)
  })
  
  colData = reactive({
    getCols(session)
  })
  
  colAnnotation = reactive({

  })
  
  
  observe({
    cdf = colData() %>% as.data.frame()
    updateSelectInput(session, "columnlab", choices = colnames(cdf), selected = colnames(cdf)[length(colnames(cdf))])
    rdf = rowData() %>% as.data.frame()
    updateSelectInput(session, "rowlab", choices = colnames(rdf), selected = colnames(rdf)[length(colnames(rdf))])
    
    output$heatmap = renderPlot({
      X = dataIn() %>%
        acast(.ri ~ .ci , value.var = ".y")

      if (input$columnlab != ""){
        clab = cdf %>% 
          select(str = any_of(input$columnlab))
        colnames(X) = clab$str
      }
      if (input$rowlab != ""){
        rlab = rdf %>% 
          select(str = any_of(input$rowlab))
        rownames(X) = rlab$str
      }
    
      
      hm = Heatmap(X, 
                   cluster_columns = input$clusterx,
                   cluster_rows = input$clustery,
                   show_column_names = input$doclab,
                   column_names_gp = gpar(fontsize = input$clsize),
                   show_row_names = input$dorlab,
                   row_names_gp = gpar(fontsize = input$rlsize))
                    
      
      draw(hm)
    })
    
    
  })
  
})


getValues <- function(session){
  ctx <- getCtx(session)
  df <- ctx %>% 
    select(.y, .ri, .ci)
}

getRows <- function(session){
  ctx <- getCtx(session)
  ctx %>% rselect()
}

getCols = function(session){
  ctx <- getCtx(session)
  ctx %>% cselect()
}





