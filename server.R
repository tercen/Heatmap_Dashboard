library(shiny)
library(tercen)
library(tidyverse)
library(ComplexHeatmap)
library(reshape2)
library(matlab)
library(viridisLite)

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
  
  
  xLim = reactive({
    q = getProps(session)
    dataIn() %>%
      summarise(xMin = quantile(.y, q[1]), xMax = quantile(.y, q[2]))
  })
  
  
  observe({
    
    cdf = colData() %>% as.data.frame()
    rdf = rowData() %>% as.data.frame()
    
    updateSelectInput(session, "columnlab", choices = colnames(cdf), selected = colnames(cdf)[length(colnames(cdf))])
    updateSelectInput(session, "xannotation", choices = colnames(cdf))
    updateSelectInput(session, "rowlab", choices = colnames(rdf), selected = colnames(rdf)[length(colnames(rdf))])
    updateSelectInput(session, "yannotation", choices = colnames(rdf))
    
    lim = xLim()
    updateNumericInput(session, "dvmin", value = as.numeric(lim$xMin))
    updateNumericInput(session, "dvmax", value = as.numeric(lim$xMax))
    updateNumericInput(session, "csmin", value = as.numeric(lim$xMin))
    updateNumericInput(session, "csmax", value = as.numeric(lim$xMax))
    
    colAnnotation = reactive({
      if (!is.null(input$xannotation)){
        haCol = cdf %>%
          select(all_of(input$xannotation)) %>%
          HeatmapAnnotation()
      } else {
        haCol  = NULL
      }
      haCol
    })
    
    
    
    rowAnn = reactive({
      if (!is.null(input$yannotation)){
        haRow = rdf %>%
          select(all_of(input$yannotation)) %>%
          rowAnnotation()
        
      } else {
        haRow  = NULL
      }
      haRow
    })
    
    colorPalette = reactive({
      
      if(input$paltype == "Divergent"){
        cp = circlize::colorRamp2(breaks = c(input$dvmin, input$dvmid, input$dvmax),
                                  colors = c(input$dcmin, input$dcmid, input$dcmax))
      } else if (input$paltype == "Continuous"){
        clr = cmaps(input$cmap)
        breaks = seq(input$csmin, input$csmax, length.out = length(clr))
        cp = circlize::colorRamp2(breaks = breaks, colors = clr)
      }
      cp
    })
    
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
                   row_names_gp = gpar(fontsize = input$rlsize),
                   top_annotation = colAnnotation(),
                   col = colorPalette())
      
      hm = hm + rowAnn()
      
      
      
      
      
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

getProps = function(session){
  ctx = getCtx(session)
  q = as.numeric(c(ctx$op.value("qMin"), ctx$op.value("qMax")))
  q = c(0.01, 0.99)
}
  
cmaps = function(type, n = 64){
  if(type == "viridis"){
    c = viridisLite::viridis(n)
  } else if (type == "jet"){
    c = matlab::jet.colors(n)
  }
  return(c)
}





