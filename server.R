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
    
    Xsorted = reactive({
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
      
      xOrder <- yOrder <- NULL
      if (input$clusterx == "native") {
        xOrder <- seq(ncol(X))
      } else if (input$clusterx == "sort") {
        xOrder = order(apply(X,2,mean))
        X = X[, xOrder]
      } else if (input$clusterx == "correlate"){
        if (is.null(rowAnn())){
          stop("A row annotation is required when using correlate as ordering method for columns.")
        }
        df.row = rdf %>%
          select(vRow = all_of(input$yannotation[1]))
        
        xOrder = order(apply(X,2, function(x) cor(x, y = df.row$vRow %>% as.factor() %>% as.numeric())))
        X = X[, xOrder]
      }
      
      if (input$clustery == "native") {
        yOrder <- seq(nrow(X))
      } else if (input$clustery == "sort"){
        yOrder = order(apply(X,1,mean))
        X = X[yOrder,]
      } else if (input$clustery == "correlate"){
        if(is.null(colAnnotation())){
          stop("A column annotation is required when using correlate as ordering method for rows.")
        }
        df.col = cdf %>%
          select(vCol = all_of(input$xannotation[1]))
        
        yOrder = order(apply(X,1, function(x) cor(x, y = df.row$vCol %>% as.factor() %>% as.numeric())))
        X = X[yOrder,]
      }
      
      return(list(X = X, xorder = xOrder, yorder = yOrder))
    })
    
    getHeatmap <- reactive({
      hm = Heatmap(Xsorted()$X, 
                   cluster_columns = input$clusterx == "cluster",
                   cluster_rows = input$clustery == "cluster",
                   show_column_names = input$doclab,
                   column_names_gp = gpar(fontsize = input$clsize),
                   show_row_names = input$dorlab,
                   row_names_gp = gpar(fontsize = input$rlsize),
                   top_annotation = colAnnotation(),
                   col = colorPalette())
      hm + rowAnn()
      draw(hm)
    })
    
    getOrderedData <- reactive({
      Xsorted <- Xsorted()
      xOrder  <- Xsorted$xorder
      yOrder  <- Xsorted$yorder
      if (input$clusterx == "cluster") {
        hm = getHeatmap()
        xOrder <- column_order(hm)
      }
      if (input$clustery == "cluster") {
        hm = getHeatmap()
        yOrder <- row_order(hm)
      }
      print(xOrder)
      print(yOrder)
      return(list(xorder = xOrder, yorder = yOrder))
    })
    
    output$heatmap = renderPlot({
      getHeatmap()
    })

    observeEvent(input$button, {
      shinyjs::disable("button")
      
      ctx  <- getCtx(session)
      data <- getReturnData(session, getOrderedData())
      if (!is.null(data)) {
        data %>% ctx$save()
      }
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
  if(!is.null(ctx$op.value("qMin"))){
    qMin = ctx$op.value("qMin") %>% as.numeric()
  }
  else{
    qMin = 0.01
  }
  if(!is.null(ctx$op.value("qMax"))){
    qMax = ctx$op.value("qMax") %>% as.numeric()
  }
  else{
    qMax = 0.99
  }
  q = c(qMin, qMax)
}

cmaps = function(type, n = 64){
  if(type == "viridis"){
    c = viridisLite::viridis(n) 
  } else if (type == "jet"){
    c = matlab::jet.colors(n)
  }
  return(c)
}

getReturnData <- function(session, ordered_data) {
  result  <- NULL
  ctx     <- getCtx(session)
  corder0 <- ordered_data$xorder 
  rorder0 <- ordered_data$yorder
  
  if (!is.null(corder0) && !is.null(rorder0)) {
    ci <- seq(from = 0, to = length(corder0) - 1)
    ri <- seq(from = 0, to = length(rorder0) - 1)
    
    corder <- as.double(ci)
    rorder <- as.double(ri)
    
    ci <- ci[corder0]
    ri <- ri[rorder0]
    
    cresult <- data.frame(
      .ci = ci,
      corder = corder
    ) %>% ctx$addNamespace()
    
    rresult <- data.frame(
      .ri = ri,
      rorder = rorder
    ) %>% ctx$addNamespace()
    
   result <- list(cresult, rresult)
  }
  result
}
