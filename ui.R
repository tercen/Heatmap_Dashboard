library(shiny)
library(shinydashboard)
library(shinyjs)
library(colourpicker)

cp1 <- conditionalPanel(
  condition = "input.side == 'axo'",
  fluidRow(
    box(width = 4,
        selectInput("clusterx",label = "column order", choices = c("cluster", "native", "sort", "correlate")),
        checkboxInput("doclab", label = "columns labels", value = TRUE),
        conditionalPanel(condition = "input.doclab == 1",
                         selectInput("columnlab", label = "", choices = ""),
                         sliderInput("clsize", label = "", min = 0, max = 16, step = 1, value = 9)
        )
    )
    ,
    box(width = 4,
        selectInput("clustery", label = "row order", choices = c("cluster", "native", "sort", "correlate")),
        checkboxInput("dorlab", label = "row labels", value = TRUE),
        conditionalPanel(condition = "input.dorlab == 1",
                         selectInput("rowlab", label = "", choices = ""),
                         sliderInput("rlsize", label = "", min = 0, max = 16, step = 1, value = 9)
        )
        
    )
  ))

cp2 <- conditionalPanel(
  condition = "input.side == 'axann'",
  fluidRow(
    box(width = 4,
        selectInput("xannotation", label = "Column Annotation",choices = "", multiple = TRUE)
    ),
    box(width = 4,
        selectInput("yannotation", label = "Row Annotation", choices = "", multiple = TRUE)
    )
  )
)

cpcp = conditionalPanel(
  condition = "input.paltype == 'Divergent'",
  fluidRow(
    box(width = 2, numericInput("dvmax", label = "Max", value = 1),
        numericInput("dvmid", label = "Mid", value = 0),
        numericInput("dvmin", label = "Min", value = -1)),
    box(width = 1, colourInput("dcmax", label = "", value = "#7a040e", showColour = "background"),
        colourInput("dcmid", label = "", value = "#dce37d", showColour = "background"),
        colourInput("dcmin", label = "", value = "#04035e", showColour = "background")
        )
  )
)

cpcs = conditionalPanel(
  condition = "input.paltype == 'Continuous'",
  fluidRow(
    box(width = 2, 
        selectInput("cmap", label = "Scale name", choices = c("viridis", "jet")),
        numericInput("csmax", label = "Max", value = 1),
        numericInput("csmin", label = "Min", value = 0))
  )
)

cp3 <- conditionalPanel(
  condition = "input.side == 'palette'",
  fluidRow(
    box(width = 3,
        selectInput("paltype", label = "Palette type",choices = c("Divergent", "Continuous"), selected  = "Continuous")
    ),
    cpcp,
    cpcs
  )
)

ui = shinyUI
( 
  fluidPage
  ( 
    
    ui <- dashboardPage
    (
      dashboardHeader(title = "Heatmap Dash"),
      dashboardSidebar(
        sidebarMenu(id = "side",
                    menuItem("Home", tabName = "home", icon = icon("home")),
                    menuItem("Rows & Columns", tabName = "axo", icon = icon("sort")),
                    menuItem("Axis annotation", tabName = "axann", icon = icon("th")),
                    menuItem("Palette", tabName = "palette", icon = icon("palette"))
                    
        ),
        tags$hr(),
        HTML(paste("<center><h5>Click to send the ordering data to Tercen</h5>", actionButton("button", "Transform data")),"</center>")
      )
      ,
      dashboardBody(
        fluidRow(cp1, cp2, cp3),
        plotOutput("heatmap",height = "600px")
        
        
      )
    ),
    shinyjs::useShinyjs()
    
  )
)
