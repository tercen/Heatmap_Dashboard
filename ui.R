library(shiny)
library(shinydashboard)
library(shinyjs)
cp1 <- conditionalPanel(
  condition = "input.side == 'axo'",
  fluidRow(
    box(width = 4,
        checkboxInput("clusterx", label = "cluster columns", value = FALSE),
        checkboxInput("doclab", label = "columns labels", value = TRUE),
        conditionalPanel(condition = "input.doclab == 1",
                         selectInput("columnlab", label = "", choices = ""),
                         sliderInput("clsize", label = "", min = 0, max = 16, step = 1, value = 9)
        )
    )
    ,
    box(width = 4,
        checkboxInput("clustery", label = "cluster rows", value = FALSE),
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

ui = shinyUI
( 
  fluidPage
  ( 
    
    ui <- dashboardPage
    (
      dashboardHeader(title = "Heatmap Plus"),
      dashboardSidebar(
        sidebarMenu(id = "side",
                    menuItem("Home", tabName = "home", icon = icon("home")),
                    menuItem("Rows & Columns", tabName = "axo", icon = icon("sort")),
                    menuItem("Axis annotation", tabName = "axann", icon = icon("th")),
                    menuItem("Palette", tabName = "palette", icon = icon("palette"))
                    
        )
      )
      ,
      dashboardBody(
        fluidRow(cp1, cp2),
        plotOutput("heatmap",height = "600px")
        
        
      )
    ),
    shinyjs::useShinyjs()
    
  )
)
