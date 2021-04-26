library(shiny)
library(tercen)
library(dplyr)
library(tidyr)


############################################

source("ui.R")
source("server.R")

options("tercen.workflowId"= "65d5ea34baf8f90db040b8f7f5135974")
options("tercen.stepId"= "f8b922fd-9c6f-46ee-a35b-c19de81d6080")

runApp(shinyApp(ui, server))  




