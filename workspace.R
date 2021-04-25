library(shiny)
library(tercen)
library(dplyr)
library(tidyr)


############################################

source("ui.R")
source("server.R")

options("tercen.workflowId"= "43f6de57a8befbf60bbcb444f30062e0")
options("tercen.stepId"= "8d2005ed-1a75-456e-bb49-870b91fadfb3")

runApp(shinyApp(ui, server))  
