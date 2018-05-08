library(shiny)
library(shinySignals)   # devtools::install_github("hadley/shinySignals")
library(dplyr)
library(shinydashboard)
library(bubbles)        # devtools::install_github("jcheng5/bubbles")
source("helpers.R")

iatResponses <- list("Not Applicable" = 0,
                     "Rarely" = 1,
                     "Occasionally" = 2,
                     "Frequently" = 3,
                     "Often" = 4,
                     "Always" = 5)

