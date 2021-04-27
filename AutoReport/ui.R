#UI file for shiny AACR report generation app

library(shiny)
library(DT)

shinyUI(fluidPage(
  
  #MOdif - horizontal line in black 8-18-2020, see primaryPanelSet.R
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
        uiOutput("topLevelPanelSet")
))