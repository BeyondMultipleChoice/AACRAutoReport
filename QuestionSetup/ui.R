#AACR Question Setup UI
library(shiny)
library(rhandsontable)
library(DT)

shinyUI(fluidPage(
        navbarPage("AACR AutoReporter Setup",
                   tabPanel("Question Data",
                            sidebarLayout(
                                    sidebarPanel(
                                            radioButtons("rubricType", label = "Rubric Type",
                                                         choices = list("Holistic" = "Typed", "Analytic" = "Binary"), 
                                                         selected = "Typed"),
                                            fileInput('fileIn',"Input File"),
                                            uiOutput("chooseWorksheet"),
                                            uiOutput("chooseHeader"),
                                            uiOutput("choose_response_col"),
                                            uiOutput("choose_score_cols"),
                                            tags$p(tags$br(),
                                                   tags$b("Acknowledgements:"), 
                                                   "This material is based upon work supported by the National Science Foundation (Grants 1323162 and 1347740). Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the supporting agencies.")
                                            
                                    ),
                                    mainPanel(
                                            tabsetPanel(type="tabs",
                                                        tabPanel("Preview",
                                                                 uiOutput("rowCount"),
                                                                 dataTableOutput(outputId="preview_datIn")
                                                        ),
                                                        tabPanel("Pre-processing",
                                                                 h4("Response Formatting"),
                                                                 checkboxInput("stemming",label="Stemming",value=TRUE),
                                                                 radioButtons("numberFormat",label="Number Format",
                                                                              choices=list("Remove numbers"="remove",
                                                                                           "Preserve numbers"="leave",
                                                                                           "Convert digits to words"="digitsToWords"),
                                                                              selected="remove"),
                                                                 textInput("preserveSymbol",label="Symbols to exclude from removal",
                                                                           placeholder="Enter symbols with no seperating characters"),
                                                                 h4("Original:"),
                                                                 uiOutput("originalText"),
                                                                 h4("Pre-processed:"),
                                                                 uiOutput("preProText")
                                                        ),
                                                        tabPanel("Algorithm",
                                                                 radioButtons("ensembleMethod",
                                                                              label="Ensemble Method",
                                                                              # choices = list("Weighted Voting: Prob + CV"=1,
                                                                              #                "Weighted Voting: Prob"=2,
                                                                              #                "Simple Voting"=3,
                                                                              #                "L1 PLR"=4,
                                                                              #                "L2 PLR"=5),
                                                                              choices = list("Weighted Voting: Prob + CV"=1,
                                                                                             #"Weighted Voting: Prob"=2,
                                                                                             "Simple Voting"=3),
                                                                                             #"L1 PLR"=4,
                                                                                             #"L2 PLR"=5),
                                                                              selected = 1
                                                                              )
                                                                 )
                                            )
                                    )
                            )
                   ),
                   tabPanel("Question Text and Rubric Description",
                            sidebarLayout(
                                    sidebarPanel(
                                            uiOutput("questionStatusQuerry1"),
                                            uiOutput("questionStatusQuerry2"),
                                            uiOutput("questionStatusQuerry3"),
                                            uiOutput("report_button")
                                    ),
                                    mainPanel(
                                            uiOutput("metadataInputPanel")
                                    )
                                    
                            )
                   ),
                   tabPanel("Cross Validation Report",
                            sidebarLayout(
                                    sidebarPanel(
                                            uiOutput("select_cvLevel"),
                                            downloadButton("downloadPDF","Download PDF Report"),
                                            uiOutput("downloadData")
                                    ),
                                    mainPanel(
                                            tabsetPanel(type="tabs",
                                                        tabPanel("Summary",
                                                                 uiOutput('cvSubtitle'),
                                                                 tableOutput("modelListTable"),
                                                                 verbatimTextOutput("cvDataOut")                     
                                                        ),
                                                        tabPanel("Probability Distributions",
                                                                 plotOutput("agreementAll"),
                                                                 plotOutput("agreementByType")
                                                        ),
                                                        tabPanel("Expected Disagreement",
                                                                 h4("Expected Disagreement by probability"),
                                                                 tableOutput("expectAgreement")
                                                        )
                                            )
                                    )
                            )
                   )
        )
))