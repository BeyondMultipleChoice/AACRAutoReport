#AACR Confirmatory Analysis UI
library(shiny)
library(rhandsontable)
library(DT)

shinyUI(fluidPage(
        navbarPage("AACR Confirmatory Analysis",
                   tabPanel("Question Data",
                            sidebarLayout(
                                    sidebarPanel(
                                            radioButtons("rubricType", label = "Rubric Type",
                                                         choices = list("Holistic" = "Typed", "Analytic" = "Binary"), 
                                                         selected = "Binary"),
                                            # radioButtons("ensembleMethod",label="Ensemble Method",
                                            #              choices = list("Simple Voting"=1,"Weighted Voting"=2, "L1 PLR"=3,
                                            #                             "L2 PLR"=4,"Weighted with CV"=5,"caret"=6,"SMOTE"=8,"3-gram"=9),#,"CVP with Coefs"=6,
                                            #                             # "CVP PLR"=7),
                                            #              selected=6),
                                            # radioButtons("ensembleMethod",label="Ensemble Method",
                                            #              choices = list("Simple Voting"=1,"Weighted Voting"=2, "L1 PLR"=3,
                                            #                             "L2 PLR"=4,"CV Rates + Prob"=5,"CVP with Coefs"=6,
                                            #                             "CVP PLR"=7),
                                            #              selected=1),
                                            fileInput('fileIn',"Input File"),
                                            uiOutput("chooseWorksheet"),
                                            uiOutput("chooseHeader"),
                                            uiOutput("choose_response_col"),
                                            uiOutput("choose_score_cols"),
                                            uiOutput("report_button"),
                                            tags$p(tags$br(),
                                                   tags$b("Acknowledgements:"), 
                                                   "This material is based upon work supported by the National Science Foundation (Grants 1323162 and 1347740). Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the supporting agencies.")
                                    ),
                                    mainPanel(
                                            tabsetPanel(type="tabs",
                                                        tabPanel("Preview",
                                                                 uiOutput("rowCount"),
                                                                 dataTableOutput(outputId="preview_datIn"),
                                                                 uiOutput("warningMessages")
                                                        ),
                                                        tabPanel("Pre-processing",
                                                                 h4("Response Formatting"),
                                                                 checkboxInput("stemming",label="Stemming",value=TRUE),
                                                                 radioButtons("chooseStopwords",
                                                                              label="Stopword Removal",
                                                                              choices = list("Remove Default Stopwords"="DEFAULT","No Stopword Removal"="NONE","Remove Custom Stopwords"="CUSTOM")),
                                                                 uiOutput("customStopwords"),
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
                                                        tabPanel("Advanced",
                                                                 h4("Feature Engineering"),
                                                                 checkboxGroupInput("ngramDepth",label="N-gram Depth",
                                                                                    choices=list("unigram"=1,"bigram"=2,"trigram"=3,"quadgram"=4),
                                                                                    selected=c(1,2)),
                                                                 numericInput("sparsity",label="Feature Matrix Sparsity",min=0.8,max=1.0,step=0.01,value=0.99),
                                                                 h4("Algorithm"),
                                                                 radioButtons("ensembleMethod",label="Ensemble Method",
                                                                              choices = list("Simple Voting"=1,"Weighted Voting"=2, "L1 PLR"=3,
                                                                                             "L2 PLR"=4,"Weighted with CV"=5),#,"caret"=6,"SMOTE"=8,"3-gram"=9),#,"CVP with Coefs"=6,
                                                                              # "CVP PLR"=7),
                                                                              selected=5),
                                                                 h4("Reporting"),
                                                                 checkboxInput("indAlgReport",label="Individual Algorithm Results",value=FALSE)
                                                        )
                                            )
                                    )
                            )
                   ),
                   tabPanel("Cross Validation Report",
                            sidebarLayout(
                                    sidebarPanel(
                                            uiOutput("select_cvLevel"),
                                            downloadButton("downloadPDF","Download PDF Report"),
                                            uiOutput("downloadData")
                                            # downloadButton("downloadExcel","Download Excel File")
                                    ),
                                    mainPanel(
                                            # uiOutput("cvTabs")
                                            tabsetPanel(type="tabs",
                                                        tabPanel("Summary",
                                                                 uiOutput('cvSubtitle'),
                                                                 tableOutput("modelListTable"),
                                                                 verbatimTextOutput("cvDataOut")                     
                                                        ),
                                                        tabPanel("Probability Distributions",
                                                                 # h4("intentionally blank")
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
                   ),
                   tabPanel("Hand Scored Report",
                            # uiOutput("displayQuestion"),
                            uiOutput("humanReportPanelSet")
                   ),
                   tabPanel("Machine Scored Report",
                            # uiOutput("displayQuestion"),
                            uiOutput("machineReportPanelSet")
                   )
        )
))