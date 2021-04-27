# AutoReport Typed Report Panel Set

navlistPanel("Student Response Scoring",
             tabPanel("Score Summary",
                      tabsetPanel(
                              tabPanel("Score Distribution",
                                       textOutput("summaryStatement"),
                                       plotlyOutput("typedDistributionPlot"),
                                       tableOutput("displayRubric")
                              ),
                              tabPanel("Scoring Probabilities",
                                       uiOutput("chooseHistogram"),
                                       plotlyOutput("probHistogram"),
                                       textOutput("probStatement")
                              )
                      )    
             ),
             tabPanel("Responses Sorted by Rubric Bin",
                      textOutput("scoringTypedStatement"),
                      dataTableOutput(outputId="exampleResponses")
             ),
             "Analysis of Term Usage",
             tabPanel("Most Important Terms",
                      tabsetPanel(
                              tabPanel("Important Terms",
                                       uiOutput("chooseImportant"),
                                       plotlyOutput("importantTermsPlot"),
                                       textOutput("importantTermsStatement")
                              ),
                              tabPanel("Table Important Terms",
                                       textOutput("ngOverStatement"),
                                       dataTableOutput(outputId="overabundanceTable"),
                                       img(src="overabundanceDef.png")
                              )
                      )
             ),
             tabPanel("Web Diagrams of Important Terms",
                      textOutput("wdStatement"),
                      uiOutput("chooseWebDiagram"),
                      plotOutput("webDiagramPlot"),
                      uiOutput("webDiagramPredConf")
             ),
             tabPanel("Term Usage and Association Map",
                      textOutput("mapStatement"),
                      uiOutput("chooseTermMap"),
                      plotOutput("termMapPlot"),
                      uiOutput("chooseMinFreq")
             ),
             "Question Reference Material",
             tabPanel("Scoring Model Performance",
                      htmlOutput("cvDisclaimer"),
                      uiOutput("select_cv_level"),
                      tableOutput("modelListTable"),
                      verbatimTextOutput("cvDataOut")
                      # uiOutput("cvDataDisplay")
             )
)