#AutoReport Binary Report Panels

navlistPanel("Student Response Scoring",
             tabPanel("Score Summary",
                      tabsetPanel(
                              tabPanel("Score Distribution",
                                       textOutput("summaryStatement"),
                                       plotlyOutput("binaryDistributionPlot"),
                                       tableOutput("displayRubric")
                              ),
                              tabPanel("Scoring Probabilities",
                                       uiOutput("chooseHistogram"),
                                       plotlyOutput("probHistogram"),
                                       textOutput("probStatement")
                              )
                      )    
             ),
             #                                      
             tabPanel("Rubric Bin Associations",
                      tabsetPanel(
                              tabPanel("Association Web Diagram",
                                       textOutput("coWDStatement"),
                                       visNetworkOutput("cooccurWDPlot")
                                       # plotOutput("cooccurWDPlot")
                              ),
                              tabPanel("Association Filters",
                                       plotlyOutput("assocFracPlot"),
                                       uiOutput("responseFilters2")
                              ),
                              tabPanel("Association Frequency",
                                       textOutput("coFigStatement"),
                                       plotlyOutput("cooccurFreqPlot"),
                                       uiOutput("chooseCooccurLev")
                              ),
                              
                              tabPanel("Association Odds Ratios",
                                       textOutput("rlCoStatement"),
                                       plotOutput("condFreqPlot"),
                                       uiOutput("chooseCondLev"),
                                       img(src="condFreqDef.png")
                              )
                      )
                      
             ),                    
             tabPanel("Scored Responses and Probabilities",
                      tabsetPanel(
                              tabPanel("Sorted by Rubric Bin",
                                       textOutput("scoringBinaryStatement"),
                                       dataTableOutput(outputId="exampleResponses")                 
                              ),
                              tabPanel("Sorted by Response",
                                       uiOutput("responseFilters"),
                                       dataTableOutput(outputId="filteredResponses")
                              )
                      )
             ),
             "Analysis of Term Usage",
             tabPanel("Most Important Terms",
                      tabsetPanel(
                              tabPanel("Important Terms",
                                       uiOutput("chooseImportant"),
                                       plotlyOutput("importantTermsPlot"),
                                       textOutput("importantTermsStatement")
                              ),
                              tabPanel("Table of Important Terms",
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