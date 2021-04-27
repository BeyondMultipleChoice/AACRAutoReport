#AutoReport Binary Report Panels

navlistPanel("Student Response Scoring",
             tabPanel("Score Summary",
                      # tabsetPanel(
                              tabPanel("Score Distribution",
                                       # textOutput("summaryStatement"),
                                       plotlyOutput("binaryDistributionPlotHS")#,
                                       # tableOutput("displayRubric")
                              )#,
                              # tabPanel("Scoring Probabilities",
                              #          uiOutput("chooseHistogram"),
                              #          plotlyOutput("probHistogram"),
                              #          textOutput("probStatement")
                              # )
                      # )    
             ),
             #                                      
             tabPanel("Rubric Bin Associations",
                      tabsetPanel(
                              tabPanel("Association Web Diagram",
                                       textOutput("coWDStatementHS"),
                                       visNetworkOutput("cooccurWDPlotHS")
                                       # plotOutput("cooccurWDPlot")
                              ),
                              tabPanel("Association Filters",
                                       plotlyOutput("assocFracPlotHS"),
                                       uiOutput("responseFilters2HS")
                              ),
                              tabPanel("Association Frequency",
                                       textOutput("coFigStatementHS"),
                                       plotlyOutput("cooccurFreqPlotHS"),
                                       uiOutput("chooseCooccurLevHS")
                              ),
                              
                              tabPanel("Association Odds Ratios",
                                       textOutput("rlCoStatementHS"),
                                       plotOutput("condFreqPlotHS"),
                                       uiOutput("chooseCondLevHS"),
                                       img(src="condFreqDef.png")
                              )
                      )
                      
             ),                    
             tabPanel("Scored Responses and Probabilities",
                      tabsetPanel(
                              tabPanel("Sorted by Rubric Bin",
                                       textOutput("scoringBinaryStatementHS"),
                                       dataTableOutput(outputId="exampleResponsesHS")                 
                              ),
                              tabPanel("Sorted by Response",
                                       uiOutput("responseFiltersHS"),
                                       dataTableOutput(outputId="filteredResponsesHS")
                              )
                      )
             ),
             "Analysis of Term Usage",
             tabPanel("Most Important Terms",
                      tabsetPanel(
                              tabPanel("Important Terms",
                                       uiOutput("chooseImportantHS"),
                                       plotlyOutput("importantTermsPlotHS"),
                                       textOutput("importantTermsStatementHS")
                              ),
                              tabPanel("Table of Important Terms",
                                       textOutput("ngOverStatementHS"),
                                       dataTableOutput(outputId="overabundanceTableHS"),
                                       img(src="overabundanceDef.png")
                              )
                      )
             ),
             tabPanel("Web Diagrams of Important Terms",
                      textOutput("wdStatementHS"),
                      uiOutput("chooseWebDiagramHS"),
                      plotOutput("webDiagramPlotHS"),
                      uiOutput("webDiagramPredConfHS")
             ),
             tabPanel("Term Usage and Association Map",
                      textOutput("mapStatementHS"),
                      uiOutput("chooseTermMapHS"),
                      plotOutput("termMapPlotHS"),
                      uiOutput("chooseMinFreqHS")
             )#,
             # "Question Reference Material",
             # tabPanel("Scoring Model Performance",
             #          htmlOutput("cvDisclaimer"),
             #          uiOutput("select_cv_level"),
             #          tableOutput("modelListTable"),
             #          verbatimTextOutput("cvDataOut")
             #          # uiOutput("cvDataDisplay")
             # )
)