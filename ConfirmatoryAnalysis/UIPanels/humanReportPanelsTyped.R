# AutoReport Typed Report Panel Set

navlistPanel("Student Response Scoring",
             tabPanel("Score Summary",
                      # tabsetPanel(
                      tabPanel("Score Distribution",
                               # textOutput("summaryStatement"),
                               plotlyOutput("typedDistributionPlotHS")#,
                               # tableOutput("displayRubric")
                               )#,
                              # tabPanel("Scoring Probabilities",
                              #          uiOutput("chooseHistogram"),
                              #          plotlyOutput("probHistogram"),
                              #          textOutput("probStatement")
                              # )
                      # )    
             ),
             tabPanel("Responses Sorted by Rubric Bin",
                      textOutput("scoringTypedStatementHS"),
                      dataTableOutput(outputId="exampleResponsesHS")
             ),
             "Analysis of Term Usage",
             tabPanel("Most Important Terms",
                      tabsetPanel(
                              tabPanel("Important Terms",
                                       uiOutput("chooseImportantHS"),
                                       plotlyOutput("importantTermsPlotHS"),
                                       textOutput("importantTermsStatementHS")
                              ),
                              tabPanel("Table Important Terms",
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