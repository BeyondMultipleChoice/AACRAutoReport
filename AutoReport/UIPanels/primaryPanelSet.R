#Auto Report Main Panel Set
navbarPage("AACR AutoReport",
           tabPanel("Data Upload",
                    sidebarLayout(
                            sidebarPanel(
                                    fileInput('fileIn',"Input File"),
                                    uiOutput("chooseWorksheet"),
                                    uiOutput("exampleButton_ui"),
                        
                                    uiOutput("choose_question"),
                                    uiOutput("chooseIDCol"),
                                    uiOutput("choose_response_col"),
                                    
                                    ####################### MODIF 8-18-2020: optional drop-down menu info
                                    tags$hr(),
                                    uiOutput("instrStatus"),
                                    uiOutput("adminIncentive"),
                                    uiOutput("adminCondition"),
                                    dateInput('adminDate',
                                              label = 'Date administered (optional)',
                                              format = "yyyy-mm-dd",
                                              value=NA
                                    ),
                                    tags$hr(),
                                    
                                    ########################
                                    
                                    
                                    
                                    textInput("emailString",label="Send report to",placeholder = "Instructor Email Address"),
                                    uiOutput("respColError"),
                                    tags$p(tags$br(),
                                           tags$b("Acknowledgements:"), 
                                           "This material is based upon work supported by the National Science Foundation (Grants 1323162 and 1347740). Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the supporting agencies.")
                                    #,
                                    #checkboxInput("saveCheckbox","Save Report",value=FALSE)
                            ),
                            mainPanel(
                                    tabsetPanel(
                                            tabPanel("Preview Data",
                                                     uiOutput("rowCount"),
                                                     h4(htmlOutput("reportAddress")),
                                                     dataTableOutput(outputId="preview_datIn")
                                            ),
                                            tabPanel("Scoring Model",
                                                     checkboxInput("useMostRecent",label="Use most recent scoring model",value=TRUE),
                                                     uiOutput("majorVersionQuerry"),
                                                     uiOutput("minorVersionQuerry"),
                                                     uiOutput("scoringModelMetadataPreview")
                                            )
                                    )

                            )
                    )
           ),
           tabPanel("Interactive Report",
                    uiOutput("displayQuestion"),
                    uiOutput("rubricURL"),  #MODIF -- added on 9-22-2020
                    uiOutput("reportPanelSet")
           ),
           tabPanel("Downloads",
                    #downloadButton("downloadHTML","Download HTML Report"),
                    downloadButton("downloadPDF","Download PDF Report"),
                    uiOutput("downloadData")#,
                    #downloadButton("downloadIndex","Download Index File")
           )
)