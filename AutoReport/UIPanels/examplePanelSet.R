#AutoReport Example Report Panel Set
navbarPage("AACR AutoReporter",
           tabPanel("Interactive Report",
                    uiOutput("displayQuestion"),
                    uiOutput("rubricURL"),  #MODIF -- added on 9-22-2020
                    uiOutput("reportPanelSet")
           )#,
           # tabPanel("Downloads",
           #          downloadButton("downloadHTML","Download HTML Report"),
           #          downloadButton("downloadPDF","Download PDF Report"),
           #          uiOutput("downloadData"),
           #          downloadButton("downloadIndex","Download Index File")
           # )
)