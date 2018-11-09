# This script contains the code related to Home tab

home_tab <- tabPanel(title="Home", id="home",
                            sidebarPanel(
                              width = 2,
                              shinyDirButton("dir", label = "Choose Directory", "Upload"),
                              tags$h5(""),
                              actionButton("load", "Load Datasets")
                            ),
                            mainPanel(
                              h4("Files in the directory"),
                              verbatimTextOutput("files"),
                              uiOutput("AnalysisRunDescriptionHeader"),
                              tableOutput('AnalysisRunParameterTable')
                            )
                   )
