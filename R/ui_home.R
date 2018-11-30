# This script contains the code related to Home tab

home_tab <- function() {
  tabPanel(
    title = "Home",
    id = "home",
    sidebarPanel(
      width = 2,
      shinyDirButton("dir", label = "Choose Directory", "Upload"),
      tags$h5("OR", align="center"),
      textInput("text_dir", label="Path", value=PATH),
      tags$strong("Note:"),
      tags$br(),
      tags$code("If both path (as text input) and directory (choosen via the file manager) is provided only the path will be consdiered"),
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
}
