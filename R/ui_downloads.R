downloads <- function() {
  tabPanel(
    "Downloads",
    id = "download",
    sidebarPanel(
      width = 3,
      uiOutput("groupSelector_6"),
      uiOutput("Kselector_6"),
      uiOutput("lambdaSelector_6")
  ),
  mainPanel(uiOutput('downloadPanel'))
  )
}
