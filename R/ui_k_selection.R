# This script contains the code related to K selection tab
k_selec<-function(){
  tabPanel(title="K selection", id="k_select",
                  sidebarPanel(
                    width = 3,
                    uiOutput("groupSelector"),
                    uiOutput("KvsStat"),
                    uiOutput("minKselector"),
                    uiOutput("maxKselector"),
                    checkboxInput("RMSEvsKplotAllLambdas", "Include all lambdas:", value=TRUE),
                    conditionalPanel('input.KvsStat === "cve" ',
                                     checkboxInput("normalizedCVE", "normalized", value=FALSE)
                    ),
                    uiOutput("lambdaSelector")
                  ),
                  mainPanel(
                    value='RMSEvsK',
                    plotOutput('RMSEvsKplot'
                               ,height = "450px"
                               ,width = "600px"
                    ),
                    br(),
                    downloadLink('RMSEvsKplotPDF', label = "PDF", class = NA)
                  )
                 )
                 }
