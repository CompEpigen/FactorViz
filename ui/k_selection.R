# This script contains the code related to K selection tab
k_selec<-tabPanel(title="K selection", id="k_select", 
                  sidebarPanel(
                    width = 2,
                    uiOutput("groupSelector"),
                    uiOutput("KvsStat"),
                    uiOutput("minKselector"),
                    uiOutput("maxKselector"),
                    checkboxInput("RMSEvsKplotAllLambdas", "Include all lambdas:", value=TRUE),
                    conditionalPanel('input.KvsStat === "cve" ',
                                     checkboxInput("normalizedCVE", "normalized", value=FALSE)
                    ),
                    conditionalPanel('(input.KvsStat === "cve" || input.KvsStat === "rmse") ',
                                     checkboxInput("normalizedCVEscale", "[0,1] scale", value=FALSE)
                    ),
                    uiOutput("lambdaSelector"),
                    checkboxInput("addPlotTitle", "Add plot titles", value=TRUE)
                    
                    
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
