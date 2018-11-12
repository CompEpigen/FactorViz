# This script contains the code related to K selection tab
l_selec<-function(){tabPanel("Lambda selection", id="l_selec",
         sidebarPanel(
           width = 2,
           uiOutput("groupSelector_2"),
           uiOutput("Kselector_2"),
           checkboxInput("RMSEvsKplotAllLambdas", "Include all lambdas:", value=TRUE),
           uiOutput("performanceModeSelector"),
           checkboxInput("includeRMSE_T", "Include RMSE of T", value=FALSE),
           checkboxInput("includeDist2C_T", "Include MDC of T", value=FALSE),
           checkboxInput("includeMAE_A", "Include MAE of A", value=FALSE),
           conditionalPanel(' input.performanceMode === "lineplots" ',
                            selectInput('lambdaScale', "Lambda scale", c("native", "logarithmic"), selected=1),
                            uiOutput("minLambdaSelector"),
                            uiOutput("maxLambdaSelector")
           ),
           checkboxInput("addPlotTitle", "Add plot titles", value=TRUE)
         ),
         mainPanel(
           uiOutput('performancePanel')
         )
)}
