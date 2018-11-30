# This script contains the code related to K selection tab
l_selec<-function(){tabPanel("Lambda selection", id="l_selec",
         sidebarPanel(
           width = 2,
           uiOutput("groupSelector_2"),
           uiOutput("Kselector_2"),
           uiOutput("performanceModeSelector"),
           uiOutput("includeStats"),

           conditionalPanel(' input.performanceMode === "lineplots" ',
                            selectInput('lambdaScale', "Lambda scale", c("native", "logarithmic"), selected=1),
                            uiOutput("minLambdaSelector"),
                            uiOutput("maxLambdaSelector")
           )
         ),
         mainPanel(
           uiOutput('performancePanel')
         )
)}
