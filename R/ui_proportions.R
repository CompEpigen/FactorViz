proportion<-function(){
                  tabPanel("Proportions", id="Proportions",
                  sidebarPanel(
                    width = 3,
                    uiOutput("groupSelector_4"),
                    uiOutput("Kselector_4"),
                    uiOutput("lambdaSelector_4"),
                    uiOutput("propPlotT"),
                    conditionalPanel('input.propPlotType === "lineplot" ',
                                     selectInput("sampleOrder", label="Order:", choices=c("increasing", "decreasing", "original"))
                    ),
                    #conditionalPanel('input.propPlotType === "scatterplot" ||
                    #                 input.propPlotType !== "lineplot" ',
                    #                 uiOutput("propMatrixSelector")
                    #),
                    conditionalPanel('input.propPlotType === "heatmap" ',
                                     checkboxInput("propClusterCols", "Cluster columns:", value=FALSE),
                                     checkboxInput("propClusterRows", "Cluster rows:", value=FALSE)
                    ),

                    conditionalPanel('input.propPlotType === "heatmap" &&
                                     input.propMatrix === "regression ref cmp" ',
                                     uiOutput("componentSelectorRef")

                    ),
                    conditionalPanel('input.propPlotType === "stratification plot" ',
                                     checkboxInput("includeRegressionLine", "Include regression line:", value=FALSE),
                                     checkboxInput("vertPlotLabs", "Vertical labels:", value=FALSE)

                    ),
                    conditionalPanel(' input.propPlotType === "lineplot" ||
                                       input.propPlotType === "scatterplot" ||
                                       input.propPlotType === "correlations" ',

                                     uiOutput("componentSelector_4"),

                                     conditionalPanel(' input.propPlotType === "lineplot" ||
                                                        input.propPlotType === "scatterplot" ',

                                                      uiOutput("refProfileSelector")
                                                    )

                    ),
                    conditionalPanel('input.propPlotType !== "lineplot" &&
                     input.propPlotType !== "barplot"',
                                     uiOutput("sampleColorSelector_4")

                    ),
                    conditionalPanel('input.propPlotType === "correlations" ',
                                     checkboxInput("correlationCentered_4", "Center matrices", value=FALSE)
                    )
                    ),
                  mainPanel(
                             plotOutput('proportionplot',
                                        height = "600px",
                                        width = "600px"),
                             br(),
                             downloadLink('proportionPlotPDF', label = "PDF", class = NA)
                  )
                  )
                  }
