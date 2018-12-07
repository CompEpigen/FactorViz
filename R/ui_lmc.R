lmc_tab <- function() {
  tabPanel(
    "LMCs",
    id = "LMC",
    sidebarPanel(
      width = 3,
      uiOutput("groupSelector_3"),
      uiOutput("Kselector_3"),
      uiOutput("refcheck"),
      conditionalPanel(
        'input.componentPlotType !== "matching plot" ',
        uiOutput("lambdaSelector_3")
      ),
      uiOutput('componentPlotT'),
      conditionalPanel('input.componentPlotType === "scatterplot all" ||
                        input.componentPlotType === "scatterplot matching" ||
                        input.componentPlotType === "scatterplot avg matching"',
        uiOutput("componentSelector")
      ),
      conditionalPanel(
        'input.componentPlotType === "heatmap" ||
        input.componentPlotType === "mds plot" ||
        input.componentPlotType === "dendrogram"  ||
        input.componentPlotType == "similarity graph" ',
        uiOutput("cgVar"),
        selectInput("mdsDist",  "Distance:",   c("euclidean", "angular", "correlation"), selected = 1 )
      ),
      conditionalPanel(
        '(input.componentPlotType === "heatmap" ||
        input.componentPlotType === "mds plot" ||
        input.componentPlotType === "dendrogram" ||
        input.componentPlotType === "similarity graph") && input.cgVarSubset ',
        uiOutput("topSDcgsSelector")
  ),
  conditionalPanel(
    'input.componentPlotType === "mds plot" &&
    METH_DATA_FLAG',

    selectInput(
      "mdsInclude_3",
      "Include data:",
      c("references" = "ref", "all data" = "data"),
      selected = 2
    )
  ),
  conditionalPanel(
    'input.componentPlotType === "mds plot" &&
    input.mdsInclude === "data"',
    uiOutput("sampleColorSelector_3")

  ),

  conditionalPanel(
    'input.componentPlotType === "scatterplot all" &&
    input.componentPlotType === "scatterplot matching" &&
    input.componentPlotType === "scatterplot avg matching" ',
    selectInput(
      "scatterType",
      "Scatterplot",
      c("smoothed", "ordinary"),
      selected = 1
    )
  ),
  conditionalPanel(
    'input.componentPlotType !== "heatmap" &&
    input.componentPlotType !== "mds plot" &&
    input.componentPlotType !== "dendrogram" &&
    input.componentPlotType !== "histogram" &&
    input.scatterType === "ordinary" ',
    uiOutput("pointColorSelector"),
    uiOutput("pointFilter"),
    uiOutput("CGcategorySubsetSelector")

  ),
  conditionalPanel(
    'input.componentPlotType === "similarity graph" ',
    sliderInput(
      'minGraphSimilarity',
      'Minimal similarity',
      min = 0,
      max = 1,
      step = 0.01,
      value = 0
    )
  ),
  conditionalPanel('
    (input.componentPlotType === "heatmap" ||
    input.componentPlotType === "scatterplot all" ||
    input.componentPlotType === "scatterplot matching" ||
    input.componentPlotType === "scatterplot avg matching" ||
    input.componentPlotType === "dendrogram" ||
    input.componentPlotType === "similarity graph") &&
    input.mdsDist !== "euclidean" ' ,
    checkboxInput("correlationCentered_3", "Center matrices", value =
                    FALSE)
  )
  ),
  mainPanel(uiOutput('componentsPanel'))
  )
}
