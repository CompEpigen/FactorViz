# This script contains the code related to K selection tab
meta_analysis <- function() {
  tabPanel(
    "Meta Analysis",
    id = "met_analysis",
    sidebarPanel(
      width = 2,
      uiOutput("groupSelector_5"),
      uiOutput("lambdaSelector_5"),
      uiOutput("Kselector_5"),
      selectInput("analysisType", "Analysis:",
                  c(if(!is.null(medecom_ref_object)) "compare LMCs" else NULL,
                    "differential methylation"), selected=1),
      conditionalPanel('input.analysisType === "compare LMCs" ',
                       wellPanel(
                         h5("Select a reference run:"),
                         uiOutput("compareRunSelector"),
                         uiOutput("Kselector_ref"),
                         uiOutput("lambdaSelector_ref")
                       ),

                       selectizeInput('compareMatrices', 'Select matrices:',
                                      choices = list(
                                        "Current run" = c(`LMCs (\\hat{T})` = 'That', `Data` = 'D', `Reference (T^star)` = 'Tstar'),
                                        "Reference run" = c(`LMCs (\\hat{T})` = 'refThat', `Data` = 'refD', `Reference (T^star)` = 'refTstar')
                                      ), multiple = TRUE),

                       selectInput("comparativePlotType", "Output type:", c("dendrogram","heatmap","correlation heatmap"), selected=1),
                       uiOutput("topSDcgsSelectorCompare"),
                       radioButtons("SDCompareMatrices", "Calculate SD on:",
                                    c(`All`= "All", `Estimates (\\hat{T})` = 'That', `Data` = 'D', `Reference (T^star)` = 'Tstar')),
                       uiOutput("analysisTokensInput")

      ),
      conditionalPanel(' input.analysisType === "differential methylation" ',
                        selectInput('diffOutputType', "Output type:", c("Table", "GO Enrichments", "LOLA Enrichments"), selected=1),
                        conditionalPanel('input.diffOutputType === "Table"',
                       uiOutput("dmCGComponentSelector")),
                       uiOutput("diffTabT"),
                       sliderInput('dmr_threshold', 'Threshold', min=0.0, max=1.0, step=0.01, value=1.0)
      ),

      #conditionalPanel(' input.analysisType === "differential methylation" &&
      #                 input.diffOutputType === "Table" ',
      #                 actionButton('locusPlotSelected', "Locus plot")

      #),
      conditionalPanel(' input.analysisType === "differential methylation" &&
                       (input.diffOutputType === "GO Enrichments" || input.diffOutputType === "LOLA Enrichments" )',
                       uiOutput("region_selector"),
                       uiOutput("assemblySelector"),
                       conditionalPanel('input.diffOutputType === "GO Enrichments" ',
                        uiOutput("lmcgoSelector"),
                        actionButton('GOsubmitQuery', "Submit GO query")),
                       conditionalPanel('input.diffOutputType === "LOLA Enrichments" ',
                       uiOutput("lmclolaSelector"),
                         actionButton('LOLAsubmitQuery', "Submit LOLA query"))

      ),
      conditionalPanel(' input.analysisType === "compare LMCs" &&
                       input.comparativePlotType !== "heatmap" ) ',

                       checkboxInput("correlationCentered_5", "Center matrices", value=FALSE)
      ),
      #conditionalPanel(' input.analysisType === "phenotype modeling" ',

      #                 selectInput("phenoModelOutput", "Output:", choices=c("summary plot",
      #                                                                      "single case")),
      #                 uiOutput("targetVariableSelector"),
      #                 uiOutput("adjustmentVariableSelector"),
      #                 selectInput("discardLMC", "Which LMC to discard:", choices=c("largest", "smallest"), selected=1),
      #                 selectInput("modelPval", "p-value:", choices=c("overall", "minimal"), selected=1)

      #),



      checkboxInput("addPlotTitle", "Add plot titles", value =
                      TRUE)
    ),
    mainPanel(uiOutput('metaAnalysisPanel'), uiOutput('additionalMetaPlots'))
  )
}
