server_pdf<-function(input, output, server_env){
output$RMSEvsKplotPDF<-downloadHandler(
			filename=function(){
				if(is.null(input$lambda)){
					lambda="All"
				}else{
					lambda=input$lambda
				}
				sprintf("Kselectionplot_ANALYSIS_%s_cgsubset_%s_lambdaval_%s.pdf",
						input$KvsStat, input$cg_group, lambda)
			},
			content=function(con){
				pdf(con)
				server_env$doKselectionPlot()
				dev.off()
			},
			contentType="application/pdf"
	)

output$lineplotPDF <- downloadHandler(
		filename=function(){
			sprintf("L_select_linplot_scale_%s_cgsubset_%s_K_%s_minlval_%s_maxlval_%s.pdf",
					input$lambdaScale, input$cg_group_2, input$K_2, input$lambdaMin, input$lambdaMax)
		},

		content=function(con){
			par(mar=c(2,2,2,2))
			pdf(con, width=7, height=10)
			server_env$doLambdaPlot()
			dev.off()
		},
		contentType="application/pdf"
)
output$componentPlotPDF <- downloadHandler(
		filename=function(){
			sprintf("componentPlot_ANALYSIS_%s_cgsubset_%s_K_%s__lambdaval_%s.pdf",
					input$componentPlotType, input$cg_group_3, input$K_3, input$lambda_3)
		},

		content=function(con){
			#pdf(con, width=7, height=10)
			pdf(con)
			par(mar=c(2,2,2,2))
			server_env$doComponentPlot()
			dev.off()
		},
		contentType="application/pdf"

)

output$proportionPlotPDF <- downloadHandler(

		filename=function(){
			sprintf("proportionPlot_ANALYSIS_%s_cgsubset_%s_K_%s_lambdaval_%s.pdf",
					input$propPlotType, input$cg_group_4, input$K_4, input$lambda_4)
		},

		content=function(con){
			#pdf(con, width=7, height=10)
			pdf(con)
			server_env$doProportionPlot()
			dev.off()
		},
		contentType="application/pdf"

)
output$diffCGPlotPDF <- downloadHandler(
		filename=function(){
			sprintf("differential_methylation_cgsubset_%s_K_%s_lambdaval_%s_direction_%s.pdf",
					input$cg_group_5, input$K_5, input$lambda_5, input$diffTableType)
		},

		content=function(con){
			#pdf(con, width=7, height=10)
			pdf(con)
			server_env$doDiffCGPlot()
			dev.off()
		},
		contentType="application/pdf"

)
output$metaPlotPDF <- downloadHandler(
		filename=function(){
			sprintf("%s_cgsubset_%s_K_%s_lambdaval_%s_direction_%s.pdf",
					input$diffOutputType, input$cg_group_5, input$K_5, input$lambda_5, input$diffTableType)

		},

		content=function(file){
			#pdf(con, width=7, height=10)
			require(ggplot)
			#pdf(con, width=7, height=10)
			ggsave(file, plot = server_env$doMetaPlot(), device = "pdf")
		},
		contentType="application/pdf"

)
output$TraitAssociationPDF <- downloadHandler(
		filename=function(){
			sprintf("%s_TraitAssociation_cgsubset_%s_K_%s_lambdaval_%s.pdf",
					input$tatstat, input$cg_group_5, input$K_5, input$lambda_5)

		},

		content=function(file){
			require(ggplot)
			#pdf(con, width=7, height=10)
			ggsave(file, plot = server_env$doTraitAssociation(), device = "pdf")
		},
		contentType="application/pdf"

)
}
