# FactorViz 2.0
*FactorViz* provides an interactive visualization and reporting environment for exploring the results of the methylome decomposition experiments carried out by [MeDeCom](http://github.com/lutsik/medecom/) or similar deconvolution tools. It is the interpretation tool used in a recently published [protocol](https://www.nature.com/articles/s41596-020-0369-6) to perform reference free deconvolution of complex DNA methylation data.

# Installation
You can install the *FactorViz* through GitHub using *devtools*:

```{r, eval=F}
install.packages("devtools")
devtools::install_github("CompEpigen/FactorViz")
```

Installation has been tested on the following operating systems:

Type |   Distribution |   Version |   R-version |   Installation successful |   Protocol tested |   Comments
---- |	 ---- | ---- | ---- | ---- | ---- | ---- 
Linux 						|  Debian 	|  Wheezy (7)  		|  R-3.5.2 |  Yes |  Yes |   
Linux						| 	 Debian |  Wheezy (7)	  	|  R-3.6.0 |  Yes |  Yes |  
Linux						| 	Debian					  	|  Jessie (8)	|  R-3.5.3	  |  Yes |  Yes (reduced)  |   
Linux						| 	Debian						| Jessie (8)									|  R-3.6.1 |  Yes | 	No	|  
Linux						| 	Debian						| 	Jessie (8)									|  R-4.0	  | 		Yes	| 	No	|  
Linux						| 	Debian						| 	Buster (10)					|  R-3.5.2 |  Yes |  Yes (reduced) |  	
Linux						| 	Fedora	|  	28									|  R-3.5.3 | 	Yes |  No |  
Linux						| 		Fedora					| 	31									|  R-3.6.1 | 	No  |  Yes (reduced) |  `igraph' dependency fails to install
Linux						| 	CentOS	| 	8.0				|  R-3.5.2 |  Yes	|  Yes (reduced) |  	
Linux						| 	CentOS						| 	8.0									|  R-3.6.1 |  Yes |  Yes (reduced) | 	
Linux						| 	Ubuntu					| 	19									|  R-3.6.1 | 		Yes	|  Yes (reduced) |  
MacOS		| 							| 	Mojave								|  R-3.5.1 | 		Yes	|  Yes (reduced)	| 	binary release used	
MacOS					| 							| 	Catalina							|  R-3.6.0 | 		Yes	|  Yes (reduced)	| 	
Windows						| 	10						| 	Pro									|  R-3.6.1 |  No |  Yes (reduced)	| 	Use docker image https://hub.docker.com/r/mscherer/medecom	
Windows					| 	7						| 	Pro									|  R-3.6.1 | 		No	| 	No |  Docker is not available for Windows 7																	

# Using FactorViz
You can start *FactorViz* by using ```startFactorViz()``` command, and then specify the output from [DecompPipeline](https://github.com/CompEpigen/DecompPipeline).
A more detailed introduction into *FactorViz* can be found in the package [vignette](vignettes/factorviz.md).

# Dependencies
FactorViz depends on [MeDeCom](http://github.com/lutsik/medecom/) and is thoroughly tested with outputs from [DecompPipeline](https://github.com/CompEpigen/DecompPipeline).

# Citation
If you use *FactorViz*, please cite:
Scherer, M., Nazarov, P. V., Toth, R., Sahay, S., Kaoma, T., Maurer, V., Vedeneev, N., Plass, C., Lengauer, T., Walter, J., & Lutsik, P. (2020). Reference-free deconvolution, visualization and interpretation of complex DNA methylation data using DecompPipeline, MeDeCom and FactorViz. Nature Protocols, 15(10), 3240–3263. https://doi.org/10.1038/s41596-020-0369-6
