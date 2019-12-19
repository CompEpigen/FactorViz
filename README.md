# FactorViz 2.0
*FactorViz* provides an interactive visualization and reporting environment for exploring the results of the methylome decomposition experiments carried out by [MeDeCom](http://github.com/lutsik/medecom/) or similar deconvolution tools. It is the interpretation tool used in a recently published [protocol](https://doi.org/10.1101/853150) to perform reference free deconvolution of complex DNA methylation data.

# Installation
You can install the *FactorViz* through GitHub using *devtools*:

```{r, eval=F}
install.packages("devtools")
devtools::install_github("CompEpigen/FactorViz")
```

# Using FactorViz
You can start *FactorViz* by using ```startFactorViz()``` command, and then specify the output from [DecompPipeline](https://github.com/CompEpigen/DecompPipeline).
A more detailed introduction into *FactorViz* can be found in the package [vignette](vignettes/factorviz.md).

# Dependencies
FactorViz depends on [MeDeCom](http://github.com/lutsik/medecom/) and is thoroughly tested with outputs from [DecompPipeline](https://github.com/CompEpigen/DecompPipeline).
