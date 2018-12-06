# FactorViz 2.0
*FactorViz* provides an interactive visualization and reporting environment for exploring the results of the methylome decomposition experiments carried out by [MeDeCom](http://public.genetik.uni-sb.de/medecom/).

# Installation
You can install the *FactorViz* through GitHub using *devtools*:

```{r, eval=F}
install.packages("devtools")
devtools::install_github("lutsik/FactorViz")
```

# Using FactorViz
You can start *FactorViz* by using ```startFactorViz()``` command.
A more detailed introduction into *FactorViz* can be found in the package vignette
(https://github/lutsik/FactorViz/blolb/master/vignettes/factorviz.md).

# Dependencies
FactorViz depends on [MeDeCom](http://public.genetik.uni-sb.de/medecom/) and is thoroughly tested with outputs from [DeCompPipeline](https://github.com/lutsik/DecompPipeline)
