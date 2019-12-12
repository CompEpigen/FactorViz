
# Introduction
The package provides an interactive visualization and reporting environment for exploring the results of the methylome decomposition experiments carried out by [MeDeCom](http://public.genetik.uni-sb.de/medecom/)
## Installation
You can install *FactorViz* through GitHub using *devtools*:


```r
install.packages("devtools")
devtools::install_github("CompEpigen/FactorViz")
```

#Using FactorViz

## Default start-up
You can start *FactorViz* by using ```startFactorViz()``` command.

## Parameterized start-up
You can also initialise *FactorViz* by providing paths to the files or the [DecompPipeline](https://github.com/CompEpigen/DecompPipeline) output to the ```startFactorViz()``` command. It accepts the following parameters.

* ```decomp_output```: takes in the directory path to DecompPipeline output
* ```medecom_set```:  takes in the path to medecom set file
* ```ann_C```: takes in the path to CpG Annotation file
* ```ann_S```: takes in the path to Sample Annotation file
* ```ref_meth```: takes in the path to Reference Methylome file

> Note:
If decomp_output is provided all other parameters are force set to NULL



## Loading Data-sets
### Using DecompPipeline Output
If paramaters are not provided you can load the file by providing the path to  [DecompPipeline](https://github.com/CompEpigen/DecompPipeline) output directory (Fig 1a) in the user interface or by selecting the directory with the built-in file manager (Fig 1b, Fig 2)


![Fig 1a](images/input_directory_1.png)


![Fig 1b](images/choose_directory_1.png)


![Fig 2](images/choose_directory_2.png)




Once selected you can check the file names of that will be loaded into *FactorViz* in the UI (Fig 3).

![Fig 3](images/choose_directory_3.png)



### Using Multiple Files
With *FactorViz* you can also load the dataset from multiple location by switching the Non DeCompPipeline checkbox (Fig 4)

![Fig 4](images/multi_directory_1.png)



After the paths are set click on the ```Load Dataset``` button to load the dataset into *FactorViz* to carry out further analysis (Fig 5)

![Fig 5](images/choose_directory_4.png)





The further section of ```K Selection, Lambda Selection, LMC, Proportions and Meta-Analysis``` are explained in detail in [MeDeCom](https://github.com/lutsik/MeDeCom) documentation.
