# simpleDE

## Introduction
This is an R package for carrying out simple differential expression analysis on gene expression data using t-test.
It requires easy format of input data and is able to choose interesting genes conveniently from the results based on specified criteria.
It also has a high computation speed due to simplicity.

## Installation
To install the github version, open R and type
```R
install.packages("devtools")
library("devtools")
install_github("https://github.com/xuan-chen/simpleDE", build_vignettes = TRUE)
```

## Main functions
* Create_simpleDE
* Filter_Data
* Normalize_Data
* DEA_test
* Get_DEG

## Quick start
This packages uses expression matrix together with sample data and gene data as input and performs simple differentail expression analysis
but an easy pipline.
```R
simple.dt = Create_simpleDE(mtx, s_data, g_data)
simple.dt = Filter_Data(simple.dt)
simple.dt = Normalize_Data(simple.dt)
de.geno = DEA_test(simple.dt, group = "treatment")
de.res = Get_DEG(de.geno, "log_FC", 0.5, is.sort = F)
de.res = Get_DEG(de.res, "p_value", 0.01, is.sort = F)
```
Please see the [vignette](https://github.com/xuan-chen/simpleDE/tree/master/vignettes) for more detailed explantions of 
the simpleDE package.
