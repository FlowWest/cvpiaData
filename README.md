-----
<img src="cvpia_logo.png" align="right" width="40%"/>

### Inputs for CVPIA SIT Salmon Poplation Model

> place holder   
*The cvpiaData package enables users to generate the inputs for the SIT model given a scenario.*
  
#### Installation

``` r
# install.packages("devtools")
devtools::install_github("FlowWest/cvpiaData")
```  

#### Usage
This package provides input data in formats ready for the Salmon Population Model. There are several
other data packages that feed into this package:    
* [`cvpiaFlow`](https://flowwest.github.io/cvpiaFlow/)
* [`cvpiaHabitat`](https://flowwest.github.io/cvpiaHabitat/) 
* [`cvpiaTemperature`](https://flowwest.github.io/cvpiaTemperature/) 

``` r
# cached values
data(package = 'cvpiaData')

# set data for model scenaro
load_SIT_data(scenario = 'baseline')
```

#### About CVPIA SIT Data Management Strategy    
The CVPIA SIT Data Management Strategy is based on concepts described by Hadley Wickham in the book, R for Data Science which were developed by Hadley in an article for the Journal of Statistical Software entitled “Tidy Data” [(August 2014, Volume 59, Issue 10).](https://www.jstatsoft.org/index.php/jss/article/view/v059i10/v59i10.pdf) The Strategy intends to develop tidy datasets that are easy to manipulate, model and visualize. The work of transforming messy datasets into tidy structure helps domain experts focus on the key questions facing the CVPIA. 

