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
> place holder   

