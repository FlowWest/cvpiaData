-----
<img src="cvpia_logo.png" align="right" width="40%"/>

### Inputs for CVPIA SIT Salmon Population Model

*This website supports inputs for the CVPIA Salmon Population Model, specifically those related to flows, habitat, and temperature.*   

The cvpiaData package enables users to load inputs formatted for use directly into the Salmon Population Model. Additional supporting data packages are provided below to allow model users access to review metadata, assumptions, and points of contact regarding the datasets that created the model inputs. 

Both the the Salmon Population Model and data packages are written in  R, an open source programming language commonly used by statisticians, data scientists, and other professional modelers. Some familiarity with R will be necessary to fully use the data packages on this page. Additional packages may be generated in the future to support various scenarios under review by the CVPIA Science Integration Team.
  
#### Installation

``` r
# install.packages("devtools")

# install these three supporting packages before installing cvpiaData
devtools::install_github("FlowWest/cvpiaFlow")
devtools::install_github("FlowWest/cvpiaTemperature")
devtools::install_github("FlowWest/cvpiaHabitat")

devtools::install_github("FlowWest/cvpiaData")
```  

#### Usage
This package provides input data in formats ready for the Salmon Population Model. There are three
unique data packages that feed into this package:    
* [`cvpiaFlow`](https://flowwest.github.io/cvpiaFlow/)
* [`cvpiaHabitat`](https://flowwest.github.io/cvpiaHabitat/) 
* [`cvpiaTemperature`](https://flowwest.github.io/cvpiaTemperature/)   

These separate data packages are available for standalone use in R. The above links are available to review package documentation including metadata on the sources, assumptions, and points of contact for further questions.  

``` r
# cached values
data(package = 'cvpiaData')

# load data for model 
load_baseline_data()
```

The SIT may develop additional scenarios to evaluate management actions or to test model assumptions. As appropriate, those scenarios will be documented here.

#### About CVPIA SIT Data Management Strategy    
The CVPIA SIT Data Management Strategy is based on concepts described by Hadley Wickham in the book, R for Data Science which were developed by Hadley in an article for the Journal of Statistical Software entitled “Tidy Data” [(August 2014, Volume 59, Issue 10).](https://www.jstatsoft.org/index.php/jss/article/view/v059i10/v59i10.pdf) The Strategy intends to develop tidy datasets that are easy to manipulate, model and visualize. The work of transforming messy datasets into tidy structure helps domain experts focus on the key questions facing the CVPIA. 


<style>.logo{margin-top: 40px;}</style>
<div class = 'logo'>Data Assembled and Maintained by <a href = "http://www.flowwest.com/" target = "_blank"> <img src="TransLogoTreb.png" width="150px"/></div>