# SubBoost 

Randomized boosting with multivariable base-learners



## Installation

  ```
  library("devtools")
  install_github("chstaerk/SubBoost")
  library("SubBoost")
  ```
To be able to use the `install_github()` command, one needs to install `devtools` first:
  ```
  install.packages("devtools")
  ```
 
 ## Fitting functions
  
  The function `AdaSubBoost` can be used to run the Adaptive Subspace Boosting algorithm as well as the Random Subspace Boosting algorithm (by specifying the option `adaptive = FALSE`). 
The function `SubBoost` can be used to run the Subspace Boosting algorithm.
