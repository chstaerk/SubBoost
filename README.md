
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/chstaerk/subboost?branch=master&svg=true)](https://ci.appveyor.com/project/mayrandy/subboost)

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


## Examples
```r
# load SubBoost library
library("SubBoost")

### low-dimensional example (SubBoost)
# load "TH.data" package for bodyfat data
library("TH.data")

# input data format: 
# list with design matrix in data$x and response vector in data$y
data <- list()
data$x <- as.matrix(bodyfat[,-2]) 
data$y <- as.vector(bodyfat$DEXfat)

# constant (gamma) in EBIC 
const <- 0 # classical BIC 

# learning rate 
tau <- 0.01 

# (maximum) number of iterations 
Iter <- 1000 

# SubBoost (only applicable for low-dimensional settings, e.g. p<=20)
outputSub <- SubBoost(data = data, Iter = Iter, const = const, tau = tau)
outputSub$selected # selected variables by SubBoost
outputSub$coef # estimated coefficient vector by SubBoost


### high-dimensional example (AdaSubBoost and RSubBoost)
# load "hdi" package for riboflavin data 
library("hdi")

data(riboflavin)
n <- length(riboflavin$y)
p <- dim(riboflavin$x)[2]

# input data format: 
# list with design matrix in data$x and response vector in data$y
data <- list()
data$x <- as.matrix(riboflavin$x) 
data$y <- as.vector(riboflavin$y)

# constant (gamma) in EBIC 
const <- 1 

# expected search size 
q <- 20 

# adaptation parameter
K <- p/q

# maximum size for initial screening 
s_max <- 15 

# learning rate 
tau <- 0.01 

# (maximum) number of iterations 
Iter <- 1000 

# AdaSubBoost
set.seed(123)
output <- AdaSubBoost(data = data, Iter = Iter, const = const, 
                      K = K, q = q, tau = tau, s_max = s_max)
output$selected # selected variables by AdaSubBoost
output$coef[names(output$selected)] # estimated non-zero coefficients by AdaSubBoost


# RSubBoost (no adaptation of sampling probabilites for base-learners)
set.seed(123)
outputRSub <- AdaSubBoost(data = data, Iter = Iter, const = const, 
                          K = K, q = q, tau = tau, s_max = s_max, 
                          adaptive = FALSE)
outputRSub$selected  # selected variables by RSubBoost
outputRSub$coef[names(outputRSub$selected)] # estimated non-zero coefficients by RSubBoost
```
