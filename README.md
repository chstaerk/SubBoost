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


# Example
```r
# load SubBoost library
library("SubBoost")

# load "TH.data" package for bodyfat data
library("TH.data")

# input data format: 
# list with design matrix in data$x and response vector in data$y
data <- list()
data$x <- as.matrix(bodyfat[,-2]) 
data$y <- as.vector(bodyfat$DEXfat)

# constant (gamma) in EBIC 
const <- 0 # classical BIC (default in AdaSubBoost)

# expected search size 
q <- 5 # should be chosen smaller than the number of covariates (p=9)

# adaptation rate
K <- 100 # default

# maximum size for initial screening 
s_max <- 9 # here the same as the number of covariates p=9

# learning rate 
tau <- 0.01 # default 

# (maximum) number of iterations 
Iter <- 1000 

# AdaSubBoost
set.seed(123)
output <- AdaSubBoost(data = data, Iter = Iter, const = const, 
                      K = K, q = q, tau = tau, s_max = s_max)
output$coef # estimated coefficient vector by AdaSubBoost
output$selected # selected variables by AdaSubBoost


# RSubBoost (no adaptation of sampling probabilites for base-learners)
set.seed(123)
outputRSub <- AdaSubBoost(data = data, Iter = Iter, const = const, 
                          K = K, q = q, tau = tau, s_max = s_max, 
                          adaptive = FALSE)
outputRSub$coef # estimated coefficient vector by RSubBoost
outputRSub$selected  # selected variables by RSubBoost


# SubBoost (only applicable for low-dimensional settings, e.g. p<=20)
outputSub <- SubBoost(data = data, Iter = Iter, const = const, tau = tau)
outputSub$coef # estimated coefficient vector by SubBoost
outputSub$selected # selected variables by SubBoost
```
