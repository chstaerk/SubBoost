
#' EBIC
#'
#' This function computes extended BIC (EBIC).
#' @param data should be a list with data$x as design matrix and data$y as response
#' @param indices indices of data$x dat are included in the model
#' @param const parameter for the EBIC, for BIC specify const = 0.
#' @keywords EBIC
#' @export
#' @examples
#' EBIC()
EBIC <- function(data, indices, const) {
  n=nrow(data$x)
  p=ncol(data$x)
  x.cur=data$x[,indices]
  x.cur=cbind(c(rep(1,n)),x.cur)
  lm.out = .lm.fit(x.cur, data$y)
  deviance = n*(1+log(2*pi)+ log(sum(lm.out$residuals^2) /n))
  EBIC = deviance + log(n)*(length(indices)+2) + 2*(length(indices)+2)*const*log(p)
  return(EBIC)
}

#' Simulate toeplitz correlated data
#'
#' This function simulates toeplitz correlated data.
#' @param n number of observations
#' @param p number of variables
#' @param beta true coefficient vector (of size p)
#' @param sigma.normal epsilon
#' @param corr base correlation
#'
#' @keywords toeplitz
#' @export
#' @examples
#' simdata.toeplitz.corr()
simdata.toeplitz.corr <- function (n, p, beta, sigma.normal, corr = 0) {

  mu=rep(0,p)
  help=numeric(p)
  for (k in 1:p) help[k]=corr^(k-1)

  Sigma=toeplitz(help)
  if (p<=140) x = mvrnorm(n , mu, Sigma) else x = rmvn(n, mu, Sigma)

  linpred=x%*%beta
  y=rnorm(n, linpred, sigma.normal)

  return(list(x=x,y=y))
}

#' Extended BIC for given estimate beta
#'
#' This function computes extended BIC for given estimate beta of length p+1
#' @param data should be a list with data$x as design matrix and data$y as response
#' @param beta given estimate for coefficients of length p + 1
#' @param const parameter for the EBIC, for BIC specify const = 0.
#' @keywords EBIC
#' @export
#' @examples
#' EBIC_beta()
EBIC_beta <- function(data, beta,const) {
  n = nrow(data$x)
  p = ncol(data$x)
  df = sum(beta!=0)
  x.cur = data$x
  x.cur = cbind(c(rep(1,n)),x.cur)
  RSS = sum( (data$y - x.cur %*% beta)^2 )
  BIC = n*(1+log(2*pi)+log(RSS/n))+log(n)*df #compute BIC, nb. of parameters=df+1 (+sigma^2)
  EBIC = BIC + 2*df*const*log(p)
  return(EBIC)
}

#' Fit ordinary least squares
#'
#' This function estimates the coefficient vector beta.
#' @param data should be a list with data$x as design matrix and data$y as response
#' @param indices indices of data$x dat are included in the model
#' @keywords OLS
#' @export
#' @examples
#' beta.hat()
beta.hat <- function(data, indices) {
  n=nrow(data$x)
  x.cur=data$x[,indices]
  x.cur=cbind(c(rep(1,n)),x.cur)                 #include intercept!
  hat.beta.cur = solve(t(x.cur)%*%x.cur) %*% t(x.cur) %*% data$y
  hat.beta = numeric(ncol(data$x)+1)
  hat.beta[c(1,indices+1)] = hat.beta.cur
  return(hat.beta)
}


#' Get y hat values
#'
#' This function estimates the y given x and coefficient vector beta.
#' @param data should be a list with data$x as design matrix
#' @param beta.hat coefficient vector
#' @keywords OLS
#' @export
#' @examples
#' y.hat()
y.hat <- function(data, beta.hat) {
  n = nrow(data$x)
  x.matrix = cbind(c(rep(1,n)),data$x)
  return (x.matrix %*% beta.hat)
}

#' Sum of squares
#'
#' This function returns the sum of squares of x.
#' @param x input
#' @keywords ssq
#' @export
#' @examples
#' sqnorm2()
sqnorm2 <- function(x) return(sum(x^2))

#' Evaluate simulation results (internal)
#'
#' This helper function evaluates simulation results.
#' @param model input
#' @param beta1 input
#' @param s0 input
#' @param data.test input
#' @param n_test input
#' @param time input
#' @param beta input
#' @param data input
#' @keywords simulations
#' @export
#' @examples
#' method_results_scenario()
method_results_scenario <- function (model, beta1, s0, data.test,
                                     n_test, time, beta=NULL, data){

  false_pos = sum(beta1[model]==0)
  false_neg = s0-sum(beta1[model]!=0)

  if (is.null(beta)){
    beta.hat =  beta.hat(data,model)
  } else {
    beta.hat =  beta
  }
  estimation_error = sqnorm2 ( c(0,beta1) - beta.hat)

  x.cur=cbind(c(rep(1,n_test)),data.test$x)
  linpred = x.cur %*% beta.hat
  test_y.hat = as.vector(linpred)

  prediction_error   = sqrt( 1/n_test * sqnorm2 ( data.test$y - test_y.hat)) # RMSE

  vect = c(false_pos,false_neg,estimation_error,prediction_error,time)
  names(vect) = c("False_positives", "False_negatives", "Estimation_error", "Prediction_error", "Comp_time")
  return(vect)
}
