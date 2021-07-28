# EBIC
EBIC <- function(data,indices,const) {
  n=nrow(data$x)
  p=ncol(data$x)

  # ToDo: change the const parameter, currently its a mix between numeric value and character (AIC)

  x.cur=data$x[,indices]
  x.cur=cbind(c(rep(1,n)),x.cur)
  lm.out = stats::.lm.fit(x.cur, data$y)
  deviance = n*(1+log(2*pi)+ log(sum(lm.out$residuals^2) /n))
  if(const!="AIC") {
    EBIC = deviance + log(n)*(length(indices)+2) + 2*(length(indices)+2)*const*log(p)
    return(EBIC)
  }
  if(const=="AIC") {
    AIC = deviance + 2*(length(indices)+2)
    return(AIC)
  }
}

# Simulate toeplitz correlated data
simdata.toeplitz.corr <- function (n, p, beta, sigma.normal, corr = 0) {

  mu=rep(0,p)
  help = numeric(p)
  for (k in 1:p) help[k]=corr^(k-1)

  Sigma = stats::toeplitz(help)
  if (p<=140) x = MASS::mvrnorm(n , mu, Sigma) else x = mvnfast::rmvn(n, mu, Sigma)

  linpred = x%*%beta
  y = stats::rnorm(n, linpred, sigma.normal)

  return(list(x=x,y=y))
}

# Extended BIC for given estimate beta
EBIC_beta <- function(data, beta,const) {
  n=nrow(data$x)
  p=ncol(data$x)
  df=sum(beta!=0)
  x.cur=data$x
  x.cur=cbind(c(rep(1,n)),x.cur)
  # ToDo: change the const argument, currently its a mixture between numeric and character (AIC)

  RSS = sum( (data$y - x.cur %*% beta)^2 )
  if (const!="AIC") {
    BIC=n*(1+log(2*pi)+log(RSS/n))+log(n)*df #compute BIC, nb. of parameters=df+1 (+sigma^2)
    EBIC=BIC+2*df*const*log(p)
    return(EBIC)
  }
  if (const=="AIC") {
    AIC = n*(1+log(2*pi)+log(RSS/n))+2*df
    return(AIC)
  }
}

# Fit ordinary least squares
beta.hat <- function(data, indices) {
  n=nrow(data$x)
  x.cur=data$x[,indices]
  x.cur=cbind(c(rep(1,n)),x.cur)                 #include intercept!
  hat.beta.cur = solve(t(x.cur)%*%x.cur) %*% t(x.cur) %*% data$y
  hat.beta = numeric(ncol(data$x)+1)
  hat.beta[c(1,indices+1)] = hat.beta.cur
  return(hat.beta)
}


# Get y hat values
y.hat <- function(data, beta.hat) {
  n = nrow(data$x)
  x.matrix = cbind(c(rep(1,n)),data$x)
  return (x.matrix %*% beta.hat)
}

# Sum of squares
sqnorm2 <- function(x) return(sum(x^2))

# Evaluate simulation results (internal)
method_res_scenario <- function (model, beta1, s0, data.test,
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
