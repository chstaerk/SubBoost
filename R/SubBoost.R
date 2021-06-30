#' SubBoost
#'
#' This function implements the subspace boosting algorithm (SubBoost).
#' @param data should be a list with data$x as design matrix and data$y as response
#' @param Iter iterations
#' @param size.fixed default is set to NULL
#' @param tau parameter tau - default is set to 0.01
#' @param const parameter const - default is set to 0
#' @param savings  default is set to 1
#' @param family default is set to "normal"
#' @param s_max default is set to 20
#' @param automatic.stopping default is set to TRUE
#' @param plotting default is set to FALSE
#' @keywords SubBoost
#' @export
#' @examples
SubBoost <- function (data, Iter, size.fixed = NULL, tau = 0.01,
                      const = 0, savings = 1, family = "normal",
                      s_max = 20, automatic.stopping = TRUE,
                      plotting = FALSE) {



  p=ncol(data$x)
  n=nrow(data$x)

  A.size<-numeric(Iter)

  values <- rep(NA, Iter)

  S.list = vector("list", Iter)
  A.list = vector("list", Iter)

  beta.mat = matrix(numeric(Iter/savings*(p+1)), p+1, floor(Iter/savings))

  data.cur = data
  beta.cur = numeric(p+1)

  # initialization of size (s) if not provided
  if (is.null(size.fixed)) {
    regs=summary(regsubsets(as.matrix(data$x), data$y, intercept = TRUE, nvmax = min(c(p,n-3)), method = "exhaustive"))
    modelmatrix=as.matrix(regs$which[,-1,drop=F])
    vec=rep(0,nrow(modelmatrix)+1)
    vec[nrow(modelmatrix)+1] = EBIC(data,NULL,const)
    for (j in 1:nrow(modelmatrix)) {
      indices.help=as.vector(which(modelmatrix[j,] == TRUE))
      vec[j]=EBIC(data, indices.help, const)
    }
    mini = which.min(vec)
    if (mini<= nrow(modelmatrix)) {
      S=as.vector(which(modelmatrix[which.min(vec),] == TRUE))
    } else {
      S = integer(0)
    }
    if (length(S)>0) {
      size = length(S)
    } else {
      size = 1
    } } else {
      size = size.fixed
    }

  for (t in 1:Iter) {

    ## selection of S (step a3)
    regs=summary(regsubsets(as.matrix(data.cur$x), data.cur$y, intercept=TRUE, nvmax=min(c(p,n-3,size)), method = "exhaustive"))
    modelmatrix=as.matrix(regs$which[,-1,drop=F])
    S = which(modelmatrix[size,])
    S.list[[t]] = S

    ## selection of A (step a4)
    if (length(S)>1) {
      regs=summary(regsubsets(as.matrix(data$x[,S,drop=F]),data$y,intercept=TRUE,nvmax=min(c(length(S),n-3)),method = "exhaustive"))
      modelmatrix=as.matrix(regs$which[,-1,drop=F])
      vec=rep(0,nrow(modelmatrix)+1)
      vec[nrow(modelmatrix)+1] = EBIC(data,NULL,const)
      for (j in 1:nrow(modelmatrix)) {
        indices.help=S[as.vector(which(modelmatrix[j,]==TRUE))]
        vec[j]=EBIC(data,indices.help,const)
      }
      mini = which.min(vec)
      if (mini<= nrow(modelmatrix)) { A=S[as.vector(which(modelmatrix[which.min(vec),]==TRUE))] } else { A = integer(0) }
    }

    if (length(S)==1) {
      model_null = EBIC(data,NULL,const)
      model_one = EBIC(data,S,const)
      if (model_null<model_one) { A=integer(0) } else { A=S }
    }

    if (length(S)==0) A=integer(0)

    A.list[[t]] = A
    A.size[t]=length(A)

    ## Update (steps b and c)
    beta.hat.cur = beta.hat(data.cur,A)
    beta.cur = beta.cur + tau * beta.hat.cur
    data.cur$y = data$y - y.hat(data, beta.cur)

    values[t] = sqrt(1/n*sum(data.cur$y^2)) # training prediction error

    if (t %% savings == 0) {
      beta.mat[,t/savings] = beta.cur
    }

    if (plotting) {
      if (t %% 1000 == 0) {
        par(mfrow=c(1,1))
        plot(values,pch=20,main="",xlab="Iteration",ylab="Training loss")
      }
    }

    if (length(A)==0 & automatic.stopping) {
      mstop <- t
      break
    }
  }

  coef <- beta.cur
  names(coef) <- c("Intercept", colnames(data$x))
  selected <- which(coef[-1]!=0)

  return(list(size = size,
              A.size=A.size,
              values=values,
              beta.cur = beta.cur,
              residuals = data.cur$y,
              S.list = S.list,
              A.list = A.list,
              beta.mat = beta.mat,
              mstop = mstop,
              coef = coef,
              selected = selected))
}
