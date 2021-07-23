# AdaSubBoost and RSubBoost
AdaSubBoost <-function (data, Iter, K = 100, q = 10, size.fixed = NULL, tau = 0.01, const = 0, savings = 1, U_C = 25, family = "normal",
                        conservative = TRUE, update = "S", adaptive = TRUE, s_max = 20, nstop = Iter, automatic.stopping = TRUE,
                        marginal.screening = FALSE, plotting = FALSE) {



  p=ncol(data$x)
  n=nrow(data$x)

  CV=matrix(numeric(Iter/savings*p),p,floor(Iter/savings))
  CS=matrix(numeric(Iter/savings*p),p,floor(Iter/savings))

  CV.cur = numeric(p)
  CS.cur = numeric(p)

  V.size<-numeric(Iter)
  S.size<-numeric(Iter)
  A.size<-numeric(Iter)

  values <- rep(NA,Iter)

  V.list = vector("list", Iter)
  S.list = vector("list", Iter)
  A.list = vector("list", Iter)

  beta.mat = matrix(numeric(Iter/savings*(p+1)),p+1,floor(Iter/savings))

  data.cur = data
  beta.cur = numeric(p+1)

  # initialization of size (s) if not provided
  if (is.null(size.fixed)) {

    if (marginal.screening) {
      marg.corr = numeric(p)
      for (j in 1:p) marg.corr[j] = abs(cor(data$x[,j],data$y))
      S = sort(order(marg.corr, decreasing=TRUE)[1:s_max])
    } else {
      regs=summary(regsubsets(as.matrix(data$x),data$y,intercept=TRUE,nvmax=s_max,method="forward"))
      modelmatrix=as.matrix(regs$which[,-1,drop=F])
      S = which(modelmatrix[s_max,])
    }

    if (length(S)>U_C) {
      regs=summary(regsubsets(as.matrix(data$x[,S,drop=F]),data$y,intercept=TRUE,nvmax=min(c(length(S),n-3)),method="backward"))
    } else {
      regs=summary(regsubsets(as.matrix(data$x[,S,drop=F]),data$y,intercept=TRUE,nvmax=min(c(length(S),n-3)),method="exhaustive"))
    }
    modelmatrix=as.matrix(regs$which[,-1,drop=F])
    vec=rep(0,nrow(modelmatrix)+1)
    vec[nrow(modelmatrix)+1] = EBIC(data,NULL,const)
    for (j in 1:nrow(modelmatrix)) {
      indices.help=S[as.vector(which(modelmatrix[j,]==TRUE))]
      vec[j]=EBIC(data,indices.help,const)
    }
    mini = which.min(vec)
    if (mini<= nrow(modelmatrix)) { S=S[as.vector(which(modelmatrix[which.min(vec),]==TRUE))] } else { S = integer(0) }

    if (length(S)>0) {
      size = length(S)
    } else {
      size = 1
    } } else {
      size = size.fixed
      regs=summary(regsubsets(as.matrix(data$x),data$y,intercept=TRUE,nvmax=size,method="forward"))
      modelmatrix=as.matrix(regs$which[,-1,drop=F])
      S = which(modelmatrix[size,])
    }

  relfreq=rep((q-size)/(p-size),p)

  counter.empty <- 0 # counter of A^(t)==0 for automatic stopping

  for (t in 1:Iter) {

    # Independent Bernoulli draws (step a1)
    b = rbinom(p,1,relfreq)

    # Definition of set V of considered base-learners (step a2)
    V = which(b==1)
    if (conservative){
      V = union(S,V)
    } else {
      V = union(A, V)
    }

    V.list[[t]] = V

    ## selection of S (step a3)
    if (length(V)<=1){
      S = V
    } else {
      if (length(V)>U_C) {
        regs=summary(regsubsets(as.matrix(data.cur$x[,V,drop=F]),data.cur$y,intercept=TRUE,nvmax=min(c(length(V),n-3)),method="backward"))
      } else {
        regs=summary(regsubsets(as.matrix(data.cur$x[,V,drop=F]),data.cur$y,intercept=TRUE,nvmax=min(c(length(V),n-3)),method="exhaustive"))
      }
      modelmatrix=as.matrix(regs$which[,-1,drop=F])
      S = V[which(modelmatrix[min(size,length(V)),])]
    }
    S.list[[t]] = S

    ## selection of A (step a4)
    if (length(S)>1) {
      regs=summary(regsubsets(as.matrix(data$x[,S,drop=F]),data$y,intercept=TRUE,nvmax=min(c(length(S),n-3)),method="exhaustive"))
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

    if (length(S)==0) A=S

    if (length(A)==0) {
      counter.empty <- counter.empty + 1
    } else {
      counter.empty <- 0
    }

    A.list[[t]] = A

    ## Update (steps b and c)
    beta.hat.cur = beta.hat(data.cur,A)
    beta.cur = beta.cur + tau * beta.hat.cur
    data.cur$y = data$y - y.hat(data,beta.cur)

    # Adaptation of sampling probabilities in AdaSubBoost (step d)
    if (adaptive) {
      if (update=="A") CS.cur[A] = CS.cur[A] + 1
      if (update=="S") CS.cur[S] = CS.cur[S] + 1
      CV.cur[V] = CV.cur[V] + 1
      relfreq[V] = (q - size + K*CS.cur[V])/(p - size + K*CV.cur[V])
    }

    V.size[t]=length(V)
    S.size[t]=length(S)
    A.size[t]=length(A)

    values[t] = sqrt(1/n*sum(data.cur$y^2))  # training prediction error

    if (t %% savings == 0) {
      beta.mat[,t/savings] = beta.cur
      CV[,t/savings] = CV.cur
      CS[,t/savings] = CS.cur
    }

    # plotting of training loss during the algorithm (every 1000 iterations)
    if (plotting) {
      if (t %% 1000 == 0) {
        par(mfrow=c(1,1))
        plot(values,pch=20,main="",xlab="Iteration",ylab="Training loss")
      }
    }

    # automatic stopping
    if (counter.empty>=nstop & automatic.stopping) {
      mstop <- t
      break
    }
    if (t==Iter) {
      mstop <- Iter
    }

  }

  relfreq.hist=(q - size + K*CS)/(p - size + K*CV)
  relfreq.final=relfreq.hist[,floor(Iter/savings)]

  coef <- beta.cur
  names(coef) <- c("Intercept", colnames(data$x))
  selected <- which(coef[-1]!=0)

  return(list(relfreq.hist=relfreq.hist, relfreq.final=relfreq.final, S.size=S.size, V.size=V.size, A.size=A.size, values=values,
              beta.cur = beta.cur, residuals = data.cur$y,
              V.list = V.list, S.list = S.list, A.list = A.list,
              beta.mat = beta.mat,
              mstop = mstop,
              coef = coef,
              selected = selected))
}

