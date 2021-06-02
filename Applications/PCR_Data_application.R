##################################################
# Please download PCR data from JRSSB 
# JRSSB Datasets Vol. 77(5), Song and Liang (2015) 
# Website: https://rss.onlinelibrary.wiley.com/hub/journal/14679868/series-b-datasets/pre_2016a

library(SubBoost)


External_LOOCV = function (i,data,...){
  set.seed(i)
  
  n=nrow(data$x)
  p=ncol(data$x)
  data.cur = list()
  
  #data.cur$x = unAsIs(data$x[-i,])
  #data.cur$y = unAsIs(data$y[-i])
  data.cur$x = data$x[-i,]
  data.cur$y = data$y[-i]
  data.adjXi = c(1,data$x[i,])
  
  # AdaSubBoost 
  start.time <- Sys.time()
  output = AdaSubBoost(data=data.cur, Iter=Iter, size.fixed=size.fixed, K=K, q=q, tau = tau, const=const, U_C=20, savings=savings, nstop=nstop, s_max=min(p,20), marginal.screening=marginal.screening, plotting = plotting)
  end.time <- Sys.time()
  timeAdaSubBoost = as.double(end.time-start.time,units = "secs")
  AdaSubBoost_beta = output$beta.cur
  AdaSubBoost_model = which(AdaSubBoost_beta[-1]!=0)
  linpred = data.adjXi %*% AdaSubBoost_beta
  SqE_AdaSubBoost = as.numeric((linpred - data$y[i])^2)
  modelsizes_AdaSubBoost = as.numeric(length(AdaSubBoost_model))
  
  # RSubBoost (without adapting probabilities, adaptive=FALSE)
  start.time <- Sys.time()
  outputRSub = AdaSubBoost(data=data.cur, Iter=Iter, size.fixed=size.fixed, K=K, q=q, tau = tau, const=const, U_C=20, savings=savings,
                           adaptive=FALSE, nstop=nstop, s_max=min(p,20), marginal.screening=marginal.screening, plotting = plotting)
  end.time <- Sys.time()
  timeRSubBoost = as.double(end.time-start.time,units = "secs")
  RSubBoost_beta = outputRSub$beta.cur
  RSubBoost_model = which(RSubBoost_beta[-1]!=0)
  linpred = data.adjXi %*%  RSubBoost_beta
  SqE_RSubBoost = as.numeric((linpred - data$y[i])^2)
  modelsizes_RSubBoost = as.numeric(length(RSubBoost_model))
  
  # SubBoost (with full enumeration)
  if (p<=20){
    start.time <- Sys.time()
    outputSub = SubBoost(data=data.cur,Iter=Iter,size.fixed=size.fixed, tau = tau, const=const, savings=1)
    end.time <- Sys.time()
    timeSubBoost = as.double(end.time-start.time,units = "secs")
    SubBoost_beta = outputSub$beta.cur
    SubBoost_model = which(SubBoost_beta[-1]!=0)
    linpred = data.adjXi %*%  SubBoost_beta
    SqE_SubBoost = as.numeric((linpred - data$y[i])^2)
    modelsizes_SubBoost = as.numeric(length(SubBoost_model))
  }
  
  #L2Boosting
  start.time <- Sys.time()
  mod <- glmboost(y = data.cur$y, x = data.cur$x, control = boost_control(mstop = mstop.max)) 
  cv10f <- cv(model.weights(mod), type = "kfold")
  cvm <-cvrisk(mod, papply = lapply,  folds = cv10f)
  m.stop = mstop(cvm)
  end.time <- Sys.time()
  timeBoost = as.double(end.time-start.time,units = "secs")
  mod.boost = mod[m.stop]
  Boost_model = which(coef(mod.boost, which=1:p)!=0)
  Boost_beta = coef(mod.boost, which=1:p)
  data.adjXi = data$x[i,,drop=F]
  linpred = predict(mod.boost,type = "response", newdata = data.adjXi)
  SqE_Boosting = as.numeric((linpred - data$y[i])^2)
  modelsizes_Boosting= as.numeric(length(Boost_model))

  # Stability Selection for Boosting 
  mod <- glmboost(y = data.cur$y, x = data.cur$x, control = boost_control(mstop = 1000)) 
  start.time <- Sys.time()
  mod.stab = stabsel(mod, PFER = PFER, sampling.type = "SS", q = q_stabsel)
  end.time <- Sys.time()
  timeStability = as.double(end.time-start.time, units = "secs")
  Stability_model = as.vector(mod.stab$selected)
  Stability_beta = beta.hat(data,Stability_model)
  data.adjXi = c(1,data$x[i,])
  linpred = data.adjXi %*% Stability_beta
  SqE_Stability= as.numeric((linpred - data$y[i])^2)
  modelsizes_Stability = as.numeric(length(Stability_model))
  
  # Twin Boosting
  mstop.max2 = 5000
  start.time <- Sys.time()
  cvm2 <- cv.bst(data.cur$x, data.cur$y, family="gaussian", ctrl = bst_control(twinboost = TRUE, twintype=1, coefir = Boost_beta ,
                                                                               xselect.init = Boost_model, mstop = mstop.max2), learner="ls")
  mstop.optimal = which.min(cvm2$cv.error)
  twinBoost = bst(data.cur$x, data.cur$y, family="gaussian", ctrl = bst_control(twinboost = TRUE, twintype=1, coefir = Boost_beta ,
                                                                                xselect.init = Boost_model, mstop = mstop.optimal), learner="ls")
  end.time <- Sys.time()
  timeBoost2 = as.double(end.time-start.time,units = "secs")
  timeTwinBoost = timeBoost + timeBoost2
  TwinBoost_model = twinBoost$xselect
  data.adjXi = data$x[i,,drop=F]
  linpred = predict(twinBoost,type = "response", newdata = data.adjXi)
  SqE_TwinBoost = as.numeric((linpred - data$y[i])^2)
  modelsizes_TwinBoost = as.numeric(length(TwinBoost_model))
  
  ## Lasso with cross-validation
  start.time <- Sys.time()
  mod_cv <- cv.glmnet(x=data.cur$x, y=data.cur$y, family="gaussian")
  Lasso_beta = coef(mod_cv, mod_cv$lambda.min) #lambda.min
  Lasso_model = which(Lasso_beta[-1] !=0)
  end.time <- Sys.time()
  timeLasso = as.double(end.time-start.time,units = "secs")
  data.adjXi = c(1,data$x[i,])
  linpred = data.adjXi %*% Lasso_beta
  SqE_Lasso = as.numeric((linpred - data$y[i])^2)
  modelsizes_Lasso = length(Lasso_model)
  
  ### Adaptive Lasso
  start.time <- Sys.time()
  cv.fit1=cv.glmnet(data.cur$x,data.cur$y,intercept=TRUE,family="gaussian",standardize=TRUE,alpha=1)
  betas.cv.fit1=as.vector(predict(cv.fit1,type="coefficients",s="lambda.min"))[-1]
  new_penalty=rep(0,p)
  new_penalty=1/pmax(abs(betas.cv.fit1),.Machine$double.eps)
  cv.fit2=cv.glmnet(data.cur$x,data.cur$y,penalty.factor=new_penalty,intercept=TRUE,family="gaussian",standardize=TRUE,alpha=1)
  AdaLasso_beta=as.vector(predict(cv.fit2,type="coefficients",s="lambda.min"))
  AdaLasso_model = which(AdaLasso_beta[-1]!=0)
  end.time <- Sys.time()
  timeAdaLasso = as.double(end.time-start.time,units = "secs")
  data.adjXi = c(1,data$x[i,])
  linpred = data.adjXi %*% AdaLasso_beta
  SqE_AdaLasso = as.numeric((linpred - data$y[i])^2)
  modelsizes_AdaLasso = length(AdaLasso_model)
  if (p<=20) {
  return(list(SqE_AdaSubBoost=SqE_AdaSubBoost, SqE_RSubBoost=SqE_RSubBoost, SqE_SubBoost=SqE_SubBoost, SqE_Boosting = SqE_Boosting, SqE_TwinBoost = SqE_TwinBoost, SqE_Lasso = SqE_Lasso , SqE_AdaLasso = SqE_AdaLasso , SqE_Stability = SqE_Stability ,  
              modelsizes_AdaSubBoost=modelsizes_AdaSubBoost, modelsizes_RSubBoost=modelsizes_RSubBoost, modelsizes_SubBoost=modelsizes_SubBoost, modelsizes_TwinBoost = modelsizes_TwinBoost, modelsizes_Boosting = modelsizes_Boosting, modelsizes_Lasso = modelsizes_Lasso, modelsizes_AdaLasso = modelsizes_AdaLasso, modelsizes_Stability = modelsizes_Stability))
  } else {
  return(list(SqE_AdaSubBoost=SqE_AdaSubBoost, SqE_RSubBoost=SqE_RSubBoost, SqE_Boosting = SqE_Boosting, SqE_TwinBoost = SqE_TwinBoost, SqE_Lasso = SqE_Lasso , SqE_AdaLasso = SqE_AdaLasso , SqE_Stability = SqE_Stability ,  
                modelsizes_AdaSubBoost=modelsizes_AdaSubBoost, modelsizes_RSubBoost=modelsizes_RSubBoost, modelsizes_TwinBoost = modelsizes_TwinBoost, modelsizes_Boosting = modelsizes_Boosting, modelsizes_Lasso = modelsizes_Lasso, modelsizes_AdaLasso = modelsizes_AdaLasso, modelsizes_Stability = modelsizes_Stability))
  }
}

unAsIs <- function(X) {
  if("AsIs" %in% class(X)) {
    class(X) <- class(X)[-match("AsIs", class(X))]
  }
  X
}

##################################################
# Please download PCR data from JRSSB 
# JRSSB Datasets Vol. 77(5), Song and Liang (2015) 
# Website: https://rss.onlinelibrary.wiley.com/hub/journal/14679868/series-b-datasets/pre_2016a

X = t( read.table("Xgenes.txt") )
Y = scan("Y3.txt")
Xnames = scan("gene_id.txt",what=character())

data = list()
data$x = as.matrix(X)
data$y = as.vector(Y)
colnames(data$x) = Xnames

n = length(data$y)
p = dim(data$x)[2]

################################################

K = 1000
q = 5
size.fixed = NULL
tau = 0.01 # 0.01
Iter = 10000
const = 1 # gamma (Penalty in EBIC)
PFER = 2
mstop.max = 5000
nstop = p / 2
q_stabsel = min(15, p/2) 
marginal.screening = TRUE 

plotting = FALSE
savings = 100


#numCores <- detectCores()
numCores = 1 # Increase number of cores for parallel computing

RNGkind("L'Ecuyer-CMRG")
set.seed(1)
LOOCV_results = mclapply(1:n, External_LOOCV, mc.cores = numCores, data=data, mstop.max = mstop.max,Iter=Iter,K=K,q=q, tau = tau, const=const, U_C=20, savings=1, mc.set.seed = TRUE,  mc.preschedule = FALSE, size.fixed=size.fixed, q_stabsel=q_stabsel, PFER=PFER, nstop=nstop, marginal.screening=marginal.screening, plotting=plotting, savings=savings)

save(LOOCV_results, file="LOOCV_PCR_CV.RData")