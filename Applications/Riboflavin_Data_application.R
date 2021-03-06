library("SubBoost")

library("leaps")
library("glmnet")
library("MASS")
library("mvnfast")
library("mboost")
library("stabs")
library("bst")
library("parallel")
library("glmnetUtils")
library("xgboost")

External_LOOCV = function (i,data,...){
  set.seed(i)
  
  n=nrow(data$x)
  p=ncol(data$x)
  data.cur = list()
  
  data.cur$x = data$x[-i,]
  data.cur$y = data$y[-i]
  data.adjXi = c(1,data$x[i,])
  
  
  # AdaSubBoost 
  start.time <- Sys.time()
  output = AdaSubBoost(data=data.cur, Iter=Iter, size.fixed=size.fixed, K=K, q=q, tau = tau, const=const, U_C=20, savings=1, nstop=nstop, s_max=s_max)
  end.time <- Sys.time()
  timeAdaSubBoost = as.double(end.time-start.time,units = "secs")
  AdaSubBoost_beta = output$beta.cur
  AdaSubBoost_model = which(AdaSubBoost_beta[-1]!=0)
  linpred = data.adjXi %*% AdaSubBoost_beta
  SqE_AdaSubBoost = as.numeric((linpred - data$y[i])^2)
  modelsizes_AdaSubBoost = as.numeric(length(AdaSubBoost_model))
  
  # RSubBoost (without adapting probabilities, adaptive=FALSE)
  start.time <- Sys.time()
  outputRSub = AdaSubBoost(data=data.cur, Iter=Iter, size.fixed=size.fixed, K=K, q=q, tau = tau, const=const, U_C=20, savings=1,
                           adaptive=FALSE, nstop=nstop, s_max=s_max)
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
  timeBoosting = as.double(end.time-start.time,units = "secs")
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
  mod.stab = stabsel(mod, PFER = PFER, sampling.type = "SS", q = q_stabsel, papply=lapply)
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
                                                                               xselect.init = Boost_model, mstop = mstop.max2), learner="ls", n.cores=1)
  mstop.optimal = which.min(cvm2$cv.error)
  twinBoost = bst(data.cur$x, data.cur$y, family="gaussian", ctrl = bst_control(twinboost = TRUE, twintype=1, coefir = Boost_beta ,
                                                                                xselect.init = Boost_model, mstop = mstop.optimal), learner="ls")
  end.time <- Sys.time()
  timeBoost2 = as.double(end.time-start.time,units = "secs")
  timeTwinBoost = timeBoosting + timeBoost2
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
  
  ## Elastic net with cross-validation for both parameters (lambda and alpha)
  start.time <- Sys.time()
  mod_cv <- cva.glmnet(x=data.cur$x, y=data.cur$y, family="gaussian")
  cv_errors_alpha <- rep(NA,length(mod_cv$alpha))
  for (k in 1:length(mod_cv$alpha)) {  # find minimal mean CV-error among alphas
    cv_errors_alpha[k] <- min(mod_cv$modlist[[k]]$cvm)
  }
  alpha_min <- mod_cv$alpha[which.min(cv_errors_alpha)]
  lambda_min <- sapply(mod_cv$modlist, `[[`, "lambda.min")[which.min(cv_errors_alpha)]
  ENet_beta = coef(mod_cv, alpha=alpha_min, s=lambda_min) #coef(mod_cv, alpha=alpha_min) 
  ENet_model = which(ENet_beta[-1] !=0)
  end.time <- Sys.time()
  timeENet = as.double(end.time-start.time,units = "secs")
  data.adjXi = c(1,data$x[i,])
  linpred = data.adjXi %*% ENet_beta
  SqE_ENet = as.numeric((linpred - data$y[i])^2)
  modelsizes_ENet = length(ENet_model)
  
  
  ## Relaxed Lasso with cross-validation
  start.time <- Sys.time()
  mod_cv <- cv.glmnet(x=data.cur$x, y=data.cur$y, family="gaussian", relax=TRUE)
  ReLasso_beta = coef(mod_cv, s="lambda.min", gamma="gamma.min") #lambda.min
  ReLasso_model = which(ReLasso_beta[-1] !=0)
  end.time <- Sys.time()
  timeReLasso = as.double(end.time-start.time,units = "secs")
  data.adjXi = c(1,data$x[i,])
  linpred = data.adjXi %*% ReLasso_beta
  SqE_ReLasso = as.numeric((linpred - data$y[i])^2)
  modelsizes_ReLasso = length(ReLasso_model)
  
  ## Model selected by EBIC with least squares
  start.time <- Sys.time()
  s_max = s_max
  # forward screening
  regs=summary(regsubsets(as.matrix(data.cur$x),data.cur$y,intercept=TRUE,nvmax=s_max,method="forward"))
  modelmatrix=as.matrix(regs$which[,-1,drop=F])
  S = which(modelmatrix[s_max,])

  regs=summary(regsubsets(as.matrix(data.cur$x[,S,drop=F]),data.cur$y,intercept=TRUE,nvmax=min(c(length(S),n-3)),method="exhaustive"))
  
  modelmatrix=as.matrix(regs$which[,-1,drop=F])
  vec=rep(0,nrow(modelmatrix)+1)
  vec[nrow(modelmatrix)+1] = EBIC(data.cur,NULL,const)
  for (j in 1:nrow(modelmatrix)) {
    indices.help=S[as.vector(which(modelmatrix[j,]==TRUE))]
    vec[j]=EBIC(data.cur,indices.help,const)
  }
  mini = which.min(vec)
  if (mini<= nrow(modelmatrix)) { S=S[as.vector(which(modelmatrix[which.min(vec),]==TRUE))] } else { S = integer(0) }
  EBIC_model <- S
  EBIC_beta <- beta.hat(data.cur,S)
  end.time <- Sys.time()
  timeEBIC = as.double(end.time-start.time,units = "secs")
  linpred = data.adjXi %*% EBIC_beta
  SqE_EBIC = as.numeric((linpred - data$y[i])^2)
  modelsizes_EBIC = length(EBIC_model)
  
  
  #XGBoost with variable selection (component-wise boosting)
  start.time <- Sys.time()
  cv <- xgb.cv(data = data.cur$x , 
               label = data.cur$y, 
               eta = 0.1, 
               nrounds = mstop.max,
               nfold = 10, 
               booster = "gblinear", 
               objective = "reg:squarederror", 
               updater = "coord_descent",
               feature_selector = "greedy", 
               base_score=0, 
               top_k = 1,
               verbose=FALSE,
               nthread=1,
               early_stopping_rounds = 10) 
  iter.opt <- which.min(cv$evaluation_log$test_rmse_mean)
  
  bst <- xgboost(data = data.cur$x , label = data.cur$y, eta = 0.1, nrounds = iter.opt, nthread = 1,
                 updater = 'coord_descent', feature_selector = 'greedy', top_k = 1, base_score=0, verbose=FALSE, 
                 booster = "gblinear", objective = "reg:squarederror", callbacks = list(cb.gblinear.history()))
  coef_path <- xgb.gblinear.history(bst)
  XGBoostCD_beta <- coef_path[iter.opt,]
  XGBoostCD_model <- which(coef_path[iter.opt,-1]!=0)
  end.time <- Sys.time()
  timeXGBoostCD = as.double(end.time-start.time,units = "secs")
  linpred = data.adjXi %*% XGBoostCD_beta
  SqE_XGBoostCD = as.numeric((linpred - data$y[i])^2)
  modelsizes_XGBoostCD = length(XGBoostCD_model)
  
  if (p<=20) {
    return(list(SqE_AdaSubBoost=SqE_AdaSubBoost, SqE_RSubBoost=SqE_RSubBoost, SqE_SubBoost=SqE_SubBoost, SqE_Boosting = SqE_Boosting, SqE_TwinBoost = SqE_TwinBoost, SqE_Lasso = SqE_Lasso, SqE_ENet = SqE_ENet, SqE_Stability = SqE_Stability ,  
                SqE_ReLasso = SqE_ReLasso, SqE_EBIC = SqE_EBIC,  SqE_XGBoostCD = SqE_XGBoostCD,  
                modelsizes_AdaSubBoost=modelsizes_AdaSubBoost, modelsizes_RSubBoost=modelsizes_RSubBoost, modelsizes_SubBoost=modelsizes_SubBoost, modelsizes_TwinBoost = modelsizes_TwinBoost, modelsizes_Boosting = modelsizes_Boosting, modelsizes_Lasso = modelsizes_Lasso, modelsizes_ENet = modelsizes_ENet, modelsizes_Stability = modelsizes_Stability,
                modelsizes_ReLasso = modelsizes_ReLasso, modelsizes_EBIC = modelsizes_EBIC,  modelsizes_XGBoostCD = modelsizes_XGBoostCD,
                timeAdaSubBoost=timeAdaSubBoost, timeRSubBoost=timeRSubBoost, timeSubBoost=timeSubBoost, timeSubBoost=timeSubBoost, timeTwinBoost = timeTwinBoost, timeBoosting = timeBoosting, timeLasso = timeLasso, timeENet = timeENet, timeStability = timeStability,
                timeReLasso = timeReLasso, timeEBIC = timeEBIC, timeXGBoostCD = timeXGBoostCD))
  } else {
    return(list(SqE_AdaSubBoost=SqE_AdaSubBoost, SqE_RSubBoost=SqE_RSubBoost, SqE_Boosting = SqE_Boosting, SqE_TwinBoost = SqE_TwinBoost, SqE_Lasso = SqE_Lasso, SqE_ENet = SqE_ENet, SqE_Stability = SqE_Stability ,  
                SqE_ReLasso = SqE_ReLasso, SqE_EBIC = SqE_EBIC, SqE_XGBoostCD = SqE_XGBoostCD,  
                modelsizes_AdaSubBoost=modelsizes_AdaSubBoost, modelsizes_RSubBoost=modelsizes_RSubBoost, modelsizes_TwinBoost = modelsizes_TwinBoost, modelsizes_Boosting = modelsizes_Boosting, modelsizes_Lasso = modelsizes_Lasso, modelsizes_ENet = modelsizes_ENet, modelsizes_Stability = modelsizes_Stability,
                modelsizes_ReLasso = modelsizes_ReLasso, modelsizes_EBIC = modelsizes_EBIC,  modelsizes_XGBoostCD = modelsizes_XGBoostCD, 
                timeAdaSubBoost=timeAdaSubBoost, timeRSubBoost=timeRSubBoost, timeTwinBoost = timeTwinBoost, timeBoosting = timeBoosting, timeLasso = timeLasso, timeENet = timeENet, timeStability = timeStability,
                timeReLasso = timeReLasso, timeEBIC = timeEBIC, timeXGBoostCD = timeXGBoostCD))
  }
}


##################################################
unAsIs <- function(X) {
  if("AsIs" %in% class(X)) {
    class(X) <- class(X)[-match("AsIs", class(X))]
  }
  X
}

##################################################
library("hdi")

data(riboflavin)
n = length(riboflavin$y)
p = dim(riboflavin$x)[2]
Xnames = colnames(riboflavin$x)

data = list()
data$x = as.matrix(riboflavin$x) 
data$y = as.vector(riboflavin$y)

data$x = unAsIs(data$x)
data$y = unAsIs(data$y)

################################################

q = min(20,p/2)
size.fixed = NULL
tau = 0.01 # 0.01
Iter = 10000
const = 1 # gamma (Penalty in EBIC)
PFER = 2
mstop.max = 5000
nstop = p / 2
q_stabsel = min(15, p/2) 
K = p/q
s_max = 15


#numCores <- detectCores()
numCores = 1 # Increase number of cores for parallel computing

RNGkind("L'Ecuyer-CMRG")
set.seed(1)
LOOCV_results = mclapply(1:n, External_LOOCV, mc.cores = numCores, data=data, mstop.max = mstop.max,Iter=Iter,K=K,q=q, tau = tau, const=const, U_C=20, savings=1, mc.set.seed = TRUE,  mc.preschedule = FALSE, size.fixed=size.fixed, q_stabsel=q_stabsel, PFER=PFER, nstop=nstop, s_max = s_max)

save(LOOCV_results, file="LOOCV_Riboflavin_CV.RData")