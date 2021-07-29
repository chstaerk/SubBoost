library("SubBoost")

library("leaps")
library("MASS")
library("mvnfast")

library("glmnet")
library("mboost")
library("stabs")
library("bst")
library("parallel")
library("glmnetUtils")
library("xgboost")


fsim <- function(i, Iter, K, q, size.fixed, tau, const, U_C=25, savings=1, PFER, q_stabsel,
                 s0, n, p, n_test, sigma.normal, corr, nstop, randomS0, s_max, mstop.max, mstop.max2){
  set.seed(i)
  if (randomS0) {
    S0 = sort(sample(1:p,s0))
  } else {
  S0 = 1:s0
  }
  beta = numeric(p)
  beta[S0] = runif(s0,-2,2)
  
  if (p>2000) {
  data = simdata.toeplitz.corr.multiple(n =n ,q = p/10 , qrep=10, beta =beta ,sigma.normal = sigma.normal,corr=corr) 
  data.test = simdata.toeplitz.corr.multiple(n_test,q = p/10,qrep=10, beta=beta,sigma.normal= sigma.normal,corr= corr)
  } else { 
  data = simdata.toeplitz.corr(n =n , p=p, beta =beta ,sigma.normal = sigma.normal,corr=corr) 
  data.test = simdata.toeplitz.corr(n = n_test , p=p, beta =beta ,sigma.normal = sigma.normal,corr=corr) 
  }
  colnames(data$x) = 1:p
  
  # AdaSubBoost 
  start.time <- Sys.time()
  output = AdaSubBoost(data=data, Iter=Iter, size.fixed=size.fixed, K=K, q=q, tau = tau, const=const, U_C=U_C, savings=savings, nstop=nstop, plotting=FALSE, s_max=s_max)
  end.time <- Sys.time()
  timeAdaSubBoost = as.double(end.time-start.time,units = "secs")
  AdaSubBoost_beta = output$beta.cur
  AdaSubBoost_model = which(AdaSubBoost_beta[-1]!=0)

  # RSubBoost (without adapting probabilities, adaptive=FALSE)
  start.time <- Sys.time()
  outputRSub = AdaSubBoost(data=data, Iter=Iter, size.fixed=size.fixed, K=K, q=q, tau = tau, const=const, U_C=U_C, savings=savings,
                           adaptive=FALSE, nstop=nstop, plotting=FALSE, s_max=s_max)
  end.time <- Sys.time()
  timeRSubBoost = as.double(end.time-start.time,units = "secs")
  RSubBoost_beta = outputRSub$beta.cur
  RSubBoost_model = which(RSubBoost_beta[-1]!=0)
  
  AdaSubBoost_model
  RSubBoost_model
  AdaSubBoost_beta[-1][S0]
  RSubBoost_beta[-1][S0]
  
  # SubBoost (with full enumeration)
  if (p<=30){
    start.time <- Sys.time()
    outputSub = SubBoost(data=data,Iter=Iter,size.fixed=size.fixed, tau = tau, const=const, savings=1)
    end.time <- Sys.time()
    timeSubBoost = as.double(end.time-start.time,units = "secs")
    SubBoost_beta = outputSub$beta.cur
    SubBoost_model = which(SubBoost_beta[-1]!=0)
  }
  
  # Boosting
  mstop.max = 5000
  start.time <- Sys.time()
  mod <- glmboost(y = data$y, x = data$x, control = boost_control(mstop = mstop.max)) 
  cv10f <- cv(model.weights(mod), type = "kfold")
  cvm <-cvrisk(mod, papply = lapply,  folds = cv10f)
  m.stop = mstop(cvm)
  end.time <- Sys.time()
  timeBoost = as.double(end.time-start.time,units = "secs")
  mod.boost = mod[m.stop]
  Boost_model = which(coef(mod.boost, which=1:p)!=0)
  Boost_beta = c(attributes(coef(mod.boost, which=1:p))$offset, coef(mod.boost, which=1:p))
  
  # Twin Boosting
  mstop.max2 = 5000
  start.time <- Sys.time()
  cvm2 <- cv.bst(data$x, data$y, family="gaussian", ctrl = bst_control(twinboost = TRUE, twintype=1, coefir = Boost_beta[-1] ,
                                                                       xselect.init = Boost_model, mstop = mstop.max2), learner="ls", n.cores=1)
  mstop.optimal = which.min(cvm2$cv.error)
  twinBoost = bst(data$x, data$y, family="gaussian", ctrl = bst_control(twinboost = TRUE, twintype=1, coefir = Boost_beta[-1] ,
                                                                        xselect.init = Boost_model, mstop = mstop.optimal), learner="ls")
  end.time <- Sys.time()
  timeBoost2 = as.double(end.time-start.time,units = "secs")
  timeTwinBoost = timeBoost + timeBoost2
  TwinBoost_model = twinBoost$xselect
  TwinBoost_beta = c(attributes(coef(twinBoost))$offset, coef(twinBoost))
  
  Stability_results = NULL
  #if (s0<=10) {
  # Stability Selection for Boosting 
  start.time <- Sys.time()
  mod <- glmboost(y = data$y, x = data$x, control = boost_control(mstop = 1000)) 
  mod.stab = stabsel(mod, PFER = PFER, sampling.type = "SS", q = q_stabsel, papply=lapply) #cutoff = 0.6)
  end.time <- Sys.time()
  timeStability = as.double(end.time-start.time, units = "secs")
  Stability_model = as.vector(mod.stab$selected)
  Stability_beta = beta.hat(data,Stability_model)
  #}
  

  ## Lasso with cross-validation
  # Run cross-validation
  start.time <- Sys.time()
  mod_cv <- cv.glmnet(x=data$x, y=data$y, family="gaussian")
  Lasso_beta = coef(mod_cv, mod_cv$lambda.min) #lambda.min
  Lasso_model = which(Lasso_beta[-1] !=0)
  end.time <- Sys.time()
  timeLasso = as.double(end.time-start.time,units = "secs")
  
  
  ## Elastic net with cross-validation for both parameters (lambda and alpha)
  start.time <- Sys.time()
  mod_cv <- cva.glmnet(x=data$x, y=data$y, family="gaussian")
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
  
  ## Relaxed Lasso with cross-validation
  start.time <- Sys.time()
  mod_cv <- cv.glmnet(x=data$x, y=data$y, family="gaussian", relax=TRUE)
  ReLasso_beta = coef(mod_cv, s="lambda.min", gamma="gamma.min") #lambda.min
  ReLasso_model = which(ReLasso_beta[-1] !=0)
  end.time <- Sys.time()
  timeReLasso = as.double(end.time-start.time,units = "secs")
  
  ## Model selected by EBIC with least squares
  start.time <- Sys.time()
  # forward screening
  s_max = s_max
  regs=summary(regsubsets(as.matrix(data$x),data$y,intercept=TRUE,nvmax=s_max,method="forward"))
  modelmatrix=as.matrix(regs$which[,-1,drop=F])
  S = which(modelmatrix[s_max,])
  
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
  EBIC_model <- S
  EBIC_beta <- beta.hat(data,S)
  end.time <- Sys.time()
  timeEBIC = as.double(end.time-start.time,units = "secs")
  
  
  #XGBoost with variable selection (component-wise boosting)
  start.time <- Sys.time()
  cv <- xgb.cv(data = data$x , 
               label = data$y, 
               eta = tau_L2Boost, 
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
  
  bst <- xgboost(data = data$x , label = data$y, eta = 0.1, nrounds = iter.opt, nthread = 1,
                 updater = 'coord_descent', feature_selector = 'greedy', top_k = 1, base_score=0, verbose=FALSE, 
                 booster = "gblinear", objective = "reg:squarederror", callbacks = list(cb.gblinear.history()))
  coef_path <- xgb.gblinear.history(bst)
  XGBoostCD_beta <- coef_path[iter.opt,]
  XGBoostCD_model <- which(coef_path[iter.opt,-1]!=0)
  end.time <- Sys.time()
  timeXGBoostCD = as.double(end.time-start.time,units = "secs")

  
  ##########
  AdaSubBoost_results = method_res_scenario(model=AdaSubBoost_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeAdaSubBoost,beta=AdaSubBoost_beta,data=data)
  RSubBoost_results = method_res_scenario(model=RSubBoost_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeRSubBoost,beta=RSubBoost_beta,data=data)
  Boost_results = method_res_scenario(model=Boost_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeBoost,beta=Boost_beta,data=data)
  TwinBoost_results = method_res_scenario(model=TwinBoost_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeTwinBoost,beta=TwinBoost_beta,data=data)
  Stability_results = method_res_scenario(model=Stability_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeStability,beta=Stability_beta,data=data)
  Lasso_results = method_res_scenario(model=Lasso_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeLasso,beta=Lasso_beta,data=data)
  ENet_results = method_res_scenario(model=ENet_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeENet,beta=ENet_beta,data=data)
  ReLasso_results = method_res_scenario(model=ReLasso_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeReLasso,beta=ReLasso_beta,data=data)
  EBIC_results =  method_res_scenario(model=EBIC_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeEBIC,beta=EBIC_beta,data=data)
  XGBoostCD_results = method_res_scenario(model=XGBoostCD_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeXGBoostCD,beta=XGBoostCD_beta,data=data)
  
  results = list(RSubBoost_results = RSubBoost_results,
                 AdaSubBoost_results = AdaSubBoost_results, 
                 Boost_results = Boost_results, 
                 TwinBoost_results = TwinBoost_results,
                 Stability_results = Stability_results,
                 Lasso_results = Lasso_results, 
                 ENet_results = ENet_results,
                 ReLasso_results = ReLasso_results,
                 EBIC_results = EBIC_results,
                 XGBoostCD_results = XGBoostCD_results)
  print(paste("Simulated dataset ", i))
  print(results)
  return(results)
}


n = 100
n_test = 1000 
p = 1000
s0 = 10
randomS0 = FALSE
sigma.normal = 1
corr = 0.8
savings = 1

q = 20 # 10
size.fixed = NULL
tau = 0.01 
tau_L2Boost = 0.1
Iter = 5000
nstop = p / 2
const = 1 # gamma (Penalty for shrinkage in EBIC)
PFER = 2
q_stabsel = 15 
s_max = 15 
U_C = 25
  
K = p/q

nSim = 500 # 500

#numCores <- detectCores()
numCores = 1 # Increase number of cores for parallel computing

RNGkind("L'Ecuyer-CMRG")
set.seed(1)
results = mclapply(1:nSim, fsim, Iter=Iter, mc.cores = numCores, K = K, q = q, size.fixed = size.fixed, tau = tau, const = const, nstop = nstop, U_C=U_C, savings=1, PFER = PFER, q_stabsel = q_stabsel, 
                   s0 = s0, n = n, p = p, n_test = n_test, sigma.normal = sigma.normal, corr = corr, mc.set.seed = TRUE,  mc.preschedule = FALSE , randomS0=randomS0, s_max = s_max)



results$n = n 
results$n_test = n_test
results$p = p
results$s0 = s0
results$sigma.normal = sigma.normal
results$corr = corr
results$K = K
results$q = q
results$size.fixed = size.fixed
results$tau = tau
results$Iter = Iter
results$const = const
results$PFER = PFER
results$q_stabsel = q_stabsel
results$nSim = nSim
results$s_max = s_max
results$U_C = U_C



save(results, file="Sim_Toeplitz08_p1000_n100_nSim500_Iter5000_fixedS0_10_autostop.RData")

