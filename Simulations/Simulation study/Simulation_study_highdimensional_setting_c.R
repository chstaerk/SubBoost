#library("devtools")
#install_github("chstaerk/SubBoost")
#install.packages("glmnetUtils")

library("SubBoost")
library("glmnetUtils")


fsim <- function(i, Iter, K, q, size.fixed, tau, const, U_C=25, savings=1, PFER, q_stabsel,
                 s0, n, p, n_test, sigma.normal, corr, nstop, randomS0, s_max, mstop.max, mstop.max2){
  set.seed(i)
  if (randomS0) {
    S0 = sort(sample(1:p,s0))
  } else {
  S0 = 1:s0
  }
  #S0 = seq(1, p, by = p/s0)
  #S0 = c(1:4, 101:104, 201:204, 301:304)
  #S0 = 1:40
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
  mstop.max = mstop.max
  start.time <- Sys.time()
  mod <- glmboost(y = data$y, x = data$x, control = boost_control(mstop = mstop.max)) 
  cv10f <- cv(model.weights(mod), type = "kfold")
  cvm <-cvrisk(mod, papply = lapply,  folds = cv10f)
  m.stop = mstop(cvm)
  end.time <- Sys.time()
  timeBoost = as.double(end.time-start.time,units = "secs")
  mod.boost = mod[m.stop]
  Boost_model = which(coef(mod.boost, which=1:p)!=0)
  Boost_beta = c(0,coef(mod.boost, which=1:p))
  
  # Twin Boosting
  mstop.max2 = mstop.max2
  start.time <- Sys.time()
  cvm2 <- cv.bst(data$x, data$y, family="gaussian", ctrl = bst_control(twinboost = TRUE, twintype=1, coefir = Boost_beta[-1] ,
                                                                       xselect.init = Boost_model, mstop = mstop.max2), learner="ls")
  mstop.optimal = which.min(cvm2$cv.error)
  twinBoost = bst(data$x, data$y, family="gaussian", ctrl = bst_control(twinboost = TRUE, twintype=1, coefir = Boost_beta[-1] ,
                                                                        xselect.init = Boost_model, mstop = mstop.optimal), learner="ls")
  end.time <- Sys.time()
  timeBoost2 = as.double(end.time-start.time,units = "secs")
  timeTwinBoost = timeBoost + timeBoost2
  TwinBoost_model = twinBoost$xselect
  TwinBoost_beta = c(0,coef(twinBoost))
  
  # Stability Selection for Boosting 
  Stability_results = NULL
  #if (s0<=10) {
  start.time <- Sys.time()
  mod <- glmboost(y = data$y, x = data$x, control = boost_control(mstop = 5000)) 
  mod.stab = stabsel(mod, PFER = PFER, sampling.type = "SS", q = q_stabsel) #cutoff = 0.6)
  end.time <- Sys.time()
  timeStability = as.double(end.time-start.time, units = "secs")
  Stability_model = as.vector(mod.stab$selected)
  Stability_beta = beta.hat(data,Stability_model)
  #}
  
  ## Adaptive Lasso with Cross-Validation in first step and EBIC in second step
  #start.time <- Sys.time()
  #cv.fit1=cv.glmnet(data$x,data$y,intercept=TRUE,family="gaussian",standardize=FALSE,alpha=1)
  #betas.cv.fit1=as.vector(predict(cv.fit1,type="coefficients",s="lambda.1se"))[-1]
  #new_penalty=rep(0,p)
  #new_penalty=1/pmax(abs(betas.cv.fit1),.Machine$double.eps)
  #AdaLasso.fit2= glmnet(data$x,data$y,penalty.factor=new_penalty,family="gaussian",intercept=TRUE,standardize=FALSE,alpha=1,dfmax=n-3)
  #AdaLasso.path <- predict(AdaLasso.fit2,type="coefficients")
  #nonzeros.AdaLasso.path = predict(AdaLasso.fit2,type="nonzero")
  #length.path = dim(AdaLasso.path)[2]
  #vec = numeric(length.path)
  #for (j in 1:length.path){
  #  vec[j] = EBIC_beta(data,AdaLasso.path[,j],const) 
  #}
  #mini = which.min(vec)
  #AdaLasso_model = nonzeros.AdaLasso.path[[mini]]
  #if (is.null(AdaLasso_model)) AdaLasso_model = integer(0)
  #AdaLasso_beta = AdaLasso.path[,mini]
  #end.time <- Sys.time()
  #timeAdaLasso = as.double(end.time-start.time,units = "secs")
  
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
  ENet_beta = coef(mod_cv, alpha=alpha_min) 
  ENet_model = which(ENet_beta[-1] !=0)
  end.time <- Sys.time()
  timeENet = as.double(end.time-start.time,units = "secs")
  
  ## Elastic net with fixed alpha=0.5 and cross-validation for lambda only
  # Run cross-validation
  #start.time <- Sys.time()
  #mod_cv <- cv.glmnet(x=data$x, y=data$y, family="gaussian", alpha=0.5)
  #ENet_beta2 = coef(mod_cv, mod_cv$lambda.min) #lambda.min
  #Lasso_beta = coef(mod_cv, mod_cv$lambda.1se)  
  #ENet_model2 = which(ENet_beta2[-1] !=0)
  #end.time <- Sys.time()
  #timeENet2 = as.double(end.time-start.time,units = "secs")
  
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
  
  
  ##########
  AdaSubBoost_results = method_results_scenario(model=AdaSubBoost_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeAdaSubBoost,beta=AdaSubBoost_beta,data=data)
  RSubBoost_results = method_results_scenario(model=RSubBoost_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeRSubBoost,beta=RSubBoost_beta,data=data)
  Boost_results = method_results_scenario(model=Boost_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeBoost,beta=Boost_beta,data=data)
  TwinBoost_results = method_results_scenario(model=TwinBoost_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeTwinBoost,beta=TwinBoost_beta,data=data)
  Stability_results = method_results_scenario(model=Stability_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeStability,beta=Stability_beta,data=data)
  #AdaLasso_results = method_results_scenario(model=AdaLasso_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeAdaLasso,beta=AdaLasso_beta,data=data)
  Lasso_results = method_results_scenario(model=Lasso_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeLasso,beta=Lasso_beta,data=data)
  ENet_results = method_results_scenario(model=ENet_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeENet,beta=ENet_beta,data=data)
  ReLasso_results = method_results_scenario(model=ReLasso_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeReLasso,beta=ReLasso_beta,data=data)
  EBIC_results =  method_results_scenario(model=EBIC_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeEBIC,beta=EBIC_beta,data=data)
  
  
  results = list(RSubBoost_results = RSubBoost_results,
                 AdaSubBoost_results = AdaSubBoost_results, 
                 Boost_results = Boost_results, 
                 TwinBoost_results = TwinBoost_results,
                 Stability_results = Stability_results,
                 Lasso_results = Lasso_results, 
                 #AdaLasso_results = AdaLasso_results, 
                 ENet_results = ENet_results,
                 ReLasso_results = ReLasso_results,
                 EBIC_results = EBIC_results)
  print(paste("Simulated dataset ", i))
  print(results)
  return(results)
}


n = 1000
n_test = 1000 
p = 1000
s0 = 100
randomS0 = FALSE
sigma.normal = 1
corr = 0.8
savings = 1
U_C = 25

q = 20
size.fixed = NULL
s_max = 15
tau = 0.01 # 0.01
tau_L2Boost = 0.1
Iter = 10000
nstop = p / 2
const = "AIC" # AIC
PFER = 2
q_stabsel = 150  
mstop.max = 10000
mstop.max2 = 10000
K = p/q

nSim = 500 

#numCores <- detectCores()
numCores = 1 # Increase number of cores for parallel computing

RNGkind("L'Ecuyer-CMRG")
set.seed(1)
results = mclapply(1:nSim, fsim, Iter=Iter, mc.cores = numCores, K = K, q = q, size.fixed = size.fixed, tau = tau, const = const, nstop = nstop, U_C=U_C, savings=1, PFER = PFER, q_stabsel = q_stabsel, 
                   s0 = s0, n = n, p = p, n_test = n_test, sigma.normal = sigma.normal, corr = corr, mc.set.seed = TRUE,  mc.preschedule = FALSE , randomS0=randomS0, s_max=s_max, mstop.max=mstop.max, mstop.max2=mstop.max2)



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



save(results, file="Sim_Toeplitz08_p1000_n1000_nSim500_Iter10000_fixedS0_100_autostop_24_05.RData")

