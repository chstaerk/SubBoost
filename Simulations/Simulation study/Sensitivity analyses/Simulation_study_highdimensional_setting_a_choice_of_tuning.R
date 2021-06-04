#library("devtools")
#install_github("chstaerk/SubBoost")

library("SubBoost")


fsim <- function(i, Iter, K, q, size.fixed, tau, const, U_C=30, savings=1,
                 s0, n, p, n_test, sigma.normal, corr, nstop, randomS0, s_max,
                 K_values, q_values, s_max_values, U_C_values){
  set.seed(i)
  if (randomS0) {
    S0 = sort(sample(1:p,s0))
  } else {
  S0 = 1:s0
  }
  #S0 = seq(1, p, by = p/s0)
  #S0 = c(1:4, 101:104, 201:204, 301:304)
  beta = numeric(p)
  beta[S0] = runif(s0,-2,2)
  #beta[S0] = rep(c(-2,-1,1,2),4)
  
  if (p>2000) {
  data = simdata.toeplitz.corr.multiple(n =n ,q = p/10 , qrep=10, beta =beta ,sigma.normal = sigma.normal,corr=corr) 
  data.test = simdata.toeplitz.corr.multiple(n_test,q = p/10,qrep=10, beta=beta,sigma.normal= sigma.normal,corr= corr)
  } else { 
  data = simdata.toeplitz.corr(n =n , p=p, beta =beta ,sigma.normal = sigma.normal,corr=corr) 
  data.test = simdata.toeplitz.corr(n = n_test , p=p, beta =beta ,sigma.normal = sigma.normal,corr=corr) 
  }
  colnames(data$x) = 1:p
  
  # AdaSubBoost for different K_values
  AdaSubBoost_betas_K <- list()
  AdaSubBoost_models_K <- list()
  timeAdaSubBoost_K <- list()
  
  for (i in 1:length(K_values)) {
    start.time <- Sys.time()
    output = AdaSubBoost(data=data, Iter=Iter, size.fixed=size.fixed, K=K_values[i], q=q, tau = tau, const=const, U_C=U_C, savings=savings, nstop=nstop, plotting=FALSE, s_max=s_max)
    end.time <- Sys.time()
    timeAdaSubBoost_K[[i]] = as.double(end.time-start.time,units = "secs")
    AdaSubBoost_betas_K[[i]] = output$beta.cur
    AdaSubBoost_models_K[[i]] = which(output$beta.cur[-1]!=0)
  }
  
  # AdaSubBoost for different q_values and s_max_values
  AdaSubBoost_betas_q <- list()
  AdaSubBoost_models_q <- list()
  timeAdaSubBoost_q <- list()
  
  for (i in 1:length(q_values)) {
    start.time <- Sys.time()
    output = AdaSubBoost(data=data, Iter=Iter, size.fixed=size.fixed, K=p/q_values[i], q=q_values[i], tau = tau, const=const, U_C=U_C_values[i], savings=savings, nstop=nstop, plotting=FALSE, s_max=s_max_values[i])
    end.time <- Sys.time()
    timeAdaSubBoost_q[[i]] = as.double(end.time-start.time,units = "secs")
    AdaSubBoost_betas_q[[i]] = output$beta.cur
    AdaSubBoost_models_q[[i]] = which(output$beta.cur[-1]!=0)
  }

  # RSubBoost (without adapting probabilities, adaptive=FALSE)
  start.time <- Sys.time()
  outputRSub = AdaSubBoost(data=data, Iter=Iter, size.fixed=size.fixed, K=K, q=q, tau = tau, const=const, U_C=U_C, savings=savings,
                           adaptive=FALSE, nstop=nstop, plotting=FALSE, s_max=s_max)
  end.time <- Sys.time()
  timeRSubBoost = as.double(end.time-start.time,units = "secs")
  RSubBoost_beta = outputRSub$beta.cur
  RSubBoost_model = which(RSubBoost_beta[-1]!=0)
  

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
  
  AdaSubBoost_results_K = list()
  AdaSubBoost_results_q = list()
  ##########
  for (i in 1:length(K_values)) {
    AdaSubBoost_results_K[[i]] = method_results_scenario(model=AdaSubBoost_models_K[[i]],beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeAdaSubBoost_K[[i]],beta=AdaSubBoost_betas_K[[i]],data=data)
  }
  
  for (i in 1:length(q_values)) {
    AdaSubBoost_results_q[[i]] = method_results_scenario(model=AdaSubBoost_models_q[[i]],beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeAdaSubBoost_q[[i]],beta=AdaSubBoost_betas_q[[i]],data=data)
  }
  
  RSubBoost_results = method_results_scenario(model=RSubBoost_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeRSubBoost,beta=RSubBoost_beta,data=data)
 
  EBIC_results =  method_results_scenario(model=EBIC_model,beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeEBIC,beta=EBIC_beta,data=data)
  
  
  results = list(RSubBoost_results = RSubBoost_results,
                 AdaSubBoost_results_K = AdaSubBoost_results_K, 
                 AdaSubBoost_results_q = AdaSubBoost_results_q,
                 EBIC_results = EBIC_results)
  print(paste("Simulated dataset ", i))
  print(results)
  return(results)
}


n = 100 # 100
n_test = 1000 
p = 1000
s0 = 10 # 10
randomS0 = FALSE # FALSE
sigma.normal = 1
corr = 0.8
savings = 1
U_C = 25

size.fixed = NULL
tau = 0.01 
Iter = 5000 # 5000
nstop = p / 2
const = 1 # const = 1 # gamma (Penalty for shrinkage in EBIC)

q = 20 # 10
s_max = 15 

K = p/q # 100

K_values = c(1, 10, 50, 100, 1000)
q_values =     c(5, 10, 15, 20, 25)
s_max_values = c(2, 5, 10, 15, 20)
U_C_values = c(25, 25, 25, 25, 25)

nSim = 500

#numCores <- detectCores()
numCores = 1 # Increase number of cores for parallel computing

RNGkind("L'Ecuyer-CMRG")
set.seed(1)
results = mclapply(1:nSim, fsim, Iter=Iter, mc.cores = numCores, K = K, q = q, size.fixed = size.fixed, tau = tau, const = const, nstop = nstop, U_C=U_C, savings=1,  
                   s0 = s0, n = n, p = p, n_test = n_test, sigma.normal = sigma.normal, corr = corr, mc.set.seed = TRUE,  mc.preschedule = FALSE , randomS0=randomS0, s_max = s_max,
                   K_values = K_values, q_values = q_values, s_max_values = s_max_values, U_C_values = U_C_values)



results$n = n 
results$n_test = n_test
results$p = p
results$s0 = s0
results$sigma.normal = sigma.normal
results$corr = corr
results$size.fixed = size.fixed
results$tau = tau
results$Iter = Iter
results$const = const
results$nSim = nSim
results$U_C = U_C
results$K = K
results$q = q
results$s_max = s_max

results$K_values = K_values
results$q_values  = q_values 
results$s_max_values  = s_max_values 
results$U_C_values = U_C_values



save(results, file="Sim_Toeplitz08_p1000_n100_nSim500_Iter5000_fixedS0_10_autostop_choice_of_tuning.RData")

