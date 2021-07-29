#library("devtools")
#install_github("chstaerk/SubBoost")

library("SubBoost")


fsim <- function(i, Iter, K, q, size.fixed, tau, const, U_C=30, savings=1,
                 s0, n, p, n_test, sigma.normal, corr, nstop, randomS0, s_max,
                 const_values){
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
  
 
  AdaSubBoost_betas_const <- list()
  AdaSubBoost_models_const <- list()
  timeAdaSubBoost_const <- list()
  
  RSubBoost_betas_const <- list()
  RSubBoost_models_const <- list()
  timeRSubBoost_const <- list()
  
  for (i in 1:length(const_values)) {
    const <- const_values[[i]]
    
    start.time <- Sys.time()
    output = AdaSubBoost(data=data, Iter=Iter, size.fixed=size.fixed, K=K, q=q, tau = tau, const=const, U_C=U_C, savings=savings, nstop=nstop, plotting=FALSE, s_max=s_max)
    end.time <- Sys.time()
    timeAdaSubBoost_const[[i]] = as.double(end.time-start.time,units = "secs")
    AdaSubBoost_betas_const[[i]] = output$beta.cur
    AdaSubBoost_models_const[[i]] = which(output$beta.cur[-1]!=0)

    # RSubBoost (without adapting probabilities, adaptive=FALSE)
    start.time <- Sys.time()
    outputRSub = AdaSubBoost(data=data, Iter=Iter, size.fixed=size.fixed, K=K, q=q, tau = tau, const=const, U_C=U_C, savings=savings,
                           adaptive=FALSE, nstop=nstop, plotting=FALSE, s_max=s_max)
    end.time <- Sys.time()
    timeRSubBoost_const[[i]] = as.double(end.time-start.time,units = "secs")
    RSubBoost_betas_const[[i]] = outputRSub$beta.cur
    RSubBoost_models_const[[i]] = which(outputRSub$beta.cur[-1]!=0)
  }
  
  AdaSubBoost_results_const = list()
  RSubBoost_results_const = list()
  ##########
  for (i in 1:length(const_values)) {
    AdaSubBoost_results_const[[i]] = method_res_scenario(model=AdaSubBoost_models_const[[i]],beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeAdaSubBoost_const[[i]],beta=AdaSubBoost_betas_const[[i]],data=data)
    RSubBoost_results_const[[i]] = method_res_scenario(model=RSubBoost_models_const[[i]],beta1=beta,s0=s0,data.test=data.test,n_test=n_test,time=timeRSubBoost_const[[i]],beta=RSubBoost_betas_const[[i]],data=data)
  }
  
  results = list(RSubBoost_results_const = RSubBoost_results_const,
                 AdaSubBoost_results_const = AdaSubBoost_results_const)
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

const_values = list("AIC", 0, 1)

nSim = 500 # 500

#numCores <- detectCores()
numCores = 1 # Increase number of cores for parallel computing

RNGkind("L'Ecuyer-CMRG")
set.seed(1)
results = mclapply(1:nSim, fsim, Iter=Iter, mc.cores = numCores, K = K, q = q, size.fixed = size.fixed, tau = tau, const = const, nstop = nstop, U_C=U_C, savings=1,  
                   s0 = s0, n = n, p = p, n_test = n_test, sigma.normal = sigma.normal, corr = corr, mc.set.seed = TRUE,  mc.preschedule = FALSE , randomS0=randomS0, s_max = s_max,
                   const_values = const_values)



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

results$const_values = const_values


save(results, file="Sim_Toeplitz08_p1000_n100_nSim500_Iter5000_fixedS0_10_autostop_choice_of_criterion.RData")

