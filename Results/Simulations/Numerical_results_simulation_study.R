library("xtable")

# Detailed numeric results 

take_element <- function(list,element1,element2,nSim){
  #l = length(list)
  l = nSim 
  vec = numeric(l)
  for (i in 1:l) vec[i] = list[[i]][[element1]][[element2]]
  return(vec)
}

take_element3 <- function(list,element1,element2,element3,nSim){
  l = nSim 
  vec = numeric(l)
  for (i in 1:l) vec[i] = list[[i]][[element1]][[element2]][[element3]]
  return(vec)
}



test_comp <- function(results, names_adjusted, metric, digi, nSim, results_criterion=NULL, names_adjusted_criterion=NULL) {
  
  mat <- matrix(NA, length(names_adjusted), nSim)

  for (i in 1:length(names_adjusted)){
    mat[i,] <- take_element(results,i,metric,nSim)
  }
  
  rownames(mat) <- names_adjusted
  
  if (!is.null(results_criterion)) {
    mat[1,] <- take_element3(results_criterion,1,3,metric,nSim)
    mat[2,] <- take_element3(results_criterion,2,3,metric,nSim)
    mat <- rbind(take_element3(results_criterion,1,1,metric,nSim),
                 take_element3(results_criterion,2,1,metric,nSim), mat)
    rownames(mat) <- names_adjusted_criterion
    names_adjusted <- names_adjusted_criterion
  }
  
  if (metric==4) mat <- sqrt(mat)
  
  means <- format(round(rowMeans(mat), digi), nsmall=digi)
  medians <- format(round(apply(mat ,1, median), digi), nsmall=digi)
  sd <- format(round(apply(mat ,1, sd), digi), nsmall=digi)
  IQR <- format(round(apply(mat ,1, IQR), digi), nsmall=digi)
  minimizer <- names(which.min(rowMeans(mat)))
  #minimizer <- names(which.min(medians))
  
  pvalues <- rep(NA, length(names_adjusted))
  names(pvalues) <- names_adjusted
  for (method in names_adjusted){
    if (method!=minimizer){
      wtest <- wilcox.test(mat[method,], mat[minimizer,], paired = TRUE, alternative = "two.sided")
      pvalues[method] <-   format(round(wtest$p.value, 3), nsmall=digi)
      if (pvalues[method]<0.001) pvalues[method] = "<0.001"
    }
  }
  
  return(list(means=means, sd=sd, medians=medians, IQR=IQR, pvalues=pvalues))
}

method_names <- c("SubBoost","RSubBoost","AdaSubBoost",
                  "L2Boost","TwinBoost","StabSel", "Lasso", "ENet", "ReLasso")
names_adjusted <- method_names

digi <- 3

load("Sim_Toeplitz08_p20_n100_nSim500_Iter1000_fixedS0_4_autostop_24_05.RData")
nSim = results$nSim

lowdim_FP <- test_comp(results, names_adjusted, 1, digi, nSim)
lowdim_FN <- test_comp(results, names_adjusted, 2, digi, nSim)
lowdim_Est <- test_comp(results, names_adjusted, 3, digi, nSim)
lowdim_Pred <- test_comp(results, names_adjusted, 4, digi, nSim)


results_tab <- rbind(
  paste(lowdim_FP$medians, " (", lowdim_FP$IQR,")", sep=""), 
  paste(lowdim_FP$means, " (", lowdim_FP$sd,")", sep=""), 
  lowdim_FP$pvalues,
  paste(lowdim_FN$medians, " (", lowdim_FN$IQR,")", sep=""), 
  paste(lowdim_FN$means, " (", lowdim_FN$sd,")", sep=""), 
  lowdim_FN$pvalues,
  paste(lowdim_Est$medians, " (", lowdim_Est$IQR,")", sep=""), 
  paste(lowdim_Est$means, " (", lowdim_Est$sd,")", sep=""), 
  lowdim_Est$pvalues,
  paste(lowdim_Pred$medians, " (", lowdim_Pred$IQR,")", sep=""), 
  paste(lowdim_Pred$means, " (", lowdim_Pred$sd,")", sep=""), 
  lowdim_Pred$pvalues)


results_tab <- cbind(rep(c("Median FP (IQR)", "Mean FP (SD)", "P-value", 
                           "Median FN (IQR)", "Mean FN (SD)", "P-value",
                           "Median MSE (IQR)", "Mean MSE (SD)", "P-value",
                           "Median RMSE (IQR)", "Mean RMSE (SD)", "P-value"), 1),
                     results_tab)
colnames(results_tab)[1] <- ""
#rownames(results_tab) <-  rep(c("Mean model size", "p-value", "Mean absolute error", "p-value"), 4)

print(xtable(results_tab), include.rownames=FALSE)

###################################################################

method_names <- c("RSubBoost","AdaSubBoost",
                  "L2Boost","TwinBoost","StabSel", "Lasso", "ENet", "ReLasso")
names_adjusted <- method_names

load("Sim_Toeplitz08_p1000_n100_nSim500_Iter5000_fixedS0_10_autostop_24_05.RData") 
setting_FP <- test_comp(results, names_adjusted, 1, digi, nSim)
setting_FN <- test_comp(results, names_adjusted, 2, digi, nSim)
setting_Est <- test_comp(results, names_adjusted, 3, digi, nSim)
setting_Pred <- test_comp(results, names_adjusted, 4, digi, nSim)

results_tab <- rbind(
  paste(setting_FP$medians, " (", setting_FP$IQR,")", sep=""), 
  paste(setting_FP$means, " (", setting_FP$sd,")", sep=""), 
  setting_FP$pvalues,
  paste(setting_FN$medians, " (", setting_FN$IQR,")", sep=""), 
  paste(setting_FN$means, " (", setting_FN$sd,")", sep=""), 
  setting_FN$pvalues,
  paste(setting_Est$medians, " (", setting_Est$IQR,")", sep=""), 
  paste(setting_Est$means, " (", setting_Est$sd,")", sep=""), 
  setting_Est$pvalues,
  paste(setting_Pred$medians, " (", setting_Pred$IQR,")", sep=""), 
  paste(setting_Pred$means, " (", setting_Pred$sd,")", sep=""), 
  setting_Pred$pvalues)


results_tab <- cbind(rep(c("Median FP (IQR)", "Mean FP (SD)", "P-value", 
                           "Median FN (IQR)", "Mean FN (SD)", "P-value",
                           "Median MSE (IQR)", "Mean MSE (SD)", "P-value",
                           "Median RMSE (IQR)", "Mean RMSE (SD)", "P-value"), 1),
                     results_tab)
colnames(results_tab)[1] <- ""
#rownames(results_tab) <-  rep(c("Mean model size", "p-value", "Mean absolute error", "p-value"), 4)

print(xtable(results_tab), include.rownames=FALSE)

###########################################################################

load("Sim_Toeplitz08_p1000_n100_nSim500_Iter5000_randomS0_10_autostop_24_05.RData")
setting_FP <- test_comp(results, names_adjusted, 1, digi, nSim)
setting_FN <- test_comp(results, names_adjusted, 2, digi, nSim)
setting_Est <- test_comp(results, names_adjusted, 3, digi, nSim)
setting_Pred <- test_comp(results, names_adjusted, 4, digi, nSim)

results_tab <- rbind(
  paste(setting_FP$medians, " (", setting_FP$IQR,")", sep=""), 
  paste(setting_FP$means, " (", setting_FP$sd,")", sep=""), 
  setting_FP$pvalues,
  paste(setting_FN$medians, " (", setting_FN$IQR,")", sep=""), 
  paste(setting_FN$means, " (", setting_FN$sd,")", sep=""), 
  setting_FN$pvalues,
  paste(setting_Est$medians, " (", setting_Est$IQR,")", sep=""), 
  paste(setting_Est$means, " (", setting_Est$sd,")", sep=""), 
  setting_Est$pvalues,
  paste(setting_Pred$medians, " (", setting_Pred$IQR,")", sep=""), 
  paste(setting_Pred$means, " (", setting_Pred$sd,")", sep=""), 
  setting_Pred$pvalues)


results_tab <- cbind(rep(c("Median FP (IQR)", "Mean FP (SD)", "P-value", 
                           "Median FN (IQR)", "Mean FN (SD)", "P-value",
                           "Median MSE (IQR)", "Mean MSE (SD)", "P-value",
                           "Median RMSE (IQR)", "Mean RMSE (SD)", "P-value"), 1),
                     results_tab)
colnames(results_tab)[1] <- ""
#rownames(results_tab) <-  rep(c("Mean model size", "p-value", "Mean absolute error", "p-value"), 4)

print(xtable(results_tab), include.rownames=FALSE)

#################################################################

method_names <- c("RSubBoost","AdaSubBoost",
                  "L2Boost","TwinBoost","StabSel", "Lasso", "ENet", "ReLasso")
names_adjusted <- method_names
names_adjusted_criterion <- c("RSubBoost AIC", "AdaSubBoost AIC", "RSubBoost EBIC1", "AdaSubBoost EBIC1",
                  "L2Boost","TwinBoost","StabSel", "Lasso", "ENet", "ReLasso")

load("Sim_Toeplitz08_p1000_n1000_nSim500_Iter5000_fixedS0_100_autostop_choice_of_criterion.RData")
results_criterion <- results
load("Sim_Toeplitz08_p1000_n1000_nSim500_Iter10000_fixedS0_100_autostop_24_05.RData")
setting_FP <- test_comp(results, names_adjusted, 1, digi, nSim, results_criterion, names_adjusted_criterion)
setting_FN <- test_comp(results, names_adjusted, 2, digi, nSim, results_criterion, names_adjusted_criterion)
setting_Est <- test_comp(results, names_adjusted, 3, digi, nSim, results_criterion, names_adjusted_criterion)
setting_Pred <- test_comp(results, names_adjusted, 4, digi, nSim, results_criterion, names_adjusted_criterion)


results_tab <- rbind(
  paste(setting_FP$medians, " (", setting_FP$IQR,")", sep=""), 
  paste(setting_FP$means, " (", setting_FP$sd,")", sep=""), 
  setting_FP$pvalues,
  paste(setting_FN$medians, " (", setting_FN$IQR,")", sep=""), 
  paste(setting_FN$means, " (", setting_FN$sd,")", sep=""), 
  setting_FN$pvalues,
  paste(setting_Est$medians, " (", setting_Est$IQR,")", sep=""), 
  paste(setting_Est$means, " (", setting_Est$sd,")", sep=""), 
  setting_Est$pvalues,
  paste(setting_Pred$medians, " (", setting_Pred$IQR,")", sep=""), 
  paste(setting_Pred$means, " (", setting_Pred$sd,")", sep=""), 
  setting_Pred$pvalues)


results_tab <- cbind(rep(c("Median FP (IQR)", "Mean FP (SD)", "P-value", 
                           "Median FN (IQR)", "Mean FN (SD)", "P-value",
                           "Median MSE (IQR)", "Mean MSE (SD)", "P-value",
                           "Median RMSE (IQR)", "Mean RMSE (SD)", "P-value"), 1),
                     results_tab)
colnames(results_tab)[1] <- ""
#rownames(results_tab) <-  rep(c("Mean model size", "p-value", "Mean absolute error", "p-value"), 4)

print(xtable(results_tab), include.rownames=FALSE)





