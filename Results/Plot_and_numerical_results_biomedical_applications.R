
library(unikn)

#setwd("//epi-login//daten//CS//AdaSubBoost_Revision")

load("LOOCV_Bodyfat_CV_25_07.RData")
load("LOOCV_Diabetes_CV_25_07.RData") 
load("LOOCV_Riboflavin_CV_25_07.RData")
load("LOOCV_PCR_CV_25_07.RData")



take_element <- function(list,element1){
  l = length(list)
  vec = numeric(l)
  for (i in 1:l) vec[i] = list[[i]][[element1]]
  return(vec)
}

take_element(LOOCV_results,1)
results = LOOCV_results


######## Plots 

par(mfrow=c(4,2))
#par(mar=c(5.1,4.1,4.1,2.1))
par(mar=c(4.3,3,4,1))

color_mean = c("black")

pal_col <- rev(seecol(pal_signal, n = 4, alpha = 0.67))

color_boxes = c(pal_col, "gray", "gray", "gray", "gray", "gray", "gray")


### Bodyfat
load("LOOCV_Bodyfat_CV_25_07.RData")
results = LOOCV_results

method_names = c("SubBoost", "RSubBoost", "AdaSubBoost", "L2Boost", "XGBoost", "TwinBoost", "StabSel",
                 "Lasso", "ENet", "ReLasso")

cex.size = 1.3
cex.lab.size = 1.1
cex.axis.size = 0.9
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)
par(font.axis = 2)
angle = 45
cex.names = 1
boxplot(take_element(results,"modelsizes_SubBoost"), 
        take_element(results,"modelsizes_RSubBoost"),
        take_element(results,"modelsizes_AdaSubBoost"), 
        take_element(results,"modelsizes_Boosting"), 
        take_element(results,"modelsizes_XGBoostCD"),  
        take_element(results,"modelsizes_TwinBoost"), take_element(results,"modelsizes_Stability"),
        take_element(results,"modelsizes_Lasso"), 
        take_element(results,"modelsizes_ENet"),
        take_element(results,"modelsizes_ReLasso"), 
        xaxt="n", 
        main ="Bodyfat data (n=71, p=9)", ylim=c(0,10),
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.5, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Number of selected variables"),  line = 0.4, cex.main = 1.1)

points( c(mean(take_element(results,"modelsizes_SubBoost")), 
          mean(take_element(results,"modelsizes_RSubBoost")), 
          mean(take_element(results,"modelsizes_AdaSubBoost")), 
          mean(take_element(results,"modelsizes_Boosting")), 
          mean(take_element(results,"modelsizes_XGBoostCD")),  
          mean(take_element(results,"modelsizes_TwinBoost")), 
          mean(take_element(results,"modelsizes_Stability")),
          mean(take_element(results,"modelsizes_Lasso")),
          mean(take_element(results,"modelsizes_ENet")),
          mean(take_element(results,"modelsizes_ReLasso"))),col=color_mean,pch=4,cex=1.2,lwd=3)

boxplot(sqrt(take_element(results,"SqE_SubBoost")), 
        sqrt(take_element(results,"SqE_RSubBoost")), 
        sqrt(take_element(results,"SqE_AdaSubBoost")), 
        sqrt(take_element(results,"SqE_Boosting")), 
        sqrt(take_element(results,"SqE_XGBoostCD")), 
        sqrt(take_element(results,"SqE_TwinBoost")), 
        sqrt(take_element(results,"SqE_Stability")),
        sqrt(take_element(results,"SqE_Lasso")),
        sqrt(take_element(results,"SqE_ENet")),
        sqrt(take_element(results,"SqE_ReLasso")),
        xaxt="n", main ="Bodyfat data (n=71, p=9)", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
title(main = ("Absolute prediction error"),  line = 0.4, cex.main = 1.1)
text(1:length(method_names), par("usr")[3] - 1.5, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
points( c(mean(sqrt(take_element(results,"SqE_SubBoost"))), mean(sqrt(take_element(results,"SqE_RSubBoost"))), mean(sqrt(take_element(results,"SqE_AdaSubBoost"))), 
          mean(sqrt(take_element(results,"SqE_Boosting"))), mean(sqrt(take_element(results,"SqE_XGBoostCD"))), 
          mean(sqrt(take_element(results,"SqE_TwinBoost"))), mean(sqrt(take_element(results,"SqE_Stability"))),
          mean(sqrt(take_element(results,"SqE_Lasso"))), mean(sqrt(take_element(results,"SqE_ENet"))), mean(sqrt(take_element(results,"SqE_ReLasso")))),col=color_mean,pch=4,cex=1.2,lwd=3)


### Diabetes
load("LOOCV_Diabetes_CV_25_07.RData")

results = LOOCV_results

cex.size = 1.3
cex.lab.size = 1.1
cex.axis.size = 0.82
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)
par(font.axis = 2)
boxplot(take_element(results,"modelsizes_SubBoost"), 
        take_element(results,"modelsizes_RSubBoost"),
        take_element(results,"modelsizes_AdaSubBoost"), 
        take_element(results,"modelsizes_Boosting"), 
        take_element(results,"modelsizes_XGBoostCD"),  
        take_element(results,"modelsizes_TwinBoost"), take_element(results,"modelsizes_Stability"),
        take_element(results,"modelsizes_Lasso"), 
        take_element(results,"modelsizes_ENet"),
        take_element(results,"modelsizes_ReLasso"), 
        xaxt="n", 
        main ="Diabetes data (n=442, p=10)", ylim=c(0,10),
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.5, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Number of selected variables"),  line = 0.4, cex.main = 1.1)

points( c(mean(take_element(results,"modelsizes_SubBoost")), 
          mean(take_element(results,"modelsizes_RSubBoost")), 
          mean(take_element(results,"modelsizes_AdaSubBoost")), 
          mean(take_element(results,"modelsizes_Boosting")), 
          mean(take_element(results,"modelsizes_XGBoostCD")),  
          mean(take_element(results,"modelsizes_TwinBoost")), 
          mean(take_element(results,"modelsizes_Stability")),
          mean(take_element(results,"modelsizes_Lasso")),
          mean(take_element(results,"modelsizes_ENet")),
          mean(take_element(results,"modelsizes_ReLasso"))),col=color_mean,pch=4,cex=1.2,lwd=3)

boxplot(sqrt(take_element(results,"SqE_SubBoost")), 
        sqrt(take_element(results,"SqE_RSubBoost")), 
        sqrt(take_element(results,"SqE_AdaSubBoost")), 
        sqrt(take_element(results,"SqE_Boosting")), 
        sqrt(take_element(results,"SqE_XGBoostCD")), 
        sqrt(take_element(results,"SqE_TwinBoost")), 
        sqrt(take_element(results,"SqE_Stability")),
        sqrt(take_element(results,"SqE_Lasso")),
        sqrt(take_element(results,"SqE_ENet")),
        sqrt(take_element(results,"SqE_ReLasso")),
        xaxt="n", main ="Diabetes data (n=442, p=10)", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
title(main = ("Absolute prediction error"),  line = 0.4, cex.main = 1.1)
text(1:length(method_names), par("usr")[3] - 7, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
points( c(mean(sqrt(take_element(results,"SqE_SubBoost"))), mean(sqrt(take_element(results,"SqE_RSubBoost"))), mean(sqrt(take_element(results,"SqE_AdaSubBoost"))), 
          mean(sqrt(take_element(results,"SqE_Boosting"))), mean(sqrt(take_element(results,"SqE_XGBoostCD"))),
          mean(sqrt(take_element(results,"SqE_TwinBoost"))), mean(sqrt(take_element(results,"SqE_Stability"))),
          mean(sqrt(take_element(results,"SqE_Lasso"))), mean(sqrt(take_element(results,"SqE_ENet"))), mean(sqrt(take_element(results,"SqE_ReLasso")))),col=color_mean,pch=4,cex=1.2,lwd=3)


color_boxes = c(pal_col[2:4], "gray", "gray", "gray", "gray", "gray", "gray", "gray")


method_names = c("RSubBoost", "AdaSubBoost", "L2Boost", "XGBoost", "TwinBoost", "StabSel",
                 "Lasso", "ENet", "ReLasso")


### Riboflavin
load("LOOCV_Riboflavin_CV_25_07.RData")
results = LOOCV_results

median(take_element(results,"modelsizes_RSubBoost"))
median(take_element(results,"modelsizes_AdaSubBoost"))
median(take_element(results,"modelsizes_Boosting"))
median(take_element(results,"modelsizes_XGBoostCD"))
median(take_element(results,"modelsizes_TwinBoost"))
median(take_element(results,"modelsizes_Stability"))
median(take_element(results,"modelsizes_Lasso"))
median(take_element(results,"modelsizes_ENet"))
median(take_element(results,"modelsizes_ReLasso"))

cex.size = 1.3
cex.lab.size = 1.1
cex.axis.size = 0.9
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)
par(font.axis = 2)
boxplot(take_element(results,"modelsizes_RSubBoost"),
        take_element(results,"modelsizes_AdaSubBoost"), 
        take_element(results,"modelsizes_Boosting"), 
        take_element(results,"modelsizes_XGBoostCD"), 
        take_element(results,"modelsizes_TwinBoost"), take_element(results,"modelsizes_Stability"),
        take_element(results,"modelsizes_Lasso"), take_element(results,"modelsizes_ENet"), take_element(results,"modelsizes_ReLasso"), 
        xaxt="n", 
        main ="Riboflavin data (n=71, p=4088)", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 12, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Number of selected variables"),  line = 0.4, cex.main = 1.1)

points( c(mean(take_element(results,"modelsizes_RSubBoost")), 
          mean(take_element(results,"modelsizes_AdaSubBoost")), 
          mean(take_element(results,"modelsizes_Boosting")), 
          mean(take_element(results,"modelsizes_XGBoostCD")), 
          mean(take_element(results,"modelsizes_TwinBoost")), 
          mean(take_element(results,"modelsizes_Stability")),
          mean(take_element(results,"modelsizes_Lasso")),
          mean(take_element(results,"modelsizes_ENet")),
          mean(take_element(results,"modelsizes_ReLasso"))),col=color_mean,pch=4,cex=1.2,lwd=3)
boxplot(sqrt(take_element(results,"SqE_RSubBoost")), 
        sqrt(take_element(results,"SqE_AdaSubBoost")), 
        sqrt(take_element(results,"SqE_Boosting")), 
        sqrt(take_element(results,"SqE_XGBoostCD")), 
        sqrt(take_element(results,"SqE_TwinBoost")), 
        sqrt(take_element(results,"SqE_Stability")),
        sqrt(take_element(results,"SqE_Lasso")),
        sqrt(take_element(results,"SqE_ENet")),
        sqrt(take_element(results,"SqE_ReLasso")),
        xaxt="n", main ="Riboflavin data (n=71, p=4088)", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.15, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Absolute prediction error"),  line = 0.4, cex.main = 1.1)
points( c(mean(sqrt(take_element(results,"SqE_RSubBoost"))), mean(sqrt(take_element(results,"SqE_AdaSubBoost"))), 
          mean(sqrt(take_element(results,"SqE_Boosting"))),
          mean(sqrt(take_element(results,"SqE_XGBoostCD"))), 
          mean(sqrt(take_element(results,"SqE_TwinBoost"))), mean(sqrt(take_element(results,"SqE_Stability"))),
          mean(sqrt(take_element(results,"SqE_Lasso"))), mean(sqrt(take_element(results,"SqE_ENet"))), mean(sqrt(take_element(results,"SqE_ReLasso")))),col=color_mean,pch=4,cex=1.2,lwd=3)


### PCR
load("LOOCV_PCR_CV_25_07.RData")

results = LOOCV_results


median(take_element(results,"modelsizes_RSubBoost"))
median(take_element(results,"modelsizes_AdaSubBoost"))
median(take_element(results,"modelsizes_Boosting"))
mean(take_element(results,"modelsizes_Boosting"))
median(take_element(results,"modelsizes_XGBoostCD"))
median(take_element(results,"modelsizes_TwinBoost"))
median(take_element(results,"modelsizes_Stability"))
median(take_element(results,"modelsizes_Lasso"))
median(take_element(results,"modelsizes_ENet"))
median(take_element(results,"modelsizes_ReLasso"))

cex.size = 1.3
cex.lab.size = 1.1
cex.axis.size = 0.9
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)
par(font.axis = 2)
boxplot(take_element(results,"modelsizes_RSubBoost"),
        take_element(results,"modelsizes_AdaSubBoost"), 
        take_element(results,"modelsizes_Boosting"), 
        take_element(results,"modelsizes_XGBoostCD"), 
        take_element(results,"modelsizes_TwinBoost"), take_element(results,"modelsizes_Stability"),
        take_element(results,"modelsizes_Lasso"), take_element(results,"modelsizes_ENet"), take_element(results,"modelsizes_ReLasso"),  
        xaxt="n", 
        main ="PCR data (n=60, p=22,575)",
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 30, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Number of selected variables"),  line = 0.4, cex.main = 1.1)
points( c(mean(take_element(results,"modelsizes_RSubBoost")), 
          mean(take_element(results,"modelsizes_AdaSubBoost")), 
          mean(take_element(results,"modelsizes_Boosting")), 
          mean(take_element(results,"modelsizes_XGBoostCD")), 
          mean(take_element(results,"modelsizes_TwinBoost")), 
          mean(take_element(results,"modelsizes_Stability")),
          mean(take_element(results,"modelsizes_Lasso")),
          mean(take_element(results,"modelsizes_ENet")),
          mean(take_element(results,"modelsizes_ReLasso"))),col=color_mean,pch=4,cex=1.2,lwd=3)

boxplot(sqrt(take_element(results,"SqE_RSubBoost")), 
        sqrt(take_element(results,"SqE_AdaSubBoost")), 
        sqrt(take_element(results,"SqE_Boosting")), 
        sqrt(take_element(results,"SqE_XGBoostCD")), 
        sqrt(take_element(results,"SqE_TwinBoost")), 
        sqrt(take_element(results,"SqE_Stability")),
        sqrt(take_element(results,"SqE_Lasso")),
        sqrt(take_element(results,"SqE_ENet")),
        sqrt(take_element(results,"SqE_ReLasso")),
        xaxt="n", main ="PCR data (n=60, p=22,575)  ", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.1, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Absolute prediction error"),  line = 0.4, cex.main = 1.1)
points( c(mean(sqrt(take_element(results,"SqE_RSubBoost"))), mean(sqrt(take_element(results,"SqE_AdaSubBoost"))), 
          mean(sqrt(take_element(results,"SqE_Boosting"))), 
          mean(sqrt(take_element(results,"SqE_XGBoostCD"))),  
          mean(sqrt(take_element(results,"SqE_TwinBoost"))), mean(sqrt(take_element(results,"SqE_Stability"))),
          mean(sqrt(take_element(results,"SqE_Lasso"))), mean(sqrt(take_element(results,"SqE_ENet"))), mean(sqrt(take_element(results,"SqE_ReLasso")))),col=color_mean,pch=4,cex=1.2,lwd=3)


###################################

# Detailed numeric results 

test_comp <- function(results, names_adjusted, metric, digi) {
  
  mat <- matrix(NA, length(names_adjusted), length(results))
  rownames(mat) <- names_adjusted
  
  for (method in names_adjusted){
    if (metric=="time"){
      mat[method,] <- take_element(results, paste(metric, method, sep=""))
    } else {
      mat[method,] <- take_element(results, paste(metric, method, sep="_"))
    }
  }
  
  if (metric=="SqE") mat <- sqrt(mat)
  
  
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

names_adjusted <- method_names
names_adjusted[names_adjusted=="L2Boost"] <- "Boosting"
names_adjusted[names_adjusted=="StabSel"] <- "Stability"
names_adjusted[names_adjusted=="XGBoost"] <- "XGBoostCD"

names_adjusted_low <- c("SubBoost", names_adjusted)

digi <- 2

load("LOOCV_Bodyfat_CV_25_07.RData")
SqE_bodyfat <- test_comp(LOOCV_results, names_adjusted_low, "SqE", digi)
modelsizes_bodyfat <- test_comp(LOOCV_results, names_adjusted_low, "modelsizes", digi)
time_bodyfat <- test_comp(LOOCV_results, names_adjusted_low, "time", digi)


load("LOOCV_Diabetes_CV_25_07.RData") 
SqE_diabetes <- test_comp(LOOCV_results, names_adjusted_low, "SqE", digi)
modelsizes_diabetes <- test_comp(LOOCV_results, names_adjusted_low, "modelsizes", digi)
time_diabetes <- test_comp(LOOCV_results, names_adjusted_low, "time", digi)


load("LOOCV_Riboflavin_CV_25_07.RData")
SqE_riboflavin <- test_comp(LOOCV_results, names_adjusted, "SqE", digi)
modelsizes_riboflavin  <- test_comp(LOOCV_results, names_adjusted, "modelsizes", digi)
time_riboflavin  <- test_comp(LOOCV_results, names_adjusted, "time", digi)


load("LOOCV_PCR_CV_25_07.RData")
SqE_PCR <- test_comp(LOOCV_results, names_adjusted, "SqE", digi)
modelsizes_PCR <- test_comp(LOOCV_results, names_adjusted, "modelsizes", digi)
time_PCR <- test_comp(LOOCV_results, names_adjusted, "time", digi)


results_tab <- rbind(
               paste(modelsizes_bodyfat$medians, " (", modelsizes_bodyfat$IQR,")", sep=""), 
               paste(modelsizes_bodyfat$means, " (", modelsizes_bodyfat$sd,")", sep=""), 
               modelsizes_bodyfat$pvalues,
               paste(SqE_bodyfat$medians, " (", SqE_bodyfat$IQR,")", sep=""), 
               paste(SqE_bodyfat$means, " (", SqE_bodyfat$sd,")", sep=""), 
               SqE_bodyfat$pvalues,
               paste(time_bodyfat$means, " (", time_bodyfat$sd,")", sep=""),
               
               paste(modelsizes_diabetes$medians, " (", modelsizes_diabetes$IQR,")", sep=""), 
               paste(modelsizes_diabetes$means, " (", modelsizes_diabetes$sd,")", sep=""), 
               modelsizes_diabetes$pvalues,
               paste(SqE_diabetes$medians, " (", SqE_diabetes$IQR,")", sep=""), 
               paste(SqE_diabetes$means, " (", SqE_diabetes$sd,")", sep=""), 
               SqE_diabetes$pvalues,
               paste(time_diabetes$means, " (", time_diabetes$sd,")", sep=""),
               
               
               c("", paste(modelsizes_riboflavin$medians, " (", modelsizes_riboflavin$IQR,")", sep="")), 
               c("", paste(modelsizes_riboflavin$means, " (", modelsizes_riboflavin$sd,")", sep="")), 
               c("", modelsizes_riboflavin$pvalues),
               c("", paste(SqE_riboflavin$medians, " (", SqE_riboflavin$IQR,")", sep="")), 
               c("", paste(SqE_riboflavin$means, " (", SqE_riboflavin$sd,")", sep="")), 
               c("", SqE_riboflavin$pvalues),
               c("", paste(time_riboflavin$means, " (", time_riboflavin$sd,")", sep="")), 
               
               
               c("", paste(modelsizes_PCR$medians, " (", modelsizes_PCR$IQR,")", sep="")),
               c("", paste(modelsizes_PCR$means, " (", modelsizes_PCR$sd,")", sep="")), 
               c("", modelsizes_PCR$pvalues),
               c("", paste(SqE_PCR$medians, " (", SqE_PCR$IQR,")", sep="")), 
               c("", paste(SqE_PCR$means, " (", SqE_PCR$sd,")", sep="")), 
               c("", SqE_PCR$pvalues),
               c("", paste(time_PCR$means, " (", time_PCR$sd,")", sep="")))

results_tab <- cbind(rep(c("Median model size (IQR)", "Mean model size (SD)", "P-value", 
                           "Median LOOCV error (IQR)", "Mean LOOCV error (SD)", "P-value",
                           "Mean Time in s (SD)"), 4),
                     results_tab)
colnames(results_tab)[1] <- ""
#rownames(results_tab) <-  rep(c("Mean model size", "p-value", "Mean absolute error", "p-value"), 4)


##### Detailed numerical results 

library("xtable")

print(xtable(results_tab), include.rownames=FALSE)



