library("unikn")

# Load results for high-dimensional setting (a)
load("Sim_Toeplitz08_p1000_n100_nSim500_Iter5000_fixedS0_10_autostop_choice_of_tuning.RData")


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

n_algos = length(results[[1]])
nSim = results$nSim


results$K_values
results$q_values



par(mar=c(3,3,4,1))
color_mean = c("black")

pal_col <- rev(seecol(pal_signal, n = 4, alpha = 0.67))

color_boxes = c(pal_col[2], rep(pal_col[3],7))


#method_names = c("RSubBoost", paste("AdaSubBoost", 1:5, sep=""))#, "EBIC")
#method_names = paste("K = ",  c(0, results$K_values[-length(results$K_values)]), sep="")
method_names = paste("K = ",  c(0, results$K_values), sep="")

cex.size = 1.2
cex.lab.size = 1.1
cex.axis.size = 1
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)
par(font.axis = 2)
cex.names = 1

par(mfrow=c(2,2))
boxplot(take_element(results,1,1,nSim), 
        take_element3(results,2,1,1,nSim), take_element3(results,2,2,1,nSim), take_element3(results,2,3,1,nSim), take_element3(results,2,4,1,nSim), take_element3(results,2,5,1,nSim),
        names = method_names, main ="High-dimensional simulation setting (a)", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
title(main = "False Positives",  line = 0.4, cex.main = 1.1)

boxplot(take_element(results,1,2,nSim), 
        take_element3(results,2,1,2,nSim), take_element3(results,2,2,2,nSim), take_element3(results,2,3,2,nSim), take_element3(results,2,4,2,nSim), take_element3(results,2,5,2,nSim),
        names = method_names, main ="High-dimensional simulation setting (a)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "False Negatives",  line = 0.4, cex.main = 1.1)

#boxplot(take_element(results,1,3,nSim), 
#        take_element3(results,2,1,3,nSim), take_element3(results,2,2,3,nSim), take_element3(results,2,3,3,nSim), take_element3(results,2,4,3,nSim), take_element3(results,2,5,3,nSim),
#        names =method_names, main ="Estimation Error (MSE)", 
#        col = color_boxes,
#        outcol=color_boxes,outpch = 19)

boxplot(take_element(results,1,4,nSim), 
        take_element3(results,2,1,4,nSim), take_element3(results,2,2,4,nSim), take_element3(results,2,3,4,nSim), take_element3(results,2,4,4,nSim), take_element3(results,2,5,4,nSim),
        names = method_names, main ="High-dimensional simulation setting (a)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "Prediction Error (RMSE)",  line = 0.4, cex.main = 1.1)


boxplot(take_element(results,1,5,nSim), 
        take_element3(results,2,1,5,nSim), take_element3(results,2,2,5,nSim), take_element3(results,2,3,5,nSim), take_element3(results,2,4,5,nSim), take_element3(results,2,5,5,nSim),
        names = method_names, main ="High-dimensional simulation setting (a)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19, 
        ylim=c(0,max(take_element(results,1,5,nSim), 
                     take_element3(results,2,1,5,nSim), 
                     take_element3(results,2,2,5,nSim), 
                     take_element3(results,2,3,5,nSim), 
                     take_element3(results,2,4,5,nSim), 
                     take_element3(results,2,5,5,nSim))))
title(main = "Computational time (in s)",  line = 0.4, cex.main = 1.1)


################################################################################################
par(mgp=c(3,1.2,0))
method_names = #c("RSub q = 20\n smax = 15", 
  paste("q = ",   results$q_values[1:5], "\n smax = ", results$s_max_values[1:5], sep="")

color_boxes = rep(pal_col[3],5)

par(mfrow=c(2,2))
boxplot(#take_element(results,1,1,nSim), 
        take_element3(results,3,1,1,nSim), take_element3(results,3,2,1,nSim), take_element3(results,3,3,1,nSim), take_element3(results,3,4,1,nSim), take_element3(results,3,5,1,nSim),
        names = method_names, main ="High-dimensional simulation setting (a)", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
title(main = "False Positives",  line = 0.4, cex.main = 1.1)

boxplot(#take_element(results,1,2,nSim), 
        take_element3(results,3,1,2,nSim), take_element3(results,3,2,2,nSim), take_element3(results,3,3,2,nSim), take_element3(results,3,4,2,nSim), take_element3(results,3,5,2,nSim),
        names = method_names, main ="High-dimensional simulation setting (a)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "False Negatives",  line = 0.4, cex.main = 1.1)

#boxplot(take_element(results,1,3,nSim), 
#        take_element3(results,3,1,3,nSim), take_element3(results,3,2,3,nSim), take_element3(results,3,3,3,nSim), take_element3(results,3,4,3,nSim), take_element3(results,3,5,3,nSim),
#        names =method_names, main ="Estimation Error (MSE)", 
#        col = color_boxes,
#        outcol=color_boxes,outpch = 19)
boxplot(#take_element(results,1,4,nSim), 
        take_element3(results,3,1,4,nSim), take_element3(results,3,2,4,nSim), take_element3(results,3,3,4,nSim), take_element3(results,3,4,4,nSim), take_element3(results,3,5,4,nSim),
        names = method_names, main ="High-dimensional simulation setting (a)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "Prediction Error (RMSE)",  line = 0.4, cex.main = 1.1)

boxplot(#take_element(results,1,5,nSim), 
  take_element3(results,3,1,5,nSim), take_element3(results,3,2,5,nSim), take_element3(results,3,3,5,nSim), take_element3(results,3,4,5,nSim), take_element3(results,3,5,5,nSim),
  names = method_names, main ="High-dimensional simulation setting (a)", 
  col = color_boxes,
  outcol=color_boxes,outpch = 19, 
  ylim=c(0,max(take_element3(results,3,1,5,nSim), 
               take_element3(results,3,2,5,nSim), 
               take_element3(results,3,3,5,nSim), 
               take_element3(results,3,4,5,nSim), 
               take_element3(results,3,5,5,nSim))))
title(main = "Computational time (in s)",  line = 0.4, cex.main = 1.1)
par(mgp=c(3,1,0))


###################################################################################

# Load results for high-dimensional setting (b)
# Uncomment the following line for setting (b)
load("Sim_Toeplitz08_p1000_n100_nSim500_Iter5000_randomS0_10_autostop_choice_of_tuning.RData")

color_boxes = c(pal_col[2], rep(pal_col[3],7))
method_names = paste("K = ",  c(0, results$K_values), sep="")

par(mfrow=c(2,2))
boxplot(take_element(results,1,1,nSim), 
        take_element3(results,2,1,1,nSim), take_element3(results,2,2,1,nSim), take_element3(results,2,3,1,nSim), take_element3(results,2,4,1,nSim), take_element3(results,2,5,1,nSim),
        names = method_names, main ="High-dimensional simulation setting (b)", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
title(main = "False Positives",  line = 0.4, cex.main = 1.1)

boxplot(take_element(results,1,2,nSim), 
        take_element3(results,2,1,2,nSim), take_element3(results,2,2,2,nSim), take_element3(results,2,3,2,nSim), take_element3(results,2,4,2,nSim), take_element3(results,2,5,2,nSim),
        names = method_names, main ="High-dimensional simulation setting (b)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "False Negatives",  line = 0.4, cex.main = 1.1)

#boxplot(take_element(results,1,3,nSim), 
#        take_element3(results,2,1,3,nSim), take_element3(results,2,2,3,nSim), take_element3(results,2,3,3,nSim), take_element3(results,2,4,3,nSim), take_element3(results,2,5,3,nSim),
#        names =method_names, main ="Estimation Error (MSE)", 
#        col = color_boxes,
#        outcol=color_boxes,outpch = 19)

boxplot(take_element(results,1,4,nSim), 
        take_element3(results,2,1,4,nSim), take_element3(results,2,2,4,nSim), take_element3(results,2,3,4,nSim), take_element3(results,2,4,4,nSim), take_element3(results,2,5,4,nSim),
        names = method_names, main ="High-dimensional simulation setting (b)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "Prediction Error (RMSE)",  line = 0.4, cex.main = 1.1)


boxplot(take_element(results,1,5,nSim), 
        take_element3(results,2,1,5,nSim), take_element3(results,2,2,5,nSim), take_element3(results,2,3,5,nSim), take_element3(results,2,4,5,nSim), take_element3(results,2,5,5,nSim),
        names = method_names, main ="High-dimensional simulation setting (b)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19, 
        ylim=c(0,max(take_element(results,1,5,nSim), 
                     take_element3(results,2,1,5,nSim), 
                     take_element3(results,2,2,5,nSim), 
                     take_element3(results,2,3,5,nSim), 
                     take_element3(results,2,4,5,nSim), 
                     take_element3(results,2,5,5,nSim))))
title(main = "Computational time (in s)",  line = 0.4, cex.main = 1.1)


################################################################################################
par(mgp=c(3,1.2,0))
method_names = #c("RSub q = 20\n smax = 15", 
  paste("q = ",   results$q_values[1:5], "\n smax = ", results$s_max_values[1:5], sep="")

color_boxes = rep(pal_col[3],5)

par(mfrow=c(2,2))
boxplot(#take_element(results,1,1,nSim), 
  take_element3(results,3,1,1,nSim), take_element3(results,3,2,1,nSim), take_element3(results,3,3,1,nSim), take_element3(results,3,4,1,nSim), take_element3(results,3,5,1,nSim),
  names = method_names, main ="High-dimensional simulation setting (b)", 
  col = color_boxes,
  outcol= color_boxes,outpch = 19)
title(main = "False Positives",  line = 0.4, cex.main = 1.1)

boxplot(#take_element(results,1,2,nSim), 
  take_element3(results,3,1,2,nSim), take_element3(results,3,2,2,nSim), take_element3(results,3,3,2,nSim), take_element3(results,3,4,2,nSim), take_element3(results,3,5,2,nSim),
  names = method_names, main ="High-dimensional simulation setting (b)", 
  col = color_boxes,
  outcol=color_boxes,outpch = 19)
title(main = "False Negatives",  line = 0.4, cex.main = 1.1)

#boxplot(take_element(results,1,3,nSim), 
#        take_element3(results,3,1,3,nSim), take_element3(results,3,2,3,nSim), take_element3(results,3,3,3,nSim), take_element3(results,3,4,3,nSim), take_element3(results,3,5,3,nSim),
#        names =method_names, main ="Estimation Error (MSE)", 
#        col = color_boxes,
#        outcol=color_boxes,outpch = 19)
boxplot(#take_element(results,1,4,nSim), 
  take_element3(results,3,1,4,nSim), take_element3(results,3,2,4,nSim), take_element3(results,3,3,4,nSim), take_element3(results,3,4,4,nSim), take_element3(results,3,5,4,nSim),
  names = method_names, main ="High-dimensional simulation setting (b)", 
  col = color_boxes,
  outcol=color_boxes,outpch = 19)
title(main = "Prediction Error (RMSE)",  line = 0.4, cex.main = 1.1)

boxplot(#take_element(results,1,5,nSim), 
  take_element3(results,3,1,5,nSim), take_element3(results,3,2,5,nSim), take_element3(results,3,3,5,nSim), take_element3(results,3,4,5,nSim), take_element3(results,3,5,5,nSim),
  names = method_names, main ="High-dimensional simulation setting (b)", 
  col = color_boxes,
  outcol=color_boxes,outpch = 19, 
  ylim=c(0,max(take_element3(results,3,1,5,nSim), 
               take_element3(results,3,2,5,nSim), 
               take_element3(results,3,3,5,nSim), 
               take_element3(results,3,4,5,nSim), 
               take_element3(results,3,5,5,nSim))))
title(main = "Computational time (in s)",  line = 0.4, cex.main = 1.1)
par(mgp=c(3,1,0))


###################################################################################

# Load results for high-dimensional setting (c)
load("Sim_Toeplitz08_p1000_n1000_nSim500_Iter5000_fixedS0_100_autostop_choice_of_tuning.RData")

color_boxes = c(pal_col[2], rep(pal_col[3],7))
method_names = paste("K = ",  c(0, results$K_values), sep="")

par(mfrow=c(2,2))
boxplot(take_element(results,1,1,nSim), 
        take_element3(results,2,1,1,nSim), take_element3(results,2,2,1,nSim), take_element3(results,2,3,1,nSim), take_element3(results,2,4,1,nSim), take_element3(results,2,5,1,nSim),
        names = method_names, main ="High-dimensional simulation setting (c)", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
title(main = "False Positives",  line = 0.4, cex.main = 1.1)

boxplot(take_element(results,1,2,nSim), 
        take_element3(results,2,1,2,nSim), take_element3(results,2,2,2,nSim), take_element3(results,2,3,2,nSim), take_element3(results,2,4,2,nSim), take_element3(results,2,5,2,nSim),
        names = method_names, main ="High-dimensional simulation setting (c)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "False Negatives",  line = 0.4, cex.main = 1.1)

#boxplot(take_element(results,1,3,nSim), 
#        take_element3(results,2,1,3,nSim), take_element3(results,2,2,3,nSim), take_element3(results,2,3,3,nSim), take_element3(results,2,4,3,nSim), take_element3(results,2,5,3,nSim),
#        names =method_names, main ="Estimation Error (MSE)", 
#        col = color_boxes,
#        outcol=color_boxes,outpch = 19)

boxplot(take_element(results,1,4,nSim), 
        take_element3(results,2,1,4,nSim), take_element3(results,2,2,4,nSim), take_element3(results,2,3,4,nSim), take_element3(results,2,4,4,nSim), take_element3(results,2,5,4,nSim),
        names = method_names, main ="High-dimensional simulation setting (c)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "Prediction Error (RMSE)",  line = 0.4, cex.main = 1.1)


boxplot(take_element(results,1,5,nSim), 
        take_element3(results,2,1,5,nSim), take_element3(results,2,2,5,nSim), take_element3(results,2,3,5,nSim), take_element3(results,2,4,5,nSim), take_element3(results,2,5,5,nSim),
        names = method_names, main ="High-dimensional simulation setting (c)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19, 
        ylim=c(0,max(take_element(results,1,5,nSim), 
                     take_element3(results,2,1,5,nSim), 
                     take_element3(results,2,2,5,nSim), 
                     take_element3(results,2,3,5,nSim), 
                     take_element3(results,2,4,5,nSim), 
                     take_element3(results,2,5,5,nSim))))
title(main = "Computational time (in s)",  line = 0.4, cex.main = 1.1)


################################################################################################
par(mgp=c(3,1.2,0))
method_names = #c("RSub q = 20\n smax = 15", 
  paste("q = ",   results$q_values[1:5], "\n smax = ", results$s_max_values[1:5], sep="")

color_boxes = rep(pal_col[3],5)

par(mfrow=c(2,2))
boxplot(#take_element(results,1,1,nSim), 
  take_element3(results,3,1,1,nSim), take_element3(results,3,2,1,nSim), take_element3(results,3,3,1,nSim), take_element3(results,3,4,1,nSim), take_element3(results,3,5,1,nSim),
  names = method_names, main ="High-dimensional simulation setting (c)", 
  col = color_boxes,
  outcol= color_boxes,outpch = 19)
title(main = "False Positives",  line = 0.4, cex.main = 1.1)

boxplot(#take_element(results,1,2,nSim), 
  take_element3(results,3,1,2,nSim), take_element3(results,3,2,2,nSim), take_element3(results,3,3,2,nSim), take_element3(results,3,4,2,nSim), take_element3(results,3,5,2,nSim),
  names = method_names, main ="High-dimensional simulation setting (c)", 
  col = color_boxes,
  outcol=color_boxes,outpch = 19)
title(main = "False Negatives",  line = 0.4, cex.main = 1.1)

#boxplot(take_element(results,1,3,nSim), 
#        take_element3(results,3,1,3,nSim), take_element3(results,3,2,3,nSim), take_element3(results,3,3,3,nSim), take_element3(results,3,4,3,nSim), take_element3(results,3,5,3,nSim),
#        names =method_names, main ="Estimation Error (MSE)", 
#        col = color_boxes,
#        outcol=color_boxes,outpch = 19)
boxplot(#take_element(results,1,4,nSim), 
  take_element3(results,3,1,4,nSim), take_element3(results,3,2,4,nSim), take_element3(results,3,3,4,nSim), take_element3(results,3,4,4,nSim), take_element3(results,3,5,4,nSim),
  names = method_names, main ="High-dimensional simulation setting (c)", 
  col = color_boxes,
  outcol=color_boxes,outpch = 19)
title(main = "Prediction Error (RMSE)",  line = 0.4, cex.main = 1.1)

boxplot(#take_element(results,1,5,nSim), 
  take_element3(results,3,1,5,nSim), take_element3(results,3,2,5,nSim), take_element3(results,3,3,5,nSim), take_element3(results,3,4,5,nSim), take_element3(results,3,5,5,nSim),
  names = method_names, main ="High-dimensional simulation setting (c)", 
  col = color_boxes,
  outcol=color_boxes,outpch = 19, 
  ylim=c(0,max(take_element3(results,3,1,5,nSim), 
               take_element3(results,3,2,5,nSim), 
               take_element3(results,3,3,5,nSim), 
               take_element3(results,3,4,5,nSim), 
               take_element3(results,3,5,5,nSim))))
title(main = "Computational time (in s)",  line = 0.4, cex.main = 1.1)
par(mgp=c(3,1,0))



