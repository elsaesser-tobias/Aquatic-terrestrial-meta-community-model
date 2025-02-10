library(ggplot2);
library(deSolve);
library(simecol);
library(OCNet);
library(reticulate);
library(readxl);
library(pls);

library(openxlsx);
library(dplyr);

#clear enviroment
rm(list = ls())



#landscape parameters
res_urban_landscape_percent <- c()
res_forest_landscape_percent <- c()
res_agriculture_landscape_percent <- c()
res_n_steps <- c()
res_n_steps_2 <- c()
res_max_range_cluster <- c()
res_landscape_type <- c()

res_mean_habitat_quality <- c()

#ODE parameters
res_aPred1_1 <- c()
res_aPred1_2 <- c()
res_aPred2_1 <- c()
res_aPred2_2 <- c()
res_hPred1 <- c()
res_hPred2 <- c()
res_mPred1 <- c()
res_mPred2 <- c()
res_modPred1 <- c()
res_modPred2 <- c()
res_prey1_growth_speed <- c()
res_prey2_growth_speed <- c()
res_capacity_prey1 <- c()
res_capacity_prey2 <- c()
res_dispersal_percent_1 <- c()
res_dispersal_percent_2 <- c()

#ODE results
res_sum_pred1 <- c()
res_sum_pred2 <- c()
res_sum_prey1 <- c()
res_sum_prey2 <- c()

res_sum_D1 <- c()
res_sum_D2 <- c()

n_landscape <- 1000
n_ODE <- 1
# coexistance per habitat
endpoints <- matrix(ncol = 85, nrow = n_landscape * n_ODE)
hq_matrix <- matrix(ncol = 21, nrow = n_landscape * n_ODE)



for(q in 1:n_landscape){
  load(paste("workspace_end",q,".RData", sep = ""))
  D_1_c <- D_1
  D_2_c <- D_2
  
  
  for(j in 1:n_ODE){
    D_1 <- D_1_c
    D_2 <- D_2_c
    #randomized starting points
    percentage_Pred1 <- 1
    percentage_Pred2 <- 1
    percentage_Prey1 <- 1
    percentage_Prey2 <- 1
    
    set.seed(q*10000 + j)
    Pred1_spots <- runif(length(habitatsx))
    
    
    Pred2_spots <- runif(length(habitatsx))
    
    
    Prey1_spots <- runif(length(habitatsx))
    
    
    Prey2_spots <- runif(length(habitatsx))
    
    for(i in 1:length(Pred1_spots)){
      if(Pred1_spots[i] <= percentage_Pred1){
        Pred1_spots[i] <- 50
      }
      else{
        Pred1_spots[i] <- 0
      }
    }
    
    for(i in 1:length(Pred2_spots)){
      if(Pred2_spots[i] <= percentage_Pred2){
        Pred2_spots[i] <- 50
      }
      else{
        Pred2_spots[i] <- 0
      }
    }
    
    for(i in 1:length(Prey1_spots)){
      if(Prey1_spots[i] <= percentage_Prey1){
        Prey1_spots[i] <- 300
      }
      else{
        Prey1_spots[i] <- 0
      }
    }
    
    for(i in 1:length(Prey2_spots)){
      if(Prey2_spots[i] <= percentage_Prey2){
        Prey2_spots[i] <- 300
      }
      else{
        Prey2_spots[i] <- 0
      }
    }
    
    
    #set parameters REMEMBER TO SET SEEDS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    P_aPred1_1 = 0.00666
    P_aPred1_1 = sample(seq(P_aPred1_1*0.5,P_aPred1_1*1.5, P_aPred1_1*0.0001),1)
    
    P_aPred1_2 = 0.00444
    P_aPred1_2 = sample(seq(P_aPred1_2*0.5,P_aPred1_2*1.5, P_aPred1_2*0.0001),1)
    
    P_aPred2_1 = 0.00666
    P_aPred2_1 = sample(seq(P_aPred2_1*0.5,P_aPred2_1*1.5, P_aPred2_1*0.0001),1)
    
    P_aPred2_2 = 0.00444
    P_aPred2_2 = sample(seq(P_aPred2_2*0.5,P_aPred2_2*1.5, P_aPred2_2*0.0001),1)
    
    P_hPred1 = 0.0192
    P_hPred1 = sample(seq(P_hPred1*0.5,P_hPred1*1.5, P_hPred1*0.0001),1)
    
    P_hPred2 = 0.0192
    P_hPred2 = sample(seq(P_hPred2*0.5,P_hPred2*1.5, P_hPred2*0.0001),1)
    
    P_mPred1 = 0.11
    P_mPred1 = sample(seq(P_mPred1*0.5,P_mPred1*1.5, P_mPred1*0.0001),1)
    
    P_mPred2 = 0.11
    P_mPred2 = sample(seq(P_mPred2*0.5,P_mPred2*1.5, P_mPred2*0.0001),1)
    
    P_modPred1 = 0.025
    P_modPred1 = sample(seq(P_modPred1*0.5,P_modPred1*1.5, P_modPred1*0.0001),1)
    
    P_modPred2 = 0.025
    P_modPred2 = sample(seq(P_modPred2*0.5,P_modPred2*1.5, P_modPred2*0.0001),1)
    
    P_prey1_growth_speed = 0.05
    P_prey1_growth_speed = sample(seq(P_prey1_growth_speed*0.5,P_prey1_growth_speed*1.5,
                                      P_prey1_growth_speed*0.0001),1)
    
    P_prey2_growth_speed = 0.05
    P_prey2_growth_speed = sample(seq(P_prey2_growth_speed*0.5,P_prey2_growth_speed*1.5,
                                      P_prey2_growth_speed*0.0001),1)
    P_capacity_prey1 = 4000
    P_capacity_prey1 = sample(seq(P_capacity_prey1*0.5,P_capacity_prey1*1.5, P_capacity_prey1*0.0001),1)
    
    P_capacity_prey2 = 8000
    P_capacity_prey2 = sample(seq(P_capacity_prey2*0.5,P_capacity_prey2*1.5, P_capacity_prey2*0.0001),1)
    
    #dispersal_percent <- 0.00833
    dispersal_percent <- 0.0833
    dispersal <- sample(seq(dispersal_percent*0.5,dispersal_percent*1.5, dispersal_percent*0.0001),2)
    dispersal_percent_1 <- dispersal[1]
    dispersal_percent_2 <- dispersal[2]
    
    D_1 <- D_1 *0.001 * dispersal_percent_1
    D_2 <- D_2 *0.001 * dispersal_percent_2
    
    
    
    #diag(D_1) <- - rowSums(D_2) * 1.1
    #diag(D_2) <- - rowSums(D_2) * 1.1
    diag(D_1) <- - dispersal_percent_1
    diag(D_2) <- - dispersal_percent_2
    
    parameters <- c(aPred1_1 = P_aPred1_1,
                    aPred1_2 = P_aPred1_2,#prefer aqua
                    aPred2_1 = P_aPred2_1,
                    aPred2_2 = P_aPred2_2,
                    hPred1 = P_hPred1,
                    hPred2 = P_hPred2,
                    mPred1 = P_mPred1,
                    mPred2 = P_mPred2,
                    modPred1 = P_modPred1,
                    modPred2 = P_modPred2,
                    prey1_growth_speed = P_prey1_growth_speed, #ohne predation über eine saison 5x
                    prey2_growth_speed = P_prey2_growth_speed, #90% mortality über life cycle
                    capacity_prey1 = P_capacity_prey1,
                    capacity_prey2 = P_capacity_prey2,
                    n_loc = length(habitatsx))
    
    
    ODE_functions<-function(t, state, parameters) {
      #state[state<0] = 0
      with(as.list(parameters),{
        
        Pred1 <- state[1:n_loc]
        Pred2 <- state[(n_loc+1):(2*n_loc)]
        Prey1 <- state[(2*n_loc+1):(3*n_loc)]
        Prey2 <- state[(3*n_loc +1):(4*n_loc)]
        
        
        
        dPred1 <- (((aPred1_1 * Pred1 * Prey1) / (1 + aPred1_1 * hPred1 * Prey1)) +
                     ((aPred1_2 * Pred1 * Prey2) / (1 + aPred1_2 * hPred1 * Prey2))) * modPred1 -
          Pred1 * mPred1 + D_1 %*% Pred1
        
        dPred2 <- (((aPred2_1 * Pred2 * Prey1) / (1 + aPred2_1 * hPred2 * Prey1)) +
                     ((aPred2_2 * Pred2 * Prey2) / (1 + aPred2_2 * hPred2 * Prey2))) * modPred2 -
          Pred2 * mPred2  + D_2 %*% Pred2
        
        
        dPrey1 <- prey1_growth_speed * (habitat_quality * capacity_prey1 - Prey1) -
          ((aPred1_1 * Pred1 * Prey1) / (1 + aPred1_1 * hPred1 * Prey1)) - 
          ((aPred2_1 * Pred2 * Prey1) / (1 + aPred2_1 * hPred2 * Prey1))
        
        dPrey2 <- prey2_growth_speed * (capacity_prey2 - Prey2) -
          ((aPred1_2 * Pred1 * Prey2) / (1 + aPred1_2 * hPred1 * Prey2)) - 
          ((aPred2_2 * Pred2 * Prey2) / (1 + aPred2_2 * hPred2 * Prey2))
        
        return(list(c(dPred1,dPred2,dPrey1,dPrey2)))
      })
      
    }
    
    
    
    initial_state <- c(Pred1_spots, Pred2_spots, Prey1_spots, Prey2_spots)
    
    times <- seq(0, 1000, by = 1)
    
    out <- lsoda(y = initial_state, times = times, func = ODE_functions, parms = parameters,
                 rtol = 1e-12, atol = 1e-12, maxsteps = 1000)
    
    
    
    #sum up species
    result <- list()
    #sort results by habitat
    for(i in 1:length(habitatsx)){
      result[[paste("habitat", i, sep = "")]][["time"]] <- out[,1]
      for(o in 0:3){
        if(o == 0)
          result[[paste("habitat", i, sep = "")]][["Pred1"]] <- out[,o*length(habitatsx) + i + 1]
        if(o == 1)
          result[[paste("habitat", i, sep = "")]][["Pred2"]] <- out[,o*length(habitatsx) + i + 1]
        if(o == 2)
          result[[paste("habitat", i, sep = "")]][["Prey1"]] <- out[,o*length(habitatsx) + i + 1]
        result[[paste("habitat", i, sep = "")]][["Prey2"]] <- out[,o*length(habitatsx) + i + 1]
      }
    }
    
    
    print(q*100 - 100 + j)
    endpoints[sum(complete.cases(hq_matrix)) + 1,] <- out[1001,]
    hq_matrix[sum(complete.cases(hq_matrix)) + 1,] <- habitat_quality
    
    
    sum_pred1 <- 0
    sum_pred2 <- 0
    sum_prey1 <- 0
    sum_prey2 <- 0
    
    diag(D_1) <- 0
    diag(D_2) <- 0
    
    D_1 <- D_1 *1000 * (1/ dispersal_percent_1)
    D_2 <- D_2 *1000 * (1/dispersal_percent_2)
    
    sum_D1 <- sum(D_1)
    sum_D2 <- sum(D_2)
    
    l <- 1
    for(i in result){
      sum_pred1 <- i$Pred1[length(times)] + sum_pred1
      sum_pred2 <- i$Pred2[length(times)] + sum_pred2
      sum_prey1 <- i$Prey1[length(times)] + sum_prey1
      sum_prey2 <- i$Prey2[length(times)] + sum_prey2
      l <- l +1
    }
    
    
    
    #append results and parameters to list
    #landscape parameters
    res_urban_landscape_percent[length(res_urban_landscape_percent) + 1] <- urban_landscape_percent
    res_forest_landscape_percent[length(res_forest_landscape_percent) + 1] <- forest_landscape_percent
    res_agriculture_landscape_percent[length(res_agriculture_landscape_percent) + 1] <- agriculture_landscape_percent
    res_n_steps[length(res_n_steps) + 1] <- n_steps
    res_n_steps_2[length(res_n_steps_2) + 1] <- n_steps_2
    res_max_range_cluster[length(res_max_range_cluster) + 1] <- max_range_cluster
    res_landscape_type[length(res_landscape_type) + 1] <- landscape_type
    
    res_mean_habitat_quality[length(res_mean_habitat_quality) + 1] <- mean(habitat_quality)
    
    #ODE parameters
    res_aPred1_1[length(res_aPred1_1) + 1] <- P_aPred1_1
    res_aPred1_2[length(res_aPred1_2) + 1] <- P_aPred1_2
    res_aPred2_1[length(res_aPred2_1) + 1] <- P_aPred2_1
    res_aPred2_2[length(res_aPred2_2) + 1] <- P_aPred2_2
    res_hPred1[length(res_hPred1) + 1] <- P_hPred1
    res_hPred2[length(res_hPred2) + 1] <- P_hPred2
    res_mPred1[length(res_mPred1) + 1] <- P_mPred1
    res_mPred2[length(res_mPred2) + 1] <- P_mPred2
    res_modPred1[length(res_modPred1) + 1] <- P_modPred1
    res_modPred2[length(res_modPred2) + 1] <- P_modPred2
    res_prey1_growth_speed[length(res_prey1_growth_speed) + 1] <- P_prey1_growth_speed
    res_prey2_growth_speed[length(res_prey2_growth_speed) + 1] <- P_prey2_growth_speed
    res_capacity_prey1[length(res_capacity_prey1) + 1] <- P_capacity_prey1
    res_capacity_prey2[length(res_capacity_prey2) + 1] <- P_capacity_prey2
    res_dispersal_percent_1[length(res_dispersal_percent_1) + 1] <- dispersal_percent_1
    res_dispersal_percent_2[length(res_dispersal_percent_2) + 1] <- dispersal_percent_2
    
    #if(round(sum_pred1) != 0 & round(sum_pred2) != 0){stop("test")} 
    #ODE results
    res_sum_pred1[length(res_sum_pred1) + 1] <- sum_pred1
    res_sum_pred2[length(res_sum_pred2) + 1] <- sum_pred2
    res_sum_prey1[length(res_sum_prey1) + 1] <- sum_prey1
    res_sum_prey2[length(res_sum_prey2) + 1] <- sum_prey2
    
    
    res_sum_D1[length(res_sum_D1) + 1] <- sum_D1
    res_sum_D2[length(res_sum_D2) + 1] <- sum_D2
    
    
  }
}

RES <- data.frame(res_urban_landscape_percent,res_forest_landscape_percent, res_agriculture_landscape_percent,
                  res_n_steps, res_n_steps_2, res_max_range_cluster,res_aPred1_1,res_aPred1_2,
                  res_aPred2_1,res_aPred2_2,res_hPred1,res_hPred2, res_mPred1, res_mPred2,
                  res_modPred1, res_modPred2, res_prey1_growth_speed, res_prey2_growth_speed,
                  res_capacity_prey1, res_capacity_prey2, res_dispersal_percent_1, res_dispersal_percent_2,
                  res_sum_pred1, res_sum_pred2, res_sum_prey1, res_sum_prey2, res_landscape_type, 
                  res_sum_D1, res_sum_D2, res_mean_habitat_quality)


split_df <- split(RES,RES$res_landscape_type)


#
#exponential decay curve
# Function to fit exponential decay model
fit_exp_decay <- function(x, y) {
  nls(y ~ a * exp(-b * x), 
      start = list(a = max(y), b = 0.01),
      lower = list(a = 0, b = 0),
      upper = list(a = Inf, b = Inf),
      algorithm = "port")
}

# Fit the models
model_1 <- fit_exp_decay(split_df$`1`$res_max_range_cluster, split_df$`1`$res_sum_D1)
model_2 <- fit_exp_decay(split_df$`2`$res_max_range_cluster, split_df$`2`$res_sum_D1)
model_3 <- fit_exp_decay(split_df$`3`$res_max_range_cluster, split_df$`3`$res_sum_D1)

# Extracting coefficients
coef_1 <- coef(model_1)
a_1 <- coef_1["a"]
b_1 <- coef_1["b"]

coef_2 <- coef(model_2)
a_2 <- coef_2["a"]
b_2 <- coef_2["b"]

coef_3 <- coef(model_3)
a_3 <- coef_3["a"]
b_3 <- coef_3["b"]

col2 <- rgb(220, 20, 60, maxColorValue = 255)
col1 <- rgb(65, 105, 225, maxColorValue = 255)

minx <- min(c(split_df$`1`$res_max_range_cluster,split_df$`2`$res_max_range_cluster,split_df$`3`$res_max_range_cluster)) + 100
miny <- min(c(split_df$`1`$res_sum_D1 , split_df$`2`$res_sum_D1, split_df$`3`$res_sum_D1)) + 100

maxx <- max(c(split_df$`1`$res_max_range_cluster,split_df$`2`$res_max_range_cluster,split_df$`3`$res_max_range_cluster)) - 100
maxy <- max(c(split_df$`1`$res_sum_D1 , split_df$`2`$res_sum_D1, split_df$`3`$res_sum_D1)) - 100

# Plotting the data and fits
plot(split_df$`1`$res_max_range_cluster, split_df$`1`$res_sum_D1, col = "black", pch = 19,
     xlab = "Cluster Range", ylab = "Amount of Dispersal", yaxt = "n", xaxt = "n", cex.main = 1.8, cex.lab = 1.50,
     ylim = c(miny,maxy), xlim = c(minx,maxx), xaxs = "i", yaxs = "i")
curve(a_1 * exp(-b_1 * x), add = TRUE, col = "black", lwd = 4, range(minx,maxx))
axis(side = 2, at = c(5000, 10000, 15000), cex.axis = 1.25)
axis(side = 1, at = c(3000, 6000, 9000), cex.axis = 1.25)

points(split_df$`2`$res_max_range_cluster, split_df$`2`$res_sum_D1, col = col1, pch = 19)
curve(a_2 * exp(-b_2 * x), add = TRUE, col = col1, lwd = 4,  range(minx,maxx))

points(split_df$`3`$res_max_range_cluster, split_df$`3`$res_sum_D1, col = col2, pch = 19)
curve(a_3 * exp(-b_3 * x), add = TRUE, col = col2, lwd = 4,  range(minx,maxx))


legend("topright", legend = c("Clustered", "Conneceted", "Random"), 
       col = c("black", col1, col2), 
       pch = c(19, 19, 19), 
       lty = 1)






biggest_landscape_type <- c()

for(i in 1:1000){
  biggest_landscape_type[i] <- 0
  if(RES$res_agriculture_landscape_percent[i] >= 0.8){
    biggest_landscape_type[i] <- 1
  }
  if(RES$res_forest_landscape_percent[i] >= 0.8){
    biggest_landscape_type[i] <- 2
  }
  if(RES$res_urban_landscape_percent[i] >= 0.8){
    biggest_landscape_type[i] <- 3
  }
  
}

RES$biggest_landscape_type <- biggest_landscape_type
split_df_2 <- split(RES,RES$biggest_landscape_type)

no_pref <- mean(split_df_2$`0`$res_sum_D1)
ag_pref <- mean(split_df_2$`1`$res_sum_D1)
forest_pref <- mean(split_df_2$`2`$res_sum_D1)
urban_pref <- mean(split_df_2$`3`$res_sum_D1)

means <- c(no_pref,ag_pref,forest_pref,urban_pref)


sd0 <- sd(split_df_2$`0`$res_sum_D1)
sd1 <- sd(split_df_2$`1`$res_sum_D1)
sd2 <- sd(split_df_2$`2`$res_sum_D1)
sd3 <- sd(split_df_2$`3`$res_sum_D1)

sds <- c(sd0,sd1,sd2,sd3)


print(paste("no_pref: ",no_pref,  "; ag_pref: ",ag_pref, "; forest_pref: ",forest_pref,"; urban_pref: ", urban_pref))


barplot_heights <- barplot(means, 
                           ylim = c(0, max(means + sds) + 0.1),  # Adjust y-axis limit
                           col = "skyblue")

# Step 3: Add the values on top of the bars
text(x = barplot_heights, 
     y = means, 
     label = means, 
     pos = 3, 
     cex = 0.8, 
     col = "black")

# Step 4: Add error bars for standard deviation
arrows(x0 = barplot_heights, 
       y0 = means - sds, 
       x1 = barplot_heights, 
       y1 = means + sds, 
       angle = 90, 
       code = 3, 
       length = 0.05, 
       col = "red")

save.image("workspace_disp_matrix_LT_percentage_effect_appendix.RData")
