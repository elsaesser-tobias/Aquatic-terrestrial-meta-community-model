library(ggplot2);
library(deSolve);
library(simecol);
library(OCNet);




#clear enviroment
rm(list = ls())




for(s in 1:4){
  


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
n_ODE <- 100
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
    D_1 <- t(D_1)
    D_2 <- t(D_2)
    

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
                    prey1_growth_speed = P_prey1_growth_speed, 
                    prey2_growth_speed = P_prey2_growth_speed, 
                    capacity_prey1 = P_capacity_prey1,
                    capacity_prey2 = P_capacity_prey2,
                    n_loc = length(habitatsx))
    
    #do all abundances:
    
    if(s == 1){
      #abundances with disp and hq
    }
    if(s == 2){
      #abdunaces with only hq
      D_1[,] <- 0
      D_2[,] <- 0
      
      diag(D_1) <- - dispersal_percent_1
      diag(D_2) <- - dispersal_percent_2
    }
    if(s == 3){
      #abundances with only disp
      habitat_quality <- rep(1,21)
    }
      #abundances without disp and hq
    if(s == 4){
      habitat_quality <- rep(1,21)
      D_1[,] <- 0
      D_2[,] <- 0
      
      diag(D_1) <- - dispersal_percent_1
      diag(D_2) <- - dispersal_percent_2
    }
    
    
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


#check where predator coexsistance is theoretically possible
#also assign which pred is aq and which is ter

R1_1 <- (res_mPred1) / (res_modPred1 * res_aPred1_1 - res_mPred1 * res_aPred1_1 * res_hPred1)
R1_2 <- (res_mPred1) / (res_modPred1 * res_aPred1_2 - res_mPred1 * res_aPred1_2 * res_hPred1)
R2_1 <- (res_mPred2) / (res_modPred2 * res_aPred2_1 - res_mPred2 * res_aPred2_1 * res_hPred2)
R2_2 <- (res_mPred2) / (res_modPred2 * res_aPred2_2 - res_mPred2 * res_aPred2_2 * res_hPred2)

Pred1_ter_or_aq <- c()
Coex_possible <- c()
for(i in 1:length(R1_1)){
  if(R1_1[i] <= R2_1[i]) Pred1_ter_or_aq[i] <- "Aq"
  
  else Pred1_ter_or_aq[i] <- "Ter"
  
  if(R1_1[i] > R2_1[i] & R1_2[i] < R2_2[i] | R1_1[i] < R2_1[i] & R1_2[i] > R2_2[i] ) Coex_possible [i] <- "Coex"
  
  else Coex_possible [i] <- "NoCoex"
}

#plot local
for(j in 1:4){
  if(j == 1) string <- "Pred1"
  if(j == 2) string <- "Pred2"
  if(j == 3) string <- "Prey1"
  if(j == 4) string <- "Prey2"
  
  for(i in 1:21){
    print(j * 21 + i - 20)
    RES[[paste(string, "Hab",i, sep = "")]] <- endpoints[,j * 21 + i - 20]
    RES[[paste("Hq_Hab",i, sep = "")]] <- hq_matrix[,j]
  }
  
}

RES$Pred1_ter_or_aq <- Pred1_ter_or_aq
RES$Coex_possible <- Coex_possible


a <- 0.5

split_df3 <- split(RES,RES$Coex_possible)
split_df2 <- split(split_df3$Coex, split_df3$Coex$Pred1_ter_or_aq)





#use gliding mean instead
gliding_mean <- function(x, y, window_size) {
  # Sort data by x-values
  sorted_data <- data.frame(x = x, y = y)
  
  sorted_data <- sorted_data[order(sorted_data$x), ]
  
  
  x <- x[1:round(length(x), - log10(window_size))] 
  y <- y[1:round(length(y), - log10(window_size))] 
  
  x_gliding_mean <- c()
  y_gliding_mean <- c()
  for(i in 1:(length(sorted_data$x) /window_size)){

    x_gliding_mean[length(x_gliding_mean) + 1] <- mean(sorted_data$x[(i*window_size - window_size + 1):(i*window_size)])
    y_gliding_mean[length(y_gliding_mean) + 1] <- mean(sorted_data$y[(i*window_size - window_size + 1):(i*window_size)])
  }
  
  return_df <- data.frame(x = x_gliding_mean, y = y_gliding_mean)
  return(return_df)
}







prey_aq_local <- c()
prey_ter_local <- c()
pred_aq_local <- c()
pred_ter_local <- c()
hq_local <- c()
for(i in 1:21){
  pred_aq_local <- c(pred_aq_local, split_df2$Ter[[paste("Pred2Hab", i, sep = "")]])
  pred_aq_local <- c(pred_aq_local, split_df2$Aq[[paste("Pred1Hab", i, sep = "")]])
  
  pred_ter_local <- c(pred_ter_local, split_df2$Ter[[paste("Pred1Hab", i, sep = "")]])
  pred_ter_local <- c(pred_ter_local, split_df2$Aq[[paste("Pred2Hab", i, sep = "")]])
  
  prey_aq_local <- c(prey_aq_local, split_df2$Ter[[paste("Prey1Hab", i, sep = "")]])
  prey_aq_local <- c(prey_aq_local, split_df2$Aq[[paste("Prey1Hab", i, sep = "")]])
  
  prey_ter_local <- c(prey_ter_local, split_df2$Ter[[paste("Prey2Hab", i, sep = "")]])
  prey_ter_local <- c(prey_ter_local, split_df2$Aq[[paste("Prey2Hab", i, sep = "")]])
  
  hq_local <- c(hq_local, split_df2$Ter[[paste("Hq_Hab", i, sep = "")]])
  hq_local <- c(hq_local, split_df2$Aq[[paste("Hq_Hab", i, sep = "")]])
  
}

#sort predators between aquatic and terrestrial preference
aq_pred_sorted <- c()
ter_pred_sorted <- c()
for(i in 1:length(split_df3$Coex$Pred1_ter_or_aq)){
  if(split_df3$Coex$Pred1_ter_or_aq[i] == "Aq"){
    aq_pred_sorted[i] <- split_df3$Coex$res_sum_pred1[i]
    ter_pred_sorted[i] <- split_df3$Coex$res_sum_pred2[i]
  }
  if(split_df3$Coex$Pred1_ter_or_aq[i] == "Ter"){
    aq_pred_sorted[i] <- split_df3$Coex$res_sum_pred2[i]
    ter_pred_sorted[i] <- split_df3$Coex$res_sum_pred1[i]
  }
}
split_df3$Coex$aq_pred_sorted <- aq_pred_sorted / 21
split_df3$Coex$ter_pred_sorted <- ter_pred_sorted / 21

split_df3$Coex$res_sum_prey1 <- split_df3$Coex$res_sum_prey1 / 21
split_df3$Coex$res_sum_prey2 <- split_df3$Coex$res_sum_prey2 /21




aq_pred <- split_df3$Coex$aq_pred_sorted
ter_pred <- split_df3$Coex$ter_pred_sorted
aq_prey <- split_df3$Coex$res_sum_prey1
ter_prey <- split_df3$Coex$res_sum_prey2
hq <- split_df3$Coex$res_mean_habitat_quality





save.image(paste("Abundances_",s,".RData"))

}



