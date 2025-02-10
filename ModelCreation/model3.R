
library(OCNet);
library(pls);
library(mvtnorm);
library(MASS);
library(dplyr);


#clear enviroment
rm(list = ls())
#setwd("C:/Users/elsae/Desktop/Server")

#read data creeated in previous R and python files
load("workspace.RData")

set.seed(b + 1000000)

nlmElement <- read.table("nlmElement.csv", header = FALSE, sep =",")
nlmElement <- as.numeric(unlist(nlmElement[sapply(nlmElement, is.numeric)]))
nlmElement <- matrix(nlmElement, nrow = 10000, ncol = 10000)


#---------------------------------------------------------------------------------------------------------------#
#create dispersal paths and dispersal matrix D


create_paths <- function(start_point_x, start_point_y, n_paths, distance, distance_2, hab_x, hab_y, rad){
  n_match <- rep(0, length(habitatsx))
  n_match_2 <- rep(0, length(habitatsx))
  for(i in 1:n_paths){
    
    
    route_x <- c(start_point_x)
    route_y <- c(start_point_y)
    route_paste <- c()
    route_paste[1] <- paste("x",start_point_x,"y",start_point_y, sep = "")
    route_dir <- c()
    route_which <- 2
    
    current_x <- start_point_x
    current_y <- start_point_y
    dir <- 0
    for(j in 1:distance){
      #check if close to edge, if yes, abandon path
      if(current_x == 1 | current_x == nrow(nlmElement) | 
         current_y == 1 | current_y == nrow(nlmElement)){
        break
      }
      
      
      #get surrounding
      around <- c()
    
      around [1] <- nlmElement[current_x - 1,current_y - 1] 
      around [2] <- nlmElement[current_x,current_y - 1]
      around [3] <- nlmElement[current_x + 1,current_y - 1]
      around [4] <- nlmElement[current_x + 1,current_y]
      around [5] <- nlmElement[current_x + 1,current_y + 1]
      around [6] <- nlmElement[current_x,current_y + 1]
      around [7] <- nlmElement[current_x - 1,current_y + 1]
      around [8] <- nlmElement[current_x - 1,current_y]
      possible_dir <- c()
      if(dir == 1){
        possible_dir <- c(7,8,1,2,3)
      } else if(dir == 2){
        possible_dir <- c(8,1,2,3,4)
      } else if(dir == 3){
        possible_dir <- c(1,2,3,4,5)
      } else if(dir == 4){
        possible_dir <- c(2,3,4,5,6)
      } else if(dir == 5){
        possible_dir <- c(3,4,5,6,7)
      } else if(dir == 6){
        possible_dir <- c(4,5,6,7,8)
      } else if(dir == 7){
        possible_dir <- c(5,6,7,8,1)
      } else if(dir == 8){
        possible_dir <- c(6,7,8,1,2)
      } else {
        possible_dir <- c(1,2,3,4,5,6,7,8)
      }
      
      #set multiplier for direction
      multiplier <- c()
      multiplier <- rep(0.01 , length(possible_dir))
      
      multiplier[which(possible_dir == dir)] <- 0.9
      multiplier[which(possible_dir == dir) + 1] <- 0.04
      multiplier[which(possible_dir == dir) - 1] <- 0.04
      
      around[possible_dir] <- around[possible_dir] * multiplier
      

      dir <- sample(possible_dir, size = 1, prob = around[possible_dir])
      
      if(dir == 1){
        current_x <- current_x - 1
        current_y <- current_y -1
      } else if(dir == 2){
        current_x <- current_x
        current_y <- current_y -1
      } else if(dir == 3){
        current_x <- current_x + 1
        current_y <- current_y -1
      } else if(dir == 4){
        current_x <- current_x + 1
        current_y <- current_y
      } else if(dir == 5){
        current_x <- current_x + 1
        current_y <- current_y + 1
      } else if(dir == 6){
        current_x <- current_x
        current_y <- current_y + 1
      } else if(dir == 7){
        current_x <- current_x - 1
        current_y <- current_y + 1
      } else if(dir == 8){
        current_x <- current_x - 1
        current_y <- current_y
      }
      
      route_x[route_which] <- current_x
      route_y[route_which] <- current_y
      route_dir[route_which - 1] <- dir
      route_which <- route_which + 1  
      
    }
    
    # repeat vector intro matrix
    matrix_x <- matrix(route_x, nrow = length(route_x), ncol = length(hab_x), byrow = FALSE)
    matrix_y <- matrix(route_y, nrow = length(route_y), ncol = length(hab_y), byrow = FALSE)
    matrix_habx <- matrix(hab_x, nrow = length(route_x), ncol = length(hab_x), byrow = TRUE)
    matrix_haby <- matrix(hab_y, nrow = length(route_y), ncol = length(hab_y), byrow = TRUE)
    
    
    # check when ecach species arrived
    arrived <- rep(NA,length(hab_x))
    dist <- sqrt(((matrix_x - matrix_habx)^2) + ((matrix_y - matrix_haby)^2)) <rad
    for(q in 1:length(hab_x)){
      if(any(dist[,q])){
        arrived[q] <- which.max(dist[,q])
      }
    }
    
    
    arrived_2 <- arrived
    arrived_2[arrived_2 > distance_2] <- NA
    
    
    n_match[which.min(arrived)] <- n_match[which.min(arrived)] + 1
    n_match_2[which.min(arrived_2)] <- n_match_2[which.min(arrived_2)] + 1

  }
  return(list(n_match = n_match, n_match_2 = n_match_2))
}

D_1 <- matrix(NA, nrow = length(habitatsx), ncol = length(habitatsx))
D_2 <- matrix(NA, nrow = length(habitatsx), ncol = length(habitatsx))

for(i in 1: length(habitatsx)){
  matches <- c()
  new_habitatsx <- habitatsx
  new_habitatsx[i] <- 100000
  new_habitatsy <- habitatsy
  new_habitatsy[i] <- 100000
  matches <- create_paths(habitatsx[i],habitatsy[i], n_dragonflies, n_steps,n_steps_2, 
                          new_habitatsx, new_habitatsy, hab_radius)
  D_1[i,] <- matches$n_match
  D_2[i,] <- matches$n_match_2
  

}
#changed to make dispersal percent in model 4 to have more varity
#



n_habitat_connections <- c()
for(i in 1:length(D_1[1,])){
  n_habitat_connections[i] <- sum(D_1[i,] > 0)
}
#--------------------------------------------------------------------------------------------------------------#
#calculate habitat quality
#create catchment areas from matrix
threshold_area2 <- 0
OCN2 <- aggregate_OCN(landscape_OCN(OCN2, zMin = 50000), thrA = threshold_area2)

FD_habitat_points <- OCN$RN$toFD[habitat_points]
catchment_areas <- OCN2$RN$upstream[FD_habitat_points]

OCN$RN$X[habitat_points]
OCN2$RN$X[FD_habitat_points]

stream_amount <- numeric(length(habitat_points))
agri_amount <- numeric(length(habitat_points))
total_amount <- numeric(length(habitat_points))
a = 1
cell <- cellsz /2
for(i in catchment_areas){
  for(j in i){
    x <- OCN2$RN$X[j]
    y <- OCN2$RN$X[j]
    stream_amount[a] <- sum(nlmElement[(x-cell+1):(x+cell), (y-cell+1):(y+cell)] == stream_prob) + stream_amount[a]
    agri_amount[a] <- sum(nlmElement[(x-cell+1):(x+cell), (y-cell+1):(y+cell)] == ag_prob) + agri_amount[a]
    total_amount[a] <- sum(nlmElement[(x-cell+1):(x+cell), (y-cell+1):(y+cell)] > 0) + total_amount[a]
  }
  a = a + 1
}


habitat_quality <- 1 - (agri_amount / (total_amount - stream_amount)) * 0.75

#rm(nlmElement)
save.image(paste("workspace_end",b, ".RData", sep = ""))
