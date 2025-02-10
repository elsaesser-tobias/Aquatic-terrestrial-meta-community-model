
library(OCNet);
library(pls);
library(mvtnorm);
library(MASS);
library(dplyr);

#clear enviroment
rm(list = ls())
#setwd("C:/Users/elsae/Desktop/Server")
start_time <- Sys.time()
#b will later be used to set different random seeds in each loop
b<- as.numeric(read.table("numbers.txt"))


#track time

#set random seed for reproducibility
set.seed(b)

#set landscape distribution
x <- abs(mvrnorm(1, c(0,0,0), diag(c(1,1,1))))
x <- x/sum(x)

urban_landscape_percent <- x[1]
forest_landscape_percent <- x[2]
agriculture_landscape_percent <- x[3]


#in step one the OCN will be generated, habitats and migration routes will be calclulated
#the second step will then be the ODE model of the metacommunity

#size of the OCN will be OCN_size^2, cellsz determines the size of a single cell of the OCN matrix in meters
OCN_size <- 100
cellsz <- 100
#percentage of cells of the OCN that will later be counted as stream, the rest is considered drainage area
percentage_of_stream_points <- 0.09
number_of_stream_points <- (OCN_size * OCN_size) * percentage_of_stream_points

#percentage of the stream points that get used as as habitat
#percentage_habitat_points <- 0.02
nhabitats <- 21

# type 1 = random element, type 2 = random cluster, type 3 = random
landscape_type <- sample(c(1,2,3), 1)


#set dispersal parameters
hab_radius <- 500
steps <- 20000

steps <- sample((steps*0.5):(steps*1.5),2)
n_steps <- max(steps)
n_steps_2 <- min(steps)

n_dragonflies <- 1000
#range von der diagonale von 8000
max_range_cluster <- sample(seq(1500,11280,1),1)
#percentages for direction taken
stream_prob = 1
ag_prob = 0.005
forest_prob = 0.00017
urban_prob = 0.0000032


#create the optimal channel network -------------------------------------------------------
OCN <- create_OCN(OCN_size, OCN_size, outletPos = OCN_size / 2, cellsize = cellsz)
OCN <- landscape_OCN(OCN, zMin = 50000)
OCN2 <- OCN

#calculate thresholds
threshold <- find_area_threshold_OCN(OCN)

#find threshold_area so that the number of stream points is slightly less than number_of_stream_points
threshold_area <- 0
for(i in 1:length(threshold$nNodesRN)){
  if(threshold$nNodesRN[i] < number_of_stream_points){
    threshold_area <- threshold$thrValues[i]
    break
  }
}

#aggregate OCN to reduce stream cells, use river network (RN) information from aggregated OCN
OCN <- aggregate_OCN(landscape_OCN(OCN, zMin = 50000), thrA = threshold_area)



#problem: no stream order for each RN node
#get AG-stream order to RN where available
RNSO <- c()

for(i in 1:length(OCN$AG$toRN)){
  RNSO[OCN$AG$toRN[i]] <- OCN$AG$streamOrder[i]
}
#RNSO[which(OCN$RN$downNode == 0)] <- max(RNSO)
#fill the rest through downstream node
#not well optimized but works

#the code takes all the places where the above node is available, but not the node itself
#the streamorder of the node itself is then filled with the one above
#this works because at AG level all changes in stream order are accounted for
for(j in 1:length(OCN$RN$downNode)){
  for(i in 1:length(OCN$RN$downNode)){
    if(is.na(RNSO[i]) == FALSE && is.na(RNSO[OCN$RN$downNode[i]]) && OCN$RN$downNode[i] != 0){
      RNSO[OCN$RN$downNode[i]] <- RNSO[i]
    }
  }
}




RNX <- OCN$RN$X
RNY <- OCN$RN$Y
RNDN <- OCN$RN$downNode


#add habitats ----------------------------------------------------------
#nhabitats <- round(length(RNX) * percentage_habitat_points)

euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}


habitat_points <- c()

for(a in 1:1000){
  primer_habitats <- c()
  
  # select primer habitats
  for(i in 1:1000){
    prime_x <- which(RNX > 1000 & RNX  < 9000)
    prime_x_and_y <- which(RNY[prime_x] > 1000 & RNY[prime_x]  < 9000)
    
    primer_habitats <- sample(prime_x_and_y, (nhabitats / 7))
    dist_12 <- euclidean_distance(RNX[primer_habitats][1], RNY[primer_habitats][1], 
                                      RNX[primer_habitats][2], RNY[primer_habitats][2])
    dist_23 <- euclidean_distance(RNX[primer_habitats][2], RNY[primer_habitats][2], 
                                      RNX[primer_habitats][3], RNY[primer_habitats][3])
    dist_13 <- euclidean_distance(RNX[primer_habitats][3], RNY[primer_habitats][3], 
                                      RNX[primer_habitats][1], RNY[primer_habitats][1])
    
    if(i == 1000){stop("No habitats found that satisfied conditions!")}
    
    if(min(c(dist_12, dist_13, dist_23)) > 2*hab_radius) break
    
  }
  
 
  habitat_points <- c()
  # find all habitat points within primer regions
  for(i in 1:length(primer_habitats)){
    x <- RNX[primer_habitats][i]
    y <- RNY[primer_habitats][i]
    possible_habitats <- c()
    
    # for each prime habitat select which other habitats are within range
    for(j in 1:length(RNX)){
      distance <- euclidean_distance(x,y,RNX[j],  RNY[j])
     if(distance < max_range_cluster & distance > hab_radius){
       possible_habitats[length(possible_habitats) + 1] <- j
     }
    }
    
    # not enough other habitats in range
    if(length(possible_habitats) < 6){break}
    
    # randomly select 6 habitats, hope that they have pairwise distance of at least 500
    for(j in 1: 10000){
      sampled <- sample(possible_habitats, 6)
      distances <- c()
        for(k in 1:length(sampled)){
          x_1 <- RNX[sampled[k]]
          y_1 <- RNY[sampled[k]]
          for(l in 1:length(sampled)){
            distances[length(distances) + 1] <- euclidean_distance(x_1,y_1, RNX[sampled[l]], RNY[sampled[l]])
          }
        }
      distances[distances == 0] <- hab_radius
      if(any(distances < hab_radius)){next}
      else{
        habitat_points <- c(habitat_points, sampled)
  
  
        break
      }
      
      if(j == 9999){
        stop("No habitats found that satisfied conditions 2!")
      }
    } 
      
      
  
    
    
  }

  habitat_points <- c(habitat_points, primer_habitats)
  
  if (length(habitat_points)!=nhabitats) next
  
  # check whether different habitats from different primer regions also have sufficient distance
  dis <- c()
  for(k in 1:length(habitat_points)){
    x_2 <- RNX[habitat_points[k]]
    y_2 <- RNY[habitat_points[k]]
    for(l in 1:length(habitat_points)){
      dis[length(dis) + 1] <- euclidean_distance(x_2,y_2, RNX[habitat_points[l]], RNY[habitat_points[l]])
    }
  }
  # change habitat distance of habitat to itself
  dis[dis == 0] <- hab_radius + 1

  if(all(dis > hab_radius)){break}
  if(a == 1000){stop("No habitats found that satisfied conditions 3!")}
}

habitatsx <- RNX[habitat_points]
habitatsy <- RNY[habitat_points]


# png(file=paste("habs",b,".png", sep =""), width=1000, height=1000, bg = "white")
 #plot(1:10000,1:10000, type = 'n')
 #points(habitatsx, habitatsy)
 #points(RNX[primer_habitats], RNY[primer_habitats], col ="blue")
# dev.off() 






#export relevant data to files, so that they can be read by the python script -------------------------------
write.table(RNX, "x_points.txt", row.names = FALSE, col.names = FALSE)
write.table(RNY, "y_points.txt", row.names = FALSE, col.names = FALSE)
write.table(RNSO, "SO.txt", row.names = FALSE, col.names = FALSE)
write.table(RNDN, "DN.txt", row.names = FALSE, col.names = FALSE)
write.table(b, "py_seed.txt", row.names = FALSE, col.names = FALSE)
write.table(cellsz, "py_cellsz.txt", row.names = FALSE, col.names = FALSE)
write.table(habitatsx, "habitatsx.txt", row.names = FALSE, col.names = FALSE)
write.table(habitatsy, "habitatsy.txt", row.names = FALSE, col.names = FALSE)

write.table(urban_landscape_percent, "urban_landscape_percent.txt", row.names = FALSE, col.names = FALSE)
write.table(forest_landscape_percent, "forest_landscape_percent.txt", row.names = FALSE, col.names = FALSE)
write.table(agriculture_landscape_percent, "agriculture_landscape_percent.txt", row.names = FALSE, col.names = FALSE)

write.table(stream_prob, "stream_prob.txt", row.names = FALSE, col.names = FALSE)
write.table(ag_prob, "ag_prob.txt", row.names = FALSE, col.names = FALSE)
write.table(forest_prob, "forest_prob.txt", row.names = FALSE, col.names = FALSE)
write.table(urban_prob, "urban_prob.txt", row.names = FALSE, col.names = FALSE)

write.table(landscape_type, "landscape_type.txt", row.names = FALSE, col.names = FALSE)

save.image("workspace.RData")