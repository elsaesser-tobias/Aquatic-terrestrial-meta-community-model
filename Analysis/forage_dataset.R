
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
setwd("C:/Users/elsae/Documents/R_Model")

A <- read.csv("FoRAGE_db_12_19_18_data_set.csv")


av_a <- c()
av_h <- c()
q <- 1
for(i in 1:length(A[,10])){
  if(A[i, 10] == "Insect"){
  av_a[q] <- A[i,38]
  av_h[q] <- A[i,41]
  q <- q + 1
  }
}

a <- median(av_a)
h <- median(av_h)
