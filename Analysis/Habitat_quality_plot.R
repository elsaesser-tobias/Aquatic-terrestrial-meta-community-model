#load results from no dispersal simulation and clear enviroment from it after
library(OCNet)

load("Abundances_without_dispersal_diag_2.RData")

no_disp_aq_pred <- split_df3$Coex$aq_pred_sorted
no_disp_ter_pred <- split_df3$Coex$ter_pred_sorted
no_disp_aq_prey <- split_df3$Coex$res_sum_prey1
no_disp_ter_prey <- split_df3$Coex$res_sum_prey2
hq_regio <- split_df3$Coex$res_mean_habitat_quality



no_disp_prey_aq_local <- prey_aq_local
no_disp_prey_ter_local <- prey_ter_local
no_disp_pred_aq_local <- pred_aq_local
no_disp_pred_ter_local  <- pred_ter_local
hq_local_2 <- hq_local

keep_vars <- c("no_disp_aq_pred", "no_disp_ter_pred","no_disp_aq_prey", "no_disp_ter_prey",
               "no_disp_prey_aq_local", "no_disp_prey_ter_local", "no_disp_pred_aq_local", "no_disp_pred_ter_local",
               "hq_regio")
rm(list = setdiff(ls(), keep_vars))

# load results from no dispersal simulation and clear enviroment from it after
load("Abundances_with_dispersal_T_2.RData")

aq_pred <- split_df3$Coex$aq_pred_sorted
ter_pred <- split_df3$Coex$ter_pred_sorted
aq_prey <- split_df3$Coex$res_sum_prey1
ter_prey <- split_df3$Coex$res_sum_prey2



keep_vars <- c("aq_pred", "ter_pred","aq_prey", "ter_prey",
               "prey_aq_local", "prey_ter_local", "pred_aq_local", "pred_ter_local")

col1 <- "red"
col2 <- "black"
col3 <-  rgb(255, 127, 14, alpha = 255, maxColorValue = 255)

col4 <- rgb(56, 143, 203, alpha = 255, maxColorValue = 255)
col5 <-  rgb(255, 153, 50, alpha = 255, maxColorValue = 255)

# there are 21 times as many local data. To show the same amount of data random habitats will be sampled (just for visualisation)
set.seed(1)
which_local <- sample(1:length(pred_aq_local), length(aq_pred))





# do gliding mean but check for different window size, put figure into appendix




  # Adjust 'mar' as needed


#### pred aq



w <- 5050
rs <- c()
slopes <- c()

#aq pred
gl_mean_local <- gliding_mean(hq_local, pred_aq_local, w)
model <- lm(gl_mean_local$y ~ gl_mean_local$x)
slope <- coef(model)[2]
intercept <- coef(model)[1]
x_min <- min(gl_mean_local$x) - min(gl_mean_local$x) * 0.05
x_max <- max(gl_mean_local$x) + max(gl_mean_local$x) * 0.01
y_min <- min(gl_mean_local$y) - min(gl_mean_local$y) * 0.1
y_max <- max(gl_mean_local$y) + max(gl_mean_local$y) * 0.05
plot(gl_mean_local$x, gl_mean_local$y, col = col2, pch = 16, cex = 1.25,
     main = "Predator AP", xlim = c(x_min,x_max), ylim = c(y_min,y_max),
     xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n", cex.main = 1.8)
axis(side = 2, at = c(10, 30, 50), cex.axis = 1.2)
axis(side = 1, at = c(0.4, 0.7, 1), cex.axis = 1.2)
abline(intercept, slope, col = col1 , lwd = 5)

#add legend
legend_text <- paste("R² =", round(summary(model)$r.squared, 3),
                     "\n Slope =", round(slope /mean(gl_mean_local$y),3))
m <- max(gl_mean_local$x)
x_pos <- m + m * 0.01
y_pos <- 9

text_width <- strwidth(legend_text)
text_height <- strheight(legend_text)
x_right <- par("usr")[2]
x_left <- x_pos - text_width * 1.35
y_bottom <- par("usr")[3]
y_top <- y_bottom + text_height * 1.55

rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top, col = "white", border = "black")
text(x = x_pos, y = y_pos, labels = legend_text, pos = 2, col = col2, cex = 1.25)



slopes[length(slopes) + 1] <- slope /mean(gl_mean_local$y)
rs[length(rs) + 1] <- summary(model)$r.squared

#ter pred
gl_mean_local <- gliding_mean(hq_local, pred_ter_local, w)
model <- lm(gl_mean_local$y ~ gl_mean_local$x)
slope <- coef(model)[2]
intercept <- coef(model)[1]
x_min <- min(gl_mean_local$x) - min(gl_mean_local$x) * 0.05
x_max <- max(gl_mean_local$x) + max(gl_mean_local$x) * 0.01
y_min <- min(gl_mean_local$y) - min(gl_mean_local$y) * 0.1
y_max <- max(gl_mean_local$y) + max(gl_mean_local$y) * 0.05
plot(gl_mean_local$x, gl_mean_local$y, col = col2, pch = 16, cex = 1.25,
     main = "Predator TP", xlim = c(x_min,x_max), ylim = c(y_min,y_max),
     xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n", cex.main = 1.8)
axis(side = 2, at = c(40, 60, 80), cex.axis = 1.2)
axis(side = 1, at = c(0.4, 0.7, 1), cex.axis = 1.2)
abline(intercept, slope, col = col1 , lwd = 5)

#add legend
legend_text <- paste("R² =", round(summary(model)$r.squared, 3),
                     "\n Slope =", round(slope / mean (gl_mean_local$y),3))
m <- min(gl_mean_local$x)
x_pos <- m + m * 0.5
y_pos <- 36.25

text_width <- strwidth(legend_text)
text_height <- strheight(legend_text)
x_right <- x_pos + text_width * 0.001
x_left <-  par("usr")[1]
y_bottom <- par("usr")[3]
y_top <- y_bottom + text_height * 1.55

rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top, col = "white", border = "black")
text(x = x_pos, y = y_pos, labels = legend_text, pos = 2, col = col2, cex = 1.25)


slopes[length(slopes) + 1] <- slope /mean(gl_mean_local$y)
rs[length(rs) + 1] <- summary(model)$r.squared


#aq prey
gl_mean_local <- gliding_mean(hq_local, prey_aq_local, w)
model <- lm(gl_mean_local$y ~ gl_mean_local$x)
slope <- coef(model)[2]
intercept <- coef(model)[1]
x_min <- min(gl_mean_local$x) - min(gl_mean_local$x) * 0.05
x_max <- max(gl_mean_local$x) + max(gl_mean_local$x) * 0.01
y_min <- min(gl_mean_local$y) - min(gl_mean_local$y) * 0.1
y_max <- max(gl_mean_local$y) + max(gl_mean_local$y) * 0.05
plot(gl_mean_local$x, gl_mean_local$y, col = col2, pch = 16, cex = 1.25,
     main = "Aquatic Prey", xlim = c(x_min,x_max), ylim = c(y_min,y_max),
     xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n", cex.main = 1.8)
axis(side = 2, at = c(200, 300, 400), cex.axis = 1.2)
axis(side = 1, at = c(0.4, 0.7, 1), cex.axis = 1.2)
abline(intercept, slope, col = col1 , lwd = 5)

#add legend
legend_text <- paste("R² =", round(summary(model)$r.squared, 3),
                     "\n Slope =", round(slope/mean(gl_mean_local$y),3))
m <- max(gl_mean_local$x)
x_pos <- m + m * 0.0125
y_pos <- 169

text_width <- strwidth(legend_text)
text_height <- strheight(legend_text)
x_right <- par("usr")[2]
x_left <- x_pos - text_width * 1.35
y_bottom <- par("usr")[3]
y_top <- y_bottom + text_height * 1.55

rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top, col = "white", border = "black")
text(x = x_pos, y = y_pos, labels = legend_text, pos = 2, col = col2, cex = 1.25)


slopes[length(slopes) + 1] <- slope /mean(gl_mean_local$y)
rs[length(rs) + 1] <- summary(model)$r.squared


#ter prey
gl_mean_local <- gliding_mean(hq_local, prey_ter_local, w)
model <- lm(gl_mean_local$y ~ gl_mean_local$x)
slope <- coef(model)[2]
intercept <- coef(model)[1]
x_min <- min(gl_mean_local$x) - min(gl_mean_local$x) * 0.05
x_max <- max(gl_mean_local$x) + max(gl_mean_local$x) * 0.01
y_min <- min(gl_mean_local$y) - min(gl_mean_local$y) * 0.1
y_max <- max(gl_mean_local$y) + max(gl_mean_local$y) * 0.05
plot(gl_mean_local$x, gl_mean_local$y, col = col2, pch = 16, cex = 1.25,
     main = "Terrestrial Prey", xlim = c(x_min,x_max), ylim = c(y_min,y_max),
     xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n", cex.main = 1.8)
axis(side = 2, at = c(800, 1100, 1400), cex.axis = 1.2)
axis(side = 1, at = c(0.4, 0.7, 1), cex.axis = 1.2)
abline(intercept, slope, col = col1 , lwd = 5)

#add legend
legend_text <- paste("R² =", round(summary(model)$r.squared, 3),
                     "\n Slope =", round(slope/mean(gl_mean_local$y),3))
m <- min(gl_mean_local$x)
x_pos <- m + 0.535* m
y_pos <- 810

text_width <- strwidth(legend_text)
text_height <- strheight(legend_text)
x_right <- x_pos
x_left <- par("usr")[1]
y_bottom <- par("usr")[3]
y_top <- y_bottom + text_height * 1.55

rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top, col = "white", border = "black")
text(x = x_pos, y = y_pos, labels = legend_text, pos = 2, col = col2, cex = 1.25)

slopes[length(slopes) + 1] <- slope /mean(gl_mean_local$y)
rs[length(rs) + 1] <- summary(model)$r.squared


#
 mtext("Habitat quality", side = 1, outer = TRUE, line = 2, cex = 2)
 mtext("Abundance", side = 2, outer = TRUE, line = 2, cex = 2)


#######################
#########################
#################################
######################################
############################################


# 
# r_sq <- c()
# slopes <- c()
# 
# #aq pred
# x <- no_disp_pred_aq_local
# y <- pred_aq_local
# w_1 <- which(x < 2)
# w_2 <- which(y < 2)
# w <- c(w_1,w_2)
# w <- setdiff(1:length(x), w)
# model <- lm(y[w] ~ x[w])
# slope <- coef(model)[2]
# r_sq[length(r_sq) + 1] <- summary(model)$r.squared
# slopes[length(slopes) + 1] <- slope
# 
# x <- no_disp_aq_pred
# y <- aq_pred
# w_1 <- which(x < 2)
# w_2 <- which(y < 2)
# w <- c(w_1,w_2)
# w <- setdiff(1:length(x), w)
# model <- lm(y[w] ~ x[w])
# slope <- coef(model)[2]
# r_sq[length(r_sq) + 1] <- summary(model)$r.squared
# slopes[length(slopes) + 1] <- slope
# 
# 
# 
# 
# 
# #ter pred
# x <- no_disp_pred_ter_local
# y <- pred_ter_local
# w_1 <- which(x < 2)
# w_2 <- which(y < 2)
# w <- c(w_1,w_2)
# w <- setdiff(1:length(x), w)
# model <- lm(y[w] ~ x[w])
# slope <- coef(model)[2]
# r_sq[length(r_sq) + 1] <- summary(model)$r.squared
# slopes[length(slopes) + 1] <- slope
# 
# x <- no_disp_ter_pred
# y <- ter_pred
# w_1 <- which(x < 2)
# w_2 <- which(y < 2)
# w <- c(w_1,w_2)
# w <- setdiff(1:length(x), w)
# model <- lm(y[w] ~ x[w])
# slope <- coef(model)[2]
# r_sq[length(r_sq) + 1] <- summary(model)$r.squared
# slopes[length(slopes) + 1] <- slope
# 
# 
# 
# #aq prey
# x <- no_disp_prey_aq_local
# y <- prey_aq_local
# w_1 <- which(x < 2)
# w_2 <- which(y < 2)
# w <- c(w_1,w_2)
# w <- setdiff(1:length(x), w)
# model <- lm(y[w] ~ x[w])
# slope <- coef(model)[2]
# r_sq[length(r_sq) + 1] <- summary(model)$r.squared
# slopes[length(slopes) + 1] <- slope
# 
# x <- no_disp_aq_prey
# y <- aq_prey
# w_1 <- which(x < 2)
# w_2 <- which(y < 2)
# w <- c(w_1,w_2)
# w <- setdiff(1:length(x), w)
# model <- lm(y[w] ~ x[w])
# slope <- coef(model)[2]
# r_sq[length(r_sq) + 1] <- summary(model)$r.squared
# slopes[length(slopes) + 1] <- slope
# 
# 
# 
# #ter prey
# x <- no_disp_prey_ter_local
# y <- prey_ter_local
# w_1 <- which(x < 2)
# w_2 <- which(y < 2)
# w <- c(w_1,w_2)
# w <- setdiff(1:length(x), w)
# model <- lm(y[w] ~ x[w])
# slope <- coef(model)[2]
# r_sq[length(r_sq) + 1] <- summary(model)$r.squared
# slopes[length(slopes) + 1] <- slope
# 
# x <- no_disp_ter_prey
# y <- ter_prey
# w_1 <- which(x < 2)
# w_2 <- which(y < 2)
# w <- c(w_1,w_2)
# w <- setdiff(1:length(x), w)
# model <- lm(y[w] ~ x[w])
# slope <- coef(model)[2]
# r_sq[length(r_sq) + 1] <- summary(model)$r.squared
# slopes[length(slopes) + 1] <- slope
# 
# 
# 
# 
# r_sq[2] - r_sq[1]
# r_sq[4] - r_sq[3]
# r_sq[6] - r_sq[5]
# r_sq[8] - r_sq[7]
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# par(mfrow = c(2, 2), mar = c(2, 2, 2, 2), oma = c(5,5,0,0))
# 
# #aq pred
# x <- no_disp_pred_aq_local
# y <- pred_aq_local
# w_1 <- which(x < 2)
# w_2 <- which(y < 2)
# w <- c(w_1,w_2)
# w <- setdiff(1:length(x), w)
# model_local <- lm(y[w] ~ x[w])
# slope_local <- coef(model_local)[2]
# intercept_local <- coef(model_local)[1]
# 
# m <- no_disp_aq_pred
# n <- aq_pred
# o_1 <- which(x < 2)
# o_2 <- which(y < 2)
# o <- c(o_1,o_2)
# o <- setdiff(1:length(m), o)
# model <- lm(n[o] ~ m[o])
# slope <- coef(model)[2]
# intercept <- coef(model)[1]
# 
# 
# 
# plot(x[w], y[w], xlab = "",
#      main = "Predator AP", ylab = "", col = col2, pch = 16, cex.lab = 1.5, cex = 1,
#      ylim = c(0,max(y[w])* 1.02) , xlim = c(0,max(x[w])* 1.02),
#      yaxt = "n", xaxt = "n", cex.main = 1.8, xaxs = "i", yaxs = "i")
# points(m[o], n[o], col = col3, pch = 16)
# 
# axis(side = 2, at = c(0, 300, 600), cex.axis = 1.2)
# axis(side = 1, at = c(0, 100, 200), cex.axis = 1.2)
# 
# 
# abline(intercept_local, slope_local, col = col4, lwd = 5)
# abline(intercept, slope, col = col5, lwd = 5)
# 
# 
# 
# #ter pred
# x <- no_disp_pred_ter_local
# y <- pred_ter_local
# w_1 <- which(x < 2)
# w_2 <- which(y < 2)
# w <- c(w_1,w_2)
# w <- setdiff(1:length(x), w)
# model_local <- lm(y[w] ~ x[w])
# slope_local <- coef(model_local)[2]
# intercept_local <- coef(model_local)[1]
# 
# m <- no_disp_ter_pred
# n <- ter_pred
# o_1 <- which(x < 2)
# o_2 <- which(y < 2)
# o <- c(o_1,o_2)
# o <- setdiff(1:length(m), o)
# model <- lm(n[o] ~ m[o])
# slope <- coef(model)[2]
# intercept <- coef(model)[1]
# 
# 
# 
# plot(x[w], y[w], xlab = "",
#      main = "Predator TP", ylab = "", col = col2, pch = 16, cex.lab = 1.5, cex = 1,
#      ylim = c(0,max(y[w])* 1.02) , xlim = c(0,max(x[w])* 1.02),
#      yaxt = "n", xaxt = "n", cex.main = 1.8, xaxs = "i", yaxs = "i")
# points(m[o], n[o], col = col3, pch = 16)
# 
# axis(side = 2, at = c(0, 400, 800), cex.axis = 1.2)
# axis(side = 1, at = c(0, 150, 300), cex.axis = 1.2)
# 
# 
# abline(intercept_local, slope_local, col = col4, lwd = 5)
# abline(intercept, slope, col = col5, lwd = 5)
# 
# 
# 
# 
# 
# 
# #aq prey
# x <- no_disp_prey_aq_local
# y <- prey_aq_local
# w_1 <- which(x < 2)
# w_2 <- which(y < 2)
# w <- c(w_1,w_2)
# w <- setdiff(1:length(x), w)
# model_local <- lm(y[w] ~ x[w])
# slope_local <- coef(model_local)[2]
# intercept_local <- coef(model_local)[1]
# 
# m <- no_disp_aq_prey
# n <- aq_prey
# o_1 <- which(x < 2)
# o_2 <- which(y < 2)
# o <- c(o_1,o_2)
# o <- 1:length(m)
# model <- lm(n[o] ~ m[o])
# slope <- coef(model)[2]
# intercept <- coef(model)[1]
# 
# 
# 
# plot(x[w], y[w], xlab = "",
#      main = "Aquatic Prey", ylab = "", col = col2, pch = 16, cex.lab = 1.5, cex = 1, 
#      ylim = c(0,max(y[w])* 1.02) , xlim = c(0,max(x[w])* 1.02),
#      yaxt = "n", xaxt = "n", cex.main = 1.8, xaxs = "i", yaxs = "i")
# points(m[o], n[o], col = col3, pch = 16)
# 
# axis(side = 2, at = c(0, 1250, 2500), cex.axis = 1.2)
# axis(side = 1, at = c(0, 1500, 3000), cex.axis = 1.2)
# 
# 
# abline(intercept_local, slope_local, col = col4, lwd = 5)
# abline(intercept, slope, col = col5, lwd = 5)
# 
# 
# 
# 
# #ter prey
# x <- no_disp_prey_ter_local
# y <- prey_ter_local
# w_1 <- which(x < 2)
# w_2 <- which(y < 2)
# w <- c(w_1,w_2)
# w <- setdiff(1:length(x), w)
# model_local <- lm(y[w] ~ x[w])
# slope_local <- coef(model_local)[2]
# intercept_local <- coef(model_local)[1]
# 
# m <- no_disp_ter_prey
# n <- ter_prey
# o_1 <- which(x < 2)
# o_2 <- which(y < 2)
# o <- c(o_1,o_2)
# o <- 1:length(m)
# model <- lm(n[o] ~ m[o])
# slope <- coef(model)[2]
# intercept <- coef(model)[1]
# 
# 
# 
# plot(x[w], y[w], xlab = "",
#      main = "Terrestrial Prey", ylab = "", col = col2, pch = 16, cex.lab = 1.5, cex = 1, 
#      ylim = c(0,max(y[w]) * 1.02) , xlim = c(0,max(x[w])* 1.02),
#      yaxt = "n", xaxt = "n", cex.main = 1.8, xaxs = "i", yaxs = "i")
# points(m[o], n[o], col = col3, pch = 16)
# 
# axis(side = 2, at = c(0, 3000, 6000), cex.axis = 1.2)
# axis(side = 1, at = c(0, 3000, 6000), cex.axis = 1.2)
# 
# 
# abline(intercept_local, slope_local, col = col4, lwd = 5)
# abline(intercept, slope, col = col5, lwd = 5)
# 
# 
# mtext("Abundace without dispersal", side = 1, outer = TRUE, line = 2, cex = 1.8)
# mtext("Abundance with dispersal", side = 2, outer = TRUE, line = 2, cex = 1.8)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 
# # 
# # 
# # 
# # 
# # 
# # # bottom legend
# # m <- max(x)
# # x_pos <- m - m * 0.001
# # y_pos <- 0 + 45
# # legend_text <- paste("R² =", round(summary(model)$r.squared, 3),
# #                      "\n Slope =", round(slope,3))
# # 
# # 
# # # Calculate the dimensions of the text box
# # text_width <- strwidth(legend_text)
# # text_height <- strheight(legend_text)
# # 
# # x_right <- par("usr")[2]
# # x_left <- x_pos - text_width * 1.35
# # y_bottom <- par("usr")[3]
# # y_top <- y_bottom + text_height * 1.6
# # 
# # # Add a white rectangle as the background for the legend text
# # rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top, col = "white", border = "black")
# # text(x = x_pos, y = y_pos, labels = legend_text, pos = 2, col = col2, cex = 1.25)
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # #ter pred
# # x <- no_disp_pred_ter_local
# # y <- pred_ter_local
# # model <- lm(y ~ x)
# # # Extract slope and intercept from the model
# # slope <- coef(model)[2]
# # intercept <- coef(model)[1]
# # 
# # plot(x, y, xlab = "",
# #      main = "Predator TP", ylab = "", col = col2, pch = 16, cex.lab = 1.5, cex = 1, ylim = c(0,max(y))
# #      , xlim = c(0,max(x)),
# #      yaxt = "n", xaxt = "n", cex.main = 1.8, xaxs = "i", yaxs = "i")
# # axis(side = 2, at = c(0, 400, 800), cex.axis = 1.6)
# # axis(side = 1, at = c(0, 150, 300), cex.axis = 1.6)
# # 
# # 
# # abline(intercept, slope, col = col1, lwd = 5)
# # 
# # 
# # # bottom legend
# # m <- max(x)
# # x_pos <- m - m * 0.001
# # y_pos <- 0 + 45
# # legend_text <- paste("R² =", round(summary(model)$r.squared, 3),
# #                      "\n Slope =", round(slope,3))
# # 
# # 
# # # Calculate the dimensions of the text box
# # text_width <- strwidth(legend_text)
# # text_height <- strheight(legend_text)
# # 
# # x_right <- par("usr")[2]
# # x_left <- x_pos - text_width * 1.35
# # y_bottom <- par("usr")[3]
# # y_top <- y_bottom + text_height * 1.6
# # 
# # # Add a white rectangle as the background for the legend text
# # rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top, col = "white", border = "black")
# # text(x = x_pos, y = y_pos, labels = legend_text, pos = 2, col = col2, cex = 1.25)
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # #aq prey
# # x <- no_disp_prey_aq_local
# # y <- prey_aq_local
# # model <- lm(y ~ x)
# # # Extract slope and intercept from the model
# # slope <- coef(model)[2]
# # intercept <- coef(model)[1]
# # 
# # plot(x, y, xlab = "",
# #      main = "Aquatic Prey", ylab = "", col = col2, pch = 16, cex.lab = 1.5, cex = 1, ylim = c(0,max(y))
# #      , xlim = c(0,max(x)),
# #      yaxt = "n", xaxt = "n", cex.main = 1.8, xaxs = "i", yaxs = "i")
# # axis(side = 2, at = c(0, 750, 1500), cex.axis = 1.6)
# # axis(side = 1, at = c(0, 1000, 2000), cex.axis = 1.6)
# # 
# # 
# # abline(intercept, slope, col = col1, lwd = 5)
# # 
# # 
# # # bottom legend
# # m <- max(x)
# # x_pos <- m - m * 0.001
# # y_pos <- 0 + 200
# # legend_text <- paste("R² =", round(summary(model)$r.squared, 3),
# #                      "\n Slope =", round(slope,3))
# # 
# # 
# # # Calculate the dimensions of the text box
# # text_width <- strwidth(legend_text)
# # text_height <- strheight(legend_text)
# # 
# # x_right <- par("usr")[2]
# # x_left <- x_pos - text_width * 1.35
# # y_bottom <- par("usr")[3]
# # y_top <- y_bottom + text_height * 1.6
# # 
# # # Add a white rectangle as the background for the legend text
# # rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top, col = "white", border = "black")
# # text(x = x_pos, y = y_pos, labels = legend_text, pos = 2, col = col2, cex = 1.25)
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # #ter prey
# # x <- no_disp_prey_ter_local
# # y <- prey_ter_local
# # model <- lm(y ~ x)
# # # Extract slope and intercept from the model
# # slope <- coef(model)[2]
# # intercept <- coef(model)[1]
# # 
# # plot(x, y, xlab = "",
# #      main = "Terrestrial Prey", ylab = "", col = col2, pch = 16, cex.lab = 1.5, cex = 1, ylim = c(0,max(y))
# #      , xlim = c(0,max(x)),
# #      yaxt = "n", xaxt = "n", cex.main = 1.8, xaxs = "i", yaxs = "i")
# # axis(side = 2, at = c(0, 3000, 6000), cex.axis = 1.6)
# # axis(side = 1, at = c(0, 3000, 6000), cex.axis = 1.6)
# # 
# # 
# # abline(intercept, slope, col = col1, lwd = 5)
# # 
# # 
# # # bottom legend
# # m <- max(x)
# # x_pos <- m - m * 0.001
# # y_pos <- 0 + 500
# # legend_text <- paste("R² =", round(summary(model)$r.squared, 3),
# #                      "\n Slope =", round(slope,3))
# # 
# # 
# # # Calculate the dimensions of the text box
# # text_width <- strwidth(legend_text)
# # text_height <- strheight(legend_text)
# # 
# # x_right <- par("usr")[2]
# # x_left <- x_pos - text_width * 1.35
# # y_bottom <- par("usr")[3]
# # y_top <- y_bottom + text_height * 1.6
# # 
# # # Add a white rectangle as the background for the legend text
# # rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top, col = "white", border = "black")
# # text(x = x_pos, y = y_pos, labels = legend_text, pos = 2, col = col2, cex = 1.25)
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # # mtext("Abundace without dispersal", side = 1, outer = TRUE, line = 2, cex = 2.2)
# # # mtext("Abundance with dispersal", side = 2, outer = TRUE, line = 2, cex = 2.2)
# # # 
# # # ##########################################################
# # # ##### pred ter
# # # model <- lm(ter_pred ~ no_disp_ter_pred)
# # # # Extract slope and intercept from the model
# # # slope <- coef(model)[2]
# # # intercept <- coef(model)[1]
# # # 
# # # plot(no_disp_ter_pred, ter_pred, xlab = "",
# # #      main = "Predator TP", ylab = "", col = col1, pch = 16, cex.lab = 1.5, cex = 1, ylim = c(0,max(ter_pred)), xlim = c(0,max(no_disp_ter_pred)),
# # #      yaxt = "n", xaxt = "n", cex.main = 1.8, xaxs = "i", yaxs = "i")
# # # axis(side = 2, at = c(0, 150, 300), cex.axis = 1.6)
# # # axis(side = 1, at = c(0, 150, 300), cex.axis = 1.6)
# # # 
# # # abline(intercept, slope, col = col2, lwd = 5)
# # # 
# # # 
# # # # bottom legend
# # # m <- max(no_disp_ter_pred)
# # # x_pos <- m - m * 0.001
# # # y_pos <- 0 + 22
# # # legend_text <- paste("R² =", round(summary(model)$r.squared, 3),
# # #                      "\n Slope =", round(slope,3))
# # # 
# # # 
# # # # Calculate the dimensions of the text box
# # # text_width <- strwidth(legend_text)
# # # text_height <- strheight(legend_text)
# # # 
# # # x_right <- par("usr")[2]
# # # x_left <- x_pos - text_width * 1.0
# # # y_bottom <- par("usr")[3]
# # # y_top <- y_bottom + text_height * 1.15
# # # 
# # # # Add a white rectangle as the background for the legend text
# # # rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top, col = "white", border = "black")
# # # text(x = x_pos, y = y_pos, labels = legend_text, pos = 2, col = col2, cex = 1.25)
# # # 
# # # 
# # # 
# # # #######################################################
# # # ##### aq prey
# # # model <- lm(aq_prey ~ no_disp_aq_prey)
# # # # Extract slope and intercept from the model
# # # slope <- coef(model)[2]
# # # intercept <- coef(model)[1]
# # # 
# # # plot(no_disp_aq_prey, aq_prey, xlab = "",
# # #      main = "Aquatic prey", ylab = "", col = col1, pch = 16, cex.lab = 1.5, cex = 1, ylim = c(0,max(aq_prey)), xlim = c(0,max(no_disp_aq_prey)),
# # #      yaxt = "n", xaxt = "n", cex.main = 1.8, xaxs = "i", yaxs = "i")
# # # axis(side = 2, at = c(0, 750, 1500), cex.axis = 1.6)
# # # axis(side = 1, at = c(0, 1000, 2000), cex.axis = 1.6)
# # # 
# # # abline(intercept, slope, col = col2, lwd = 5)
# # # 
# # # 
# # # # bottom legend
# # # m <- max(no_disp_aq_prey)
# # # x_pos <- m - m * 0.001
# # # y_pos <- 0 + 85
# # # legend_text <- paste("R² =", round(summary(model)$r.squared, 3),
# # #                      "\n Slope =", round(slope,3))
# # # 
# # # 
# # # # Calculate the dimensions of the text box
# # # text_width <- strwidth(legend_text)
# # # text_height <- strheight(legend_text)
# # # 
# # # x_right <- par("usr")[2]
# # # x_left <- x_pos - text_width * 1.0
# # # y_bottom <- par("usr")[3]
# # # y_top <- y_bottom + text_height * 1.15
# # # 
# # # # Add a white rectangle as the background for the legend text
# # # rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top, col = "white", border = "black")
# # # text(x = x_pos, y = y_pos, labels = legend_text, pos = 2, col = col2, cex = 1.25)
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # ########################################################
# # # ##### ter prey
# # # model <- lm(ter_prey ~ no_disp_ter_prey)
# # # # Extract slope and intercept from the model
# # # slope <- coef(model)[2]
# # # intercept <- coef(model)[1]
# # # 
# # # plot(no_disp_ter_prey, ter_prey, xlab = "",
# # #      main = "Terrestrial prey", ylab = "", col = col1, pch = 16, cex.lab = 1.5, cex = 1, ylim = c(0,max(ter_prey)), xlim = c(0,max(no_disp_ter_prey)),
# # #      yaxt = "n", xaxt = "n", cex.main = 1.8, xaxs = "i", yaxs = "i")
# # # axis(side = 2, at = c(0, 3000, 6000), cex.axis = 1.6)
# # # axis(side = 1, at = c(0, 3000, 6000), cex.axis = 1.6)
# # # 
# # # abline(intercept, slope, col = col2, lwd = 5)
# # # 
# # # 
# # # # bottom legend
# # # m <- max(no_disp_ter_prey)
# # # x_pos <- m - m * 0.001
# # # y_pos <- 0 + 300
# # # legend_text <- paste("R² =", round(summary(model)$r.squared, 3),
# # #                      "\n Slope =", round(slope,3))
# # # 
# # # 
# # # # Calculate the dimensions of the text box
# # # text_width <- strwidth(legend_text)
# # # text_height <- strheight(legend_text)
# # # 
# # # x_right <- par("usr")[2]
# # # x_left <- x_pos - text_width * 1.0
# # # y_bottom <- par("usr")[3]
# # # y_top <- y_bottom + text_height * 1.15
# # # 
# # # # Add a white rectangle as the background for the legend text
# # # rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top, col = "white", border = "black")
# # # text(x = x_pos, y = y_pos, labels = legend_text, pos = 2, col = col2, cex = 1.25)
# # # 
# # # 
# # # 
# # # mtext("Abundace without dispersal", side = 1, outer = TRUE, line = 2, cex = 2.2)
# # # mtext("Abundance with dispersal", side = 2, outer = TRUE, line = 2, cex = 2.2)
# # # 
# # # 
# # 
# # # 
# # # 
# # # 
# # # ########################################################################################################################
# # # 
# # # q1 <- lm(ter_prey ~ no_disp_ter_prey)
# # # q2 <- lm(prey_ter_local ~ no_disp_prey_ter_local)
# # # 
# # # summary(q1)$r.squared
# # # summary(q2)$r.squared
# # 
