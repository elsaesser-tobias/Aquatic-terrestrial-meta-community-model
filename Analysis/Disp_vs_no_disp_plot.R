#load results from no dispersal simulation and clear enviroment from it after
library(OCNet)
library(ggplot2)
library(dplyr)


load("Abundances_1.RData")

disp_hq_aq_pred <- split_df3$Coex$aq_pred_sorted
disp_hq_ter_pred <- split_df3$Coex$ter_pred_sorted
disp_hq_aq_prey <- split_df3$Coex$res_sum_prey1
disp_hq_ter_prey <- split_df3$Coex$res_sum_prey2
disp_hq_hq_regio <- split_df3$Coex$res_mean_habitat_quality


load("Abundances_2.RData")

hq_aq_pred <- split_df3$Coex$aq_pred_sorted
hq_ter_pred <- split_df3$Coex$ter_pred_sorted
hq_aq_prey <- split_df3$Coex$res_sum_prey1
hq_ter_prey <- split_df3$Coex$res_sum_prey2
hq_hq_regio <- split_df3$Coex$res_mean_habitat_quality

load("Abundances_3.RData")

disp_aq_pred <- split_df3$Coex$aq_pred_sorted
disp_ter_pred <- split_df3$Coex$ter_pred_sorted
disp_aq_prey <- split_df3$Coex$res_sum_prey1
disp_ter_prey <- split_df3$Coex$res_sum_prey2
disp_hq_regio <- split_df3$Coex$res_mean_habitat_quality

load("Abundances_4.RData")

aq_pred <- split_df3$Coex$aq_pred_sorted
ter_pred <- split_df3$Coex$ter_pred_sorted
aq_prey <- split_df3$Coex$res_sum_prey1
ter_prey <- split_df3$Coex$res_sum_prey2
hq_regio <- split_df3$Coex$res_mean_habitat_quality




#aq_pred
aq_pred_r <- c()
x <- hq_aq_pred
y <- disp_hq_aq_pred
model <- lm(y ~ x)
aq_pred_r[1] <- summary(model)$r.squared


x <- disp_aq_pred
y <- disp_hq_aq_pred
model <- lm(y ~ x)
aq_pred_r[2] <- summary(model)$r.squared

x <- aq_pred
y <- disp_hq_aq_pred
model <- lm(y ~ x)
aq_pred_r[3] <- summary(model)$r.squared




#ter_pred
ter_pred_r <- c()
x <- hq_ter_pred
y <- disp_hq_ter_pred
model <- lm(y ~ x)
ter_pred_r[1] <- summary(model)$r.squared


x <- disp_ter_pred
y <- disp_hq_ter_pred
model <- lm(y ~ x)
ter_pred_r[2] <- summary(model)$r.squared

x <- ter_pred
y <- disp_hq_ter_pred
model <- lm(y ~ x)
ter_pred_r[3] <- summary(model)$r.squared



#aq_prey
aq_prey_r <- c()
x <- hq_aq_prey
y <- disp_hq_aq_prey
model <- lm(y ~ x)
aq_prey_r[1] <- summary(model)$r.squared


x <- disp_aq_prey
y <- disp_hq_aq_prey
model <- lm(y ~ x)
aq_prey_r[2] <- summary(model)$r.squared

x <- aq_prey
y <- disp_hq_aq_prey
model <- lm(y ~ x)
aq_prey_r[3] <- summary(model)$r.squared




#ter_prey
ter_prey_r <- c()
x <- hq_ter_prey
y <- disp_hq_ter_prey
model <- lm(y ~ x)
ter_prey_r[1] <- summary(model)$r.squared


x <- disp_ter_prey
y <- disp_hq_ter_prey
model <- lm(y ~ x)
ter_prey_r[2] <- summary(model)$r.squared

x <- ter_prey
y <- disp_hq_ter_prey
model <- lm(y ~ x)
ter_prey_r[3] <- summary(model)$r.squared


minimum_value <- min(aq_pred_r,ter_pred_r,aq_prey_r,ter_prey_r)

aq_pred_r <- 1 - aq_pred_r
ter_pred_r <- 1 - ter_pred_r
aq_prey_r <- 1 - aq_prey_r
ter_prey_r <- 1 - ter_prey_r


data <- data.frame(
  Group = factor(rep(c("Predator AP", "Predator TP", "Aquatic Prey", "Terrestrial Prey"), each = 3),
                 levels = c("Predator AP", "Predator TP", "Aquatic Prey", "Terrestrial Prey")),
  Category = factor(rep(c("dispersal", "habitat quality","both combined"),
                      times = 4),
                    levels = c("dispersal", "habitat quality",
                               "both combined")),  # Categories 1 to 4
  Value = c(aq_pred_r, ter_pred_r, aq_prey_r, ter_prey_r)
)






ggplot(data, aes(x = Group, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = NULL, y = "Effect of ... on abundance", fill = NULL) +
  scale_fill_manual(values = c("dispersal" = "#1f77b4",  # Blue
                               "habitat quality" = "#ff7f0e",  # Orange
                               "both combined" = "#2ca02c"))





# aq_pred_r[1] * aq_pred_r[2]
# aq_pred_r[3]
# 
# ter_pred_r[1] * ter_pred_r[2]
# ter_pred_r[3]
# 
# aq_prey_r[1] * aq_prey_r[2]
# aq_prey_r[3]
# 
# ter_prey_r[1] * ter_prey_r[2]
# ter_prey_r[3]

