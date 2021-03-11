library(mgcv)
library(tidyverse)
# Simulate
set.seed(500)
N <- 5000
nx_pred <- 100
x1 <- runif(N, -3, 3)
x2 <- runif(N, -3, 3)
f1 <- function(x1,x2) 2*cos(pi*x1/4)*sin(pi*x2/2) 
f2 <- function(x1,x2) sin(pi/2 + x1) + cos(x2^2/4) 
y_x1_x2_f1 <- f1(x1,x2) + rnorm(N)
y_x1_x2_f2 <- f2(x1,x2) + rnorm(N)
df_fit <- data.frame(y_x1_x2_f1,y_x1_x2_f2,x1,x2)
## fit the two models 
fit_te_x1_x2_f1 <- gam(y_x1_x2_f1 ~ te(x1, x2, k=10, bs="cr"), method="REML", data=df_fit)
fit_te_x1_x2_f2 <- gam(y_x1_x2_f2 ~ te(x1, x2, k=10, bs="cr"), method="REML", data=df_fit)
# get estimated coefficients
# grid of new x1, x2 values to predict on
x1_pred <- seq(min(x1),max(x1),len=nx_pred)
x2_pred <- seq(min(x2),max(x2),len=nx_pred)
# get all combinations of x1 and x2 values
df_pred <- expand.grid(x1=x1_pred, x2=x2_pred)
# True values
df_pred$f1y = f1(df_pred$x1,df_pred$x2)
df_pred$f2y = f2(df_pred$x1,df_pred$x2)
# get actual coefficient estimates
f1hat_x1_x2 <- predict(fit_te_x1_x2_f1, newdata=df_pred, type='terms') 
f2hat_x1_x2 <- predict(fit_te_x1_x2_f2, newdata=df_pred, type='terms')
# Plot a heatmaps of fhat(x1,x2) - ftrue(x1,x2)
plt_diff <-
  data.frame(df_pred, 
             f1diff=df_pred$f1y-f1hat_x1_x2[,"te(x1,x2)"],
             f2diff=df_pred$f2y-f2hat_x1_x2[,"te(x1,x2)"]) %>%
  pivot_longer(cols=c("f1diff","f2diff")) %>%
  ggplot() + theme_bw() +theme_classic(base_size=18) +
  geom_raster(aes(x1,x2,fill=value)) + facet_wrap(~name) +
  scale_fill_gradientn(colours=c("red","white","blue"), limits=c(-2,2))

plt_diff
