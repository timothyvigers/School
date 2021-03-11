library("mgcv")
set.seed(80487)
N <- 200 
P <- 4
X <- matrix(rnorm(N*P), N, P)
## set up the association structures
f1 <- function(x) sin(pi*x)
f2 <- function(x) 2*x
f3 <- function(x) 0.25*x^3
f4 <- function(x) cos(pi*x)*x
## get the linear predictor
eta <- 1 + f1(X[,1]) + f2(X[,2]) + f3(X[,3]) + f4(X[,4]) ## simulate gaussian outcomes
y_g <- eta + rnorm(N, sd=1)
## simulate binary outcomes
pr_y <- 1/(1+exp(-eta))
y_b <- vapply(pr_y, function(x) sample(c(0,1), size=1, prob=c(1-x,x)),
              numeric(1))
## combine data into a dataframe
df_fit <- data.frame(y_g=y_g, y_b=y_b, X)
## set up basis type for all smooth terms
bs <- "cr"
## number of basis functions for all smooth terms 
K <- 20
## fit the models on the Gaussian data
fit_g_GCV <- gam(y_g ~ s(X1, bs=bs, k=K) + s(X2, bs=bs, k=K) + s(X3, bs=bs, k=K) + s(X4,bs=bs, k=K),
                 family=gaussian(), method="GCV.Cp", data=df_fit)
fit_g_REML <- gam(y_g ~ s(X1, bs=bs, k=K) + s(X2, bs=bs, k=K) + s(X3, bs=bs, k=K) + s(X4,bs=bs, k=K),
                  family=gaussian(), method="REML", data=df_fit) ## fit the models on the binary data
fit_b_GCV <- gam(y_b ~ s(X1, bs=bs, k=K) + s(X2, bs=bs, k=K) + s(X3, bs=bs, k=K) + s(X4,bs=bs, k=K),
                 family=binomial(), method="GCV.Cp", data=df_fit)
fit_b_REML <- gam(y_b ~ s(X1, bs=bs, k=K) + s(X2, bs=bs, k=K) + s(X3, bs=bs, k=K) + s(X4,bs=bs, k=K),
                  family=binomial(), method="REML", data=df_fit)
# Plot
par(mfrow=c(2,2)) 
nx_pred <- 1000 
xind_pred <- lapply(1:P, function(x){
    rn_x <- range(X[,x])
    seq(rn_x[1], rn_x[2], len=nx_pred)
  })
fn_ls <- list(f1,f2,f3,f4) 
for(p in 1:P){
  plot(fit_g_GCV, select=p, shade=TRUE)
  lines(xind_pred[[p]], fn_ls[[p]](xind_pred[[p]]),col='red',lwd=2,lty=2)
}
## set up a new data frame with all "X" predictors at a new range of values
xind_pred <- seq(-3,3,len=1000)
df_pred <- data.frame(X1=xind_pred, X2=xind_pred, X3=xind_pred, X4=xind_pred) 
head(df_pred)
# Predict
yhat_g_REML <- predict(fit_g_REML, newdata=df_pred, type="response", se.fit=T)
etahat_g_REML <- predict(fit_g_REML, newdata=df_pred, type="link", se.fit=T)
smhat_g_REML <- predict(fit_g_REML, newdata=df_pred, type="terms", se.fit=T)
Phi_g_REML <- predict(fit_g_REML, newdata=df_pred, type="lpmatrix", se.fit=T)
# Class exercise
N <- 2000
P <- 2
X <- matrix(rnorm(N*P), N, P)
## set up the association structures
f1 <- function(x) exp(x)
f2 <- function(x) x^2
eta <- 1 + f1(X[,1]) + f2(X[,2])
y_p <- rpois(N,lambda = eta)
# DF
df_fit <- data.frame(y_p=y_p, X)
# Fit
fit_p_GCV <- gam(y_p ~ s(X1, bs=bs, k=K) + s(X2, bs=bs, k=K),
                 family=poisson(), method="GCV.Cp", data=df_fit)
# Plot
par(mfrow=c(1,2)) 
plot(fit_p_GCV)
# predict
xind_pred <- seq(-3,3,len=N)
df_pred <- data.frame(X1=xind_pred, X2=xind_pred) 
smhat_p_REML <- predict(fit_p_GCV, newdata=df_pred, type="terms")
## get the design matrix for our initial fit
Phi = predict(fit_p_GCV, newdata=df_pred, type="lpmatrix")
## get the estimated function at our x-values for predicting
yhat = Phi %*% fit_p_GCV$coefficients
