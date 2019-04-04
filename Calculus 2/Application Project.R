library(ggplot2)
library(reshape2)
# Create dummy dataframe.
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
# Define pure exponential growth rate function.
exponential.rate <- function(p,r = 0.1) {
  r*p
}
# Plot.
exponential.plot <- p+
  stat_function(fun = exponential.rate)+
  xlim(0,500)+
  xlab("Population")+
  ylab("Growth Rate")+
  ggtitle("Pure Exponential Growth Rate as a Function of Population Size, \n r = 0.1")
# Define logistic growth rate function.
logistic.rate <- function(p, r = 0.1, K = 500) {
  r * p * (1-(p/K))
}
# Plot.
logistic.plot <- p+
  stat_function(fun = logistic.rate)+
  xlim(0,500)+
  xlab("Population")+
  ylab("Growth Rate")+
  ggtitle("Logistic Growth Rate as a Function of Population Size, \n r = 0.1 K = 500")
# Make population sketch data table.
population.sketch <- 
  data.frame(population = c(10, 50, 100, 250, 350, 400, 450, 480, 490, 500),
             time = c(1:10))
# Plot.
population.sketch.plot <- 
  ggplot(population.sketch, aes(y = population.sketch$population, x = population.sketch$time))+
  geom_smooth(se = FALSE)+
  ylab("Population")+
  xlab("Time")+
  ggtitle("Logistic Model of Population Over Time (Sketch)")
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
# Define logistic population function.
  logistic.population <- function(t, r = 0.1, K = 500, pz = 10) {
    (K * pz)/(((K-pz)*(exp(-(r*t))))+pz)
  }
# Plot.
  population.plot <- p+
    stat_function(fun = logistic.population)+
    xlim(0,150)+
    ylab("Population")+
    xlab("Time")+
    ggtitle("Logistic Model of Population Over Time")
# Make a table to graph different values of r.
  changing.r <- data.frame(matrix(nrow = 200,ncol = 5))
  colnames(changing.r) <- c("time","r = 0.05","r = 0.1","r = 0.2","r = 0.5")
  changing.r$time <- c(1:200)
  changing.r$`r = 0.05` <- logistic.population(t = c(1:200),r = 0.05, K = 500, pz = 10)
  changing.r$`r = 0.1` <- logistic.population(t = c(1:200),r = 0.1, K = 500, pz = 10)
  changing.r$`r = 0.2` <- logistic.population(t = c(1:200),r = 0.2, K = 500, pz = 10)
  changing.r$`r = 0.5` <- logistic.population(t = c(1:200),r = 0.5, K = 500, pz = 10)
# Plot different values for R
  melted.changing.r <- melt(changing.r,id.vars = "time")
  r.values.plot <- 
    ggplot(melted.changing.r, aes(x = time, y = value,color = variable))+
    geom_line()+
    xlab("Time")+
    ylab("Population")+
    ggtitle("Population Curves with Different r Values")+
    theme(legend.title=element_blank())
# Make a table to graph different values of K.
  changing.k <- data.frame(matrix(nrow = 200,ncol = 5))
  colnames(changing.k) <- c("time","K = 100","K = 500","K = 1000","K = 1500")
  changing.k$time <- c(1:200)
  changing.k$`K = 100` <- logistic.population(t = c(1:200),r = 0.1, K = 100, pz = 10)
  changing.k$`K = 500` <- logistic.population(t = c(1:200),r = 0.1, K = 500, pz = 10)
  changing.k$`K = 1000` <- logistic.population(t = c(1:200),r = 0.1, K = 1000, pz = 10)
  changing.k$`K = 1500` <- logistic.population(t = c(1:200),r = 0.1, K = 1500, pz = 10)
  # Plot different values for R
  melted.changing.k <- melt(changing.k,id.vars = "time")
  k.values.plot <- 
    ggplot(melted.changing.k, aes(x = time, y = value,color = variable))+
    geom_line()+
    xlab("Time")+
    ylab("Population")+
    ggtitle("Population Curves with Different K Values")+
    theme(legend.title=element_blank())

