passenger <- data.frame(matrix(c(123,200,158,119,528,181),nrow = 3,ncol = 2,byrow = T))
colnames(passenger) <- c("no","yes")
passenger$class <- 1:3

passenger.long <- reshape(passenger,direction='long',
                          varying=c('yes','no'),v.names='count',
                          timevar='survive',times=1:0,idvar = "class")

passenger.longrep <- passenger.long[rep(1:6,passenger.long$count),c('class','survive')]

intmod <- glm(survive ~ 1,data = passenger.longrep,family = "binomial")
classmod <- glm(survive ~ as.factor(class),data = passenger.longrep,family = "binomial")
logLik(intmod)
logLik(classmod)