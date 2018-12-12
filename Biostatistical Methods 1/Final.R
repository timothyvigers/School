#-------------------------------------------------------------------------------
#
# Biostatistical Methods Final Exam
# Tim Vigers 12/11/18
#
#-------------------------------------------------------------------------------

# Mom vs. BMI
crudemom <- -0.28069
adjustedmom <- -0.27841
biostat <- ((crudemom - adjustedmom) / crudemom)*100
epi <- ((crudemom - adjustedmom) / adjustedmom)*100

# Mom vs. food approach
badj <- -0.12539
bcrude <- -0.28069
bz <- -0.31981
gammacheck <- (bcrude - badj)/bz
gamma <- 0.48559

prop.med <- (gammacheck * bz)/bcrude

sebz <- 0.07431
segamma <- 0.08590
seindirect <- sqrt((gamma^2*sebz)+(bz^2*segamma))
ci95indirectlow <- (bcrude - badj) - (1.96*seindirect)
ci95indirecthigh <- (bcrude - badj) + (1.96*seindirect)
ci95 <- 
  paste0(((ci95indirectlow/bcrude)*100),", ",((ci95indirecthigh/bcrude)*100))
z <- (gamma*bz)/seindirect
p <- pnorm(z)*2

exp(0.1251)
exp(0.11366)
exp(0.13653)
