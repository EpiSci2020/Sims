set.seed(123)
library(simstudy)
library(ggplot2)
library(GGally)

#create a correlation matrix
rel <- matrix(c(1, .52, -.10, .15,
                .52,  1,   0,   0,
                -.10, 0,   1,   0,
                .15,  0,   0,   1), nrow = 4)
rel

ozone <-genCorData(100, mu = c(16, 105,10,50),
                   sigma = c(6, 50, 5, 15),
                   corMatrix = rel)
colnames(ozone) <- c("id","oz", "sol", "wind", "temp")
ozone <- round(ozone, 0)
head(ozone)  
summary(ozone) 
ozone$wind <- ifelse(ozone$wind < 0, 0, ozone$wind)
summary(ozone)

#pairs plot
ggpairs(ozone[, 2:4],
        lower = list(continuous = "smooth") ,
        upper = list(continuous = w_ggally_cor),
        diag = list(continuous = "densityDiag"))

#create missing wind observations
zap <- unique(as.integer(runif(22, min = 1, max = 100)))
zap
length(zap)
for(i in zap) {
  ozone$wind[i] <- NA
}
summary(ozone)
ozone








