set.seed(123)
library(simstudy)
library(ggplot2)
library(GGally)
library(R.utils)

#create a correlation matrix
rel <- matrix(c(1, .52, -.10, .15,
                .52,  1,   0,   0,
                -.10, 0,   1,   0,
                .15,  0,   0,   1), nrow = 4)
rel

#create the correlated data
ozone <-genCorData(100, mu = c(16, 105,10,50),
                   sigma = c(6, 50, 5, 15),
                   corMatrix = rel)
colnames(ozone) <- c("id","oz", "sol", "wind", "temp")
ozone <- round(ozone, 0)
head(ozone)  
summary(ozone) 
ozone$wind <- ifelse(ozone$wind < 0, 0, ozone$wind)
summary(ozone)

#pairs plot for ozone
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

#different approaches to impute missing data
#impute the mean
ozone1 <- ozone
ozone1$wind <- ifelse(is.na(ozone1$wind),
                      mean(ozone1$wind, na.rm = TRUE),
                      ozone1$wind)
summary(ozone1)
ggplot(ozone, aes(x = wind, y = oz)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(aes(y = wind,
                 x = mean(ozone$wind, na.rm = TRUE)),
             shape = 0, color = "blue")

#******************************************************************
#impute normally distributed random selection of 19 values
imp <- round(rnorm(19, mean = mean(ozone$wind, na.rm = TRUE),
                   sd = sd(ozone$wind, na.rm = TRUE)),0)
imp

ggplot(ozone, aes(x = wind, y = oz)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(aes(y = wind,
                 x = mean(ozone$wind, na.rm = TRUE)),
             shape = 0, color = "blue")
#replace the NA's with the imps
ozone2 <- ozone
nas <- which(is.na(ozone2$wind))
nas
ozone2$wind <- replace(ozone2$wind,
                      list = nas,
                      values = imp)














