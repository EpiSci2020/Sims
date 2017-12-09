set.seed(123)
library(simstudy)

#create a 2x2 matriz to show solar radiation and ozone
#with a .52 correlation
rel <- matrix(c(1, 0.52, 0.52, 1), nrow = 2)
sol_oz <- genCorData(100, mu = c(120, 15),
                 sigma = c(50, 5),
                 corMatrix = rel)
colnames(sol_oz) <- c("id","sol", "oz")
sol_oz$sol <- round(sol_oz$sol, 0)
sol_oz$oz <- round(sol_oz$oz, 0)
#create wind and tmperature data
wind_temp <- defData(varname = "wind", 
               dist = "gamma",
               formula = 10,
               variance = 1)
wind_temp <- defData(wind_temp, varname = "temp", 
               dist = "normal",
               formula = 50,
               variance = 30)
win_tmp <- genData(100, wind_temp)
win_tmp$wind <- round(win_tmp$wind, 1)
win_tmp$temp <- round(win_tmp$temp, 0)
ozone <- cbind(sol_oz, win_tmp)
ozone <- ozone[,-4]

coef(lm(oz ~ sol, data = sol_oz))
ggplot(ozone, aes(x = temp, y = oz)) +
  geom_point() +
  geom_abline(intercept = 10, slope = .044)





defM <- defMiss(varname = "w", formula = 10)
def_obs <- genObs(data, defM, idvars = "id")



  






