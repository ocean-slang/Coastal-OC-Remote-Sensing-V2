
library(boot)
library(lmodel2)
setwd('/Volumes/slangSSD/mcwc_R_mat')


#data for Part 1: Statistical Water Quality Model
data<-read.csv("matchup.csv")

L8_data<-data[data$type=="L8",];
S2_data<-data[data$type=="S2",];
#bootstrapping

# Bootstrap 95% CI for R-Squared

set.seed(980)
# function to obtain R-Squared from the data

bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}

rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$adj.r.square)
}

rwsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$r.squared)
}


stnderr<- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$sigma)
}

# bootstrapping with 1000 replications
results <- boot(data=S2_data, statistic=bs,
                R=10000, formula=lm(formula = insitu ~ sat, data = S2_data))

results <- boot(data=L8_data, statistic=bs,
                R=10000, formula=lm(formula = insitu ~ sat, data = L8_data))

# view results
results
plot(results,index=1)
plot(results,index=2)

# get 95% confidence interval
boot.ci(results, type="bca",index=1) #intercept
boot.ci(results, type="bca",index=2) #slope

results1 <- boot(data=L8_data, statistic=rsq,
                 R=10000, formula=lm(formula = insitu ~ sat, data = L8_data))


results1 <- boot(data=S2_data, statistic=rsq,
                 R=10000, formula=lm(formula = insitu ~ sat, data = S2_data))

# view results
results1
plot(results1)

# get 95% confidence interval
boot.ci(results1, type="bca")


results2 <- boot(data=data, statistic=stnderr,
                 R=10000, formula=lmodel2(formula = insitu ~ sat, data = L8_data))

# view results
results2
plot(results2)

# get 95% confidence interval
boot.ci(results2, type="bca")