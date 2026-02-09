#####################
# project: PS01
# author: Lisanne van Vucht

# load libraries
# set wd
# clear global .envir
#####################


# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd("/Users/lisannevanvucht/Documents/TCD/Hillary Term/Quants II/Tutorials/WK II")

#####################
# Problem 1
#####################

#step 1: generate data

#We generate our own data by the following prompt. This Cauchy data is used 
#because it is not normally distributed because of its fat tails. So generally
#it deviates from pnorm()

set.seed(123)
data<- rcauchy(1000, location = 0, scale = 1)

#check data structure
#data
#str(data)

#step 2: 

ks_byhand <- function(data) {
  ECDF <- ecdf(data)
  
# evaluate the empirical CDF at each observed data point
  empiricalCDF <- ECDF(data)
  
# D is the maximum absolute difference between empirical and theoretical CDFs
  D <- max(abs(empiricalCDF - pnorm(data)))
  
# calculate the p-value > How extreme is D if the data is normally distibuted?
# Pi on R approximates to 3.141593
  sum_term <- 0.0
  for (k in 1:1000) {
    exponent <- - (((2*k - 1)^2) * (pi^2)) / ((8 * D^2))
    term <- exp(exponent)
    sum_term <- sum_term + term
  }
  
  p_value <- (sqrt(2*pi) / D) * sum_term
  
  return(list(
    empiricalCDF = empiricalCDF,
    D = D,
    p_value = p_value
  ))
}

# manual K-S test function I created
ks_byhand(data)

#step 3: peform the K-S test 
ks.test(data,"pnorm")

##############################################################################
# Problem 2
#####################

# step 1: create data
# where the true slope is 2.75 so y = 2.75*x 

set.seed(123)
ex_data <- data.frame(x = runif(200, 1, 10))
ex_data$y <- 0 + 2.75 * ex_data$x + rnorm(200, 0, 1.5)


# step 2 

ols_manual <- function(outcome, input, parameter) {
  
# number of betas
  n <- ncol(input)
  beta <- parameter[1:n]
  
# predicted values
  y_hat <- input %*% beta
  
# squared errors
  sum((outcome - y_hat)^2)
}


# step 3 
results_ols <- optim(
  fn      = ols_manual,
  outcome = ex_data$y,
  input   = cbind(1, ex_data$x),  # column of 1s = intercept
  par     = c(1, 1),              # starting guess
  method  = "BFGS",
)

# print intercept and slope
round(results_ols$par, 2)

# confirm same result with lm()
round(coef(lm(y~x, data=ex_data)), 2)

