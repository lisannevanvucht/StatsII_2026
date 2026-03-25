#####################
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

lapply(c("nnet", "MASS"),  pkgTest)

library (tidyverse)

# set wd for current folder
setwd("/Users/lisannevanvucht/Documents/TCD/Hillary Term/Quants II/PS/PSO3")

#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)

#check data
str(gdp_data)

#when I check this structure of GDPWdiff I see that it is an integer and numbers
#can be positive, negative and potentially also zero if there if zero 'raw difference'

#I select the relevant variables as asked in the prompt
#and drop the missing observations 
gdp_q1 <- na.omit(gdp_data[, c("GDPWdiff", "REG", "OIL")])

# I create a categorical variable for numerical 'raw difference' in GDP change
#this means that negative is a decrease in GDP, positive implies an increase in GDP
#and equal to zero means no change in GDP that year
gdp_q1$GDPWdiff_cat <- case_when(
  gdp_q1$GDPWdiff < 0  ~ "negative",
  gdp_q1$GDPWdiff > 0  ~ "positive",
  gdp_q1$GDPWdiff == 0 ~ "no change"
)

# I check the distribution of change in GDP in a given year using this table
table(gdp_q1$GDPWdiff_cat)

#this shows that in total there are 1105 observations in which the GDP change 
#was negative meaning that it decreased. furthermore, are 16 zeros meaning that
#the GDP did not change and there are 2600 observations in which it increased
#between 1950 or the year of independence or the first year forwhich data on 
#economic growth are available ("entry year"), and 1990 or the last year for 
#which data on economic growth are available ("exit year").

#part A

#STEP 1: 
#the assignment asks for the 'no change' category as reference/baseline category. 
gdp_q1$GDPWdiff_unordered <- relevel(
  factor(gdp_q1$GDPWdiff_cat, levels = c("no change", "negative", "positive")),
  ref = "no change"
)

#STEP 2:
# I apply an unordered multinomial logit model. I assume no order between
#the cateorgies; positive, negative and no change, they are simply three seperate
#options.
#we use trace = FALSE because we also used it during the tutorial practise
mnl_mod <- multinom(GDPWdiff_unordered ~ REG + OIL, data = gdp_q1, trace = FALSE)

#results
summary(mnl_mod)

#STEP 3:
#multinom() does not give us the p-values, just the coefficients, therefore
# we calculate the p's manually by calculating the z scores (standard error)
#p = the change of such an extreme z value if H0 would be true
# z = this tells us how many standard errors the coefficient is away from zero
# the further from zero, the less likely it is due to coincidence
# p = probability of observing a z-value this extreme if H0 (no effect) were true
# we multiply by 2 because it is a two-sided test (effect could be + or -)
# pnorm() gives us the cumulative probability under the standard normal distributio
# p < 0.05  = significant  (*)
# p < 0.01  = significant  (**)
# p < 0.001 = significant  (***)
mnl_z <- summary(mnl_mod)$coefficients / summary(mnl_mod)$standard.errors
mnl_p <- (1 - pnorm(abs(mnl_z), 0, 1)) * 2

mnl_z
mnl_p

# STEP 4
#the coefficients are log-odds, and difficult to directly interpret. Therefore
#we take exp() and transform them into odds ratios. If this value > 1 this means
#that we are more likely to observe this category instead of our reference category.
# < 1 means a lower chance to observe this category instead of our reference category.
exp(coef(mnl_mod))

# part B

# STEP 1:
#for the ordered logit I assume there is a natural order between the categories:
#negative < no change < positive. First, I create an ordered factor to reflect
#this.
gdp_q1$GDPWdiff_ordered <- ordered(
  gdp_q1$GDPWdiff_cat,
  levels = c("negative", "no change", "positive")
)

# STEP 2:
#Here I apply an ordered multinomial logit model using polr() from the MASS package.
# Hess = TRUE implies a Hessian matrix.
ologit_mod <- polr(GDPWdiff_ordered ~ REG + OIL, data = gdp_q1,
                   method = "logistic", Hess = TRUE)

#results
summary(ologit_mod)

# STEP 3:
#we manually calculate the p values
#
ctable <- coef(summary(ologit_mod))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# STEP 4:
# odds ratios via exp(), same as Tutorial 5
exp(coef(ologit_mod))

# STEP 5:
# the cutoff points (zeta) tell us where the model draws the boundary
# between the ordered categories
ologit_mod$zeta


#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")

#outcome variable = number of times the winning PAN presidential candidate in 2006
#visited (pan.visits.06) a district leading up to the 2009 federal elections,
#this is a count # number of times, fixed, discrete. Our main predictor of interests
#is whether the district was highly contested or whether it was not
#(the PAN or their opponents have electoral security) in the previous federal 
#elections during 2000 (competitive.district), which is binary 
#(1=close/swing district, 0="safe seat"). We also include \texttt{marginality.06}
#(a measure of poverty) and \texttt{PAN.governor.06} (a dummy for whether the 
#state has a PAN-affiliated governor) as additional control variables. 


str(mexico_elections)

mode(mexico_elections)
summary(mexico_elections$PAN.visits.06)
table(mexico_elections$competitive.district)



#we see that pan.visits is an integer, meaning that it is counts indeed. therefore
#Poisson would be the suiting model here

#furthermore, we see that the median is 0, meaning that and the mean is 0.09, this 
#implies that we have loads of zeros in our data set. which makes sense, because
# a candidate might have never visited a district, which has equal relevance
#in our analysis as when someone has visited one time. therefore 0 has a meaning. 

#check of mean ≈ variance to see if the variance is equal to the mean, if not?
#this is the first red flag for poisson
mean(mexico_elections$PAN.visits.06)
var(mexico_elections$PAN.visits.06)

dispersiontest(mexico_elections$PAN.visits.06)

#we see that the variance is 0.64 and the mean is 0.09 meaning that they are not
#equal. This means there is overdispersion, meaning that the data shows
#higher variability than expected.

#map the data frequencies
hist(mexico_elections$PAN.visits.06, breaks = 20)

# I select the relevant variables and drop missing observations
mexico_q2 <- na.omit(mexico_elections[, c(
  "PAN.visits.06",
  "competitive.district",
  "marginality.06",
  "PAN.governor.06"
)])

# STEP 1:
#the outcome variable PAN.visits.06 is a count variable (non-negative integer)
# Therefore, as discussed during the tutorial, 
#OLS is not appropriate here — it would predict negative values and violates
#the assumption of constant variance. As the number of visits increases,
#the dispersion also increases (as noted in Tutorial 9). Therefore I use 
#a Poisson regression with a log link.
pois_mod <- glm(
  PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
  data   = mexico_q2,
  family = poisson(link = "log")
)

summary(pois_mod)

# STEP 2:
#A: is there evidence that PAN candidates visit swing districts more?
#I calculate the z-statistic and p-value for competitive.district directly
#from the coefficient table
comp_z <- coef(summary(pois_mod))["competitive.district", "z value"]
comp_p <- coef(summary(pois_mod))["competitive.district", "Pr(>|z|)"]

comp_z
comp_p

# STEP 3:
#B: interpret marginality.06 and PAN.governor.06
#I exponentiate the coefficients to get Incidence Rate Ratios (IRRs)
#exp(coef()) transforms log-counts into multipliers
#IRR > 1 means more visits, IRR < 1 means fewer visits
exp(coef(pois_mod))

# STEP 4:
#C: predicted mean number of visits for a hypothetical district
# competitive.district = 1, marginality.06 = 0, PAN.governor.06 = 1
# following Tutorial 9: predict() with type = "response" gives expected count
new_case <- data.frame(
  competitive.district = 1,
  marginality.06       = 0,
  PAN.governor.06      = 1
)

predict(pois_mod, newdata = new_case, type = "response")

