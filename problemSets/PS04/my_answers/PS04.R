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

lapply(c("nnet", "MASS", "survival", "eha", "stargazer", "sampleSelection"),  pkgTest)

# set wd for current folder
setwd("/Users/lisannevanvucht/Documents/TCD/Hillary Term/Quants II/PS/PS04")

#####################
# Problem 1
#####################

# load data on child mortality by mother's background and child gender
data("child")

# i check the structure

str(child)

# I create an object for survival. enter is the age when a child enters the risico group
# this is always 0 and 'exit' is the age when it leaves that group (dead or aged 15)
#event tells us whether it was (1) dead or censuring (0)

child_survival <- with(child, Surv(enter, exit, event))
child_cox <- coxph(child_survival ~ m.age + sex, data = child)

# first look output
summary(child_cox)

#model fit test > see what happens if you drop one of the variables? we see that 
#the AIC increases to 113022 wif we would drop age and 113018 if we were to drop 
#sex so both help model fit here

drop1(child_cox, test = "Chisq")

#create nice table
stargazer(child_cox)

#output:

# we have around 26K children in the dataset of which 5616 died and approx 21000
#censured on 15

# we see that age is exponentially 1007 with a p value of 0.0003 which is highly
#significant. This tells us that for every year that a mother gets older, the hazard of 
# children morality increases by 0.76% keeping all other predictors (in this case 
#only gender) constant. this is a small but significant effect.


#we see that the coffisient is 0.921 exponantially for gender. in which 'male'
# is the reference category. this means that females have a hazard ratio for every
#100 deaths among males, 92 girls die. This is a 8% lower change in dying 
#before the age of 15, keeping all other predictors (in this case only age) constant


#model fit: we see that the concordance is 0.519 which means that our results are
#likely to be due to coincidence. this might be due to the fact that we only have two
#explaining variables and we could expect there are more factors contributing to 
#19th century children mortality beyond age and gender


#####################
# Problem 2
#####################

# load data
disaster_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/refs/heads/main/datasets/disaster_response.csv")


#structuur check
str(disaster_data)

#originalcontribution in million usd is -25.3 for all cases when bincontribution = 0
#this is a place holder 


# i want to see how this original contirbution variable is strucutred using summary
# but i want it seperately dependeing on whether contribution takes value 0 or 1
#because we want to know if -25.4 is really a place holder or a true vallue

#so i check the distibution

tapply(
  disaster_data$originalContributionMillionUSDLogged,
  disaster_data$binContribution,
  summary
)


#results show that for bincontribution the logged contribution is always exactly
# -25,33 nd this is not a true value but a place holder for ''no donation'' 
#when we look at the variation for bincontribution = 1 meaning there is a donation 
#we see that values range between -9 and 6,5

# select only the variables needed for the question
disaster_cleaned <- disaster_data[, c(
  "binContribution",
  "originalContributionMillionUSDLogged",
  "occurrences", "deathsEM", "normalizedDamageEMLogged"
)]

# recode placeholder to NA
# because the Heckman model only predicts outcomes when the donation variable holds 1 and
# not 0. we do not want R to think -25.33 is an observed value it will mess the
#model's predictions
disaster_cleaned$originalContributionMillionUSDLogged[
  disaster_cleaned$binContribution == 0
] <- NA

# selection() requires a factor
disaster_cleaned$binContribution <- factor(disaster_cleaned$binContribution, levels = c(0, 1))


#selection = was there a donation (binary)
# outcome = models how much donated but only if bincontribution = 1
# method = maximum likelihood. 
disaster_model <- selection(
  selection = binContribution ~ occurrences + deathsEM + normalizedDamageEMLogged,
  outcome   = originalContributionMillionUSDLogged ~ occurrences + deathsEM + normalizedDamageEMLogged,
  data      = disaster_cleaned,
  method    = "ml"
)
summary(disaster_model)

#nice tabel

stargazer(disaster_model)


