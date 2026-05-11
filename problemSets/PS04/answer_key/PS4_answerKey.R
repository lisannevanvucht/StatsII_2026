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

lapply(c("eha", "survival", "survminer", "sampleSelection"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data on child mortality by mother's background and child gender
data("child")

# estimate duration Cox PH model that includes both predictors (child, mother)
infantMorality <- coxph(Surv(enter, exit, event) ~ m.age + sex, data = child)

# Create agegroup from mother's age (23 and 30 as cut points)
pdf("figure1.pdf", width=10)
plot(survfit(infantMorality, newdata = data.frame(sex=c("male","male","female","female"), m.age=c(23,30,23,30))),
     lty = 1:4, col = 1:4, axes=F, ylim = c(0.7, 1.0), xlab="Survival Time (Years)", ylab="Survival Probability")
axis(1); axis(2)
legend("bottomleft", c("Boy w/ 23 year old mother", "Boy w/ 30 year old mother", "Girl w/ 23 year old mother", "Girl w/ 30 year old mother"),
       lty=c(1:4), col=c(1:4), bty='n')
dev.off()

#####################
# Problem 2
#####################

# read in disaster relief data
disasters <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/refs/heads/main/datasets/disaster_response.csv")
# define selection equation
selectEq <- binContribution ~ occurrences  + deathsEM + normalizedDamageEMLogged 
# define outcome equation
outcomeEq <- originalContributionMillionUSDLogged ~ occurrences  + deathsEM + normalizedDamageEMLogged 
# estimate heckman selection model
heckFullModel <- heckit(selectEq, outcomeEq, data = disasters)
