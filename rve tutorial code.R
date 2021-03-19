## tutorial from Handling Complex Meta-analytic Data StructuresUsing Robust Variance Estimates: a Tutorial in R
## https://link.springer.com/content/pdf/10.1007/s40865-016-0026-5.pdf


#### install libraries ####
### Start by installing and loading all required libraries

### remove all objects from workspace
rm(list=ls())

### set working directory
#setwd("~/My Documents")

### install & load metafor package
install.packages("metafor")
library(metafor)

## Loading required package: Matrix
## Loading 'metafor' package (version 1.9-8). For an overview ## and introduction to the package please type: help(metafor).

### install & load robumeta package
install.packages("robumeta")
library(robumeta)

## Loading required package: grid

### install & load clubSandwich package from Github
install.packages("devtools")
library(devtools)

## WARNING: Rtools is required to build R packages, but is not currently installed.
##
## Please download and install Rtools 3.3 from http://cran.r-project.org/bin/windows/Rtools/ and then run find_rtools().

install_github("jepusto/clubSandwich")
## "clubSandwich" repo does not appear in "user library", but is still running
library(clubSandwich)

### install violin plot package
install.packages("vioplot")
library(vioplot)

## Loading required package: sm
## Package 'sm', version 2.2-5.4: type help(sm) for summary information

#### load dataset ####

### load example data
SchoolMotivationRisk = read.csv("SchoolMotivationRisk.csv")
View(SchoolMotivationRisk)

str(SchoolMotivationRisk)

### create between-study version of covariates 
?group.mean
SchoolMotivationRisk$aget1_m <- group.mean(SchoolMotivationRisk$aget1, SchoolMotivationRisk$studyid)
SchoolMotivationRisk$aget2_m <- group.mean(SchoolMotivationRisk$aget2, SchoolMotivationRisk$studyid)

#### create violin plot to visualize the plot ####
str(SchoolMotivationRisk)

vioplot(SchoolMotivationRisk$yi, col="grey", names="School motivation risk")
title(ylab="Risk-crime correlation")

#### fit naive random-effects model that ignores dependencies ####

res_1<-rma(yi, vi, data=SchoolMotivationRisk) 
print(res_1)
forest(res_1)
funnel(res_1)
predict(res_1, transf=transf.ztor, digits=2)

?rma()

#### fit naive mixed-effects meta-regression model that ignores dependencies ####

res_2<-rma(yi, vi, mods= ~ aget1_m*aget2_m, data=SchoolMotivationRisk) 
print(res_2)

#### fit RVE random-effects model with correlated effects weights ####

### rho assumed = 0.8; sensitivity analyses could vary rho from 0.1 to 0.9 
### small = TRUE option applies the small sample correction to df 
res_3<-robu(formula = yi ~ 1, var.eff.size=vi, studynum = studyid, 
            modelweights = "CORR", rho = 0.8, small=TRUE, data=SchoolMotivationRisk) 
print(res_3)

#### fit RVE mixed-effects meta-regression with age main effects ####
## The Wald_test function tests the linear contrasts from a regression model, using a sandwich estimator for the variance-covariance
## matrix and a small sample correction for the p-value. Various corrections can be used by specifying test = "". Type "?Wald_test" 
## for more information.

res_4<-robu(formula = yi ~ aget1_m + aget2_m, var.eff.size=vi, studynum = studyid, 
            modelweights = "CORR", rho = 0.8, small=TRUE, data=SchoolMotivationRisk)
print(res_4)
Wald_test(res_4, constraints = 2:3, vcov="CR2")

#### fit RVE mixed-effects meta-regression model with age interaction ####

res_5<-robu(formula = yi ~ aget1_m*aget2_m, var.eff.size=vi, studynum = studyid, 
            modelweights = "CORR", rho = 0.8, small=TRUE, data=SchoolMotivationRisk)
print(res_5)
Wald_test(res_5, constraints = 2:4, vcov="CR2")

#### fit RVE mixed-effects meta-regression model with age interaction, adjusting for sex mix of samples ####

res_6<-robu(formula = yi ~ aget1_m*aget2_m + sexmix, var.eff.size=vi, studynum = studyid, 
            modelweights = "CORR", rho = 0.8, small=TRUE, data=SchoolMotivationRisk)
print(res_6)
Wald_test(res_6, constraints = 2:6, vcov="CR2")

### Omnibus F-test for categorical sex mix variable
Wald_test(res_6, constraints = 4:5, vcov="CR2")

#### create graph to illustrate the distribu on of the correla ons by age ####
## start by manipulating the dataset

install.packages("ggplot2")
library(ggplot2)

### graph prediction line at different ages, overlaid with effect sizes 
### create interaction variable
SchoolMotivationRisk$aget1_mXaget2_m <- SchoolMotivationRisk$aget1_m*SchoolMotivationRisk$aget2_m

### create factored age1 variable
SchoolMotivationRisk$age1_cat <- ifelse(SchoolMotivationRisk$aget1 <= 12.9,1,
                                        ifelse(SchoolMotivationRisk$aget1 <= 14.9, 2,
                                               ifelse(SchoolMotivationRisk$aget1 <= 16.9, 3, 
                                                      ifelse(SchoolMotivationRisk$aget1 <= 18.9, 4, NA))))
View(SchoolMotivationRisk)

SchoolMotivationRisk$age1_cat <- factor(SchoolMotivationRisk$age1_cat, 
                                        levels = c(1,2,3,4),
                                        labels = c("11-13", "13-15", "15-17", "17-19"))

#### create plot using ggplot2 ####
## more information about the various modifiers may be found by typing "?ggplot2"

Graph_withoutSE <- ggplot(data = SchoolMotivationRisk,
                          aes(x = aget2_m, y = yi, color = factor(age1_cat))) +
  geom_point(position = "jitter") +
  geom_smooth(method = lm, se = FALSE) + 
  scale_color_discrete(name = "Age at risk measurement") + 
  ylab("Risk-crime correlation") +
  xlab("Age at crime outcome measurement") + 
  theme(axis.ticks.x = element_blank()) +
  theme(text = element_text(size = 18)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor =  element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 

Graph_withoutSE

Graph_withSE <- ggplot(data = SchoolMotivationRisk,
                       aes(x = aget2_m, y = yi, color = factor(age1_cat))) +
  geom_point(position = "jitter") +
  geom_smooth(method = lm) +
  scale_color_discrete(name = "Age at risk measurement") + 
  ylab("Risk-crime correlation") +
  xlab("Age at crime outcome measurement") + 
  theme(axis.ticks.x = element_blank()) +
  theme(text = element_text(size = 18)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 

Graph_withSE

summary(SchoolMotivationRisk)
summary(as.factor(SchoolMotivationRisk$studyid))
