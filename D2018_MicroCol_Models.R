# METADATA===================================================================
# 2018 BUMBLE BEE MICROCOLONY EXPERIMENTS
# Model construction and analysis
# Jeremy Hemberger - j.hemberger.wisc@gmail.com
# November 28, 2018

# Model microcolony response to landscape-simulated food treatments
# Experiment 1 dates: March 15, 2018 - May 24, 2018
# Experiment 2 dates: 
# Experiment 3 dates: 


# Import packages ---------------------------------------------------------
library(psych)
library(nlme)
library(car)
library(multcompView)
library(lsmeans)
library(tidyverse)
library(rcompanion)


# Load in data ------------------------------------------------------------
mc1.df <- read_csv("./D2018_MicroCol_Round1_Clean.csv")
mc1.feed.df <- read_csv("./D2018_MicroCol_Round1_Feed_Clean.csv")
mc1.df$treatment <- factor(mc1.df$treatment)

# Repeated measures analysis ----------------------------------------------
corr.model = lme(true_mc_mass ~ treatment + date + treatment*date, 
                 random = ~ 1 | id,
                 data = mc1.df,
                 na.action = na.exclude)

ACF(corr.model) # Lag 1: 0.314

model.1 = lme(true_mc_mass ~ treatment + date + treatment*date, 
              random = ~ 1 | id,
              data = mc1.df,
              correlation = corAR1(form = ~ date | id,
                                   value = 0.314),
              na.action = na.exclude,
              method = "REML")
Anova(model.1)

model.1.fixed = gls(true_mc_mass ~ treatment + date + treatment*date,
                    data = mc1.df,
                    method="REML",
                    na.action = na.exclude)

anova(model.1, model.1.fixed)

plot(model.1)
