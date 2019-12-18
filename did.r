

#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("scales")
library("hms") # hms, for times.
library("lubridate") # lubridate, for date/times.
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.
library("vctrs")
# library("precis") # use precis2 from btools

library("tibbletime") # https://business-science.github.io/tibbletime/

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("btools") # library that I created (install from github)
library("bdata")

#install.packages("lmtest") ; install.packages("Epi")
#install.packages("tsModel"); install.packages("vcd")


#****************************************************************************************************
#                functions ####
#****************************************************************************************************
ma <- function(x, period) {
  # note that this requires zoo, which is on the Depends line in the Description file
  zoo::rollapply(x, period, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")
}


#****************************************************************************************************
#                DID ####
#****************************************************************************************************
# https://www.princeton.edu/~otorres/DID101R.pdf

library(foreign)
mydata <- read.dta("http://dss.princeton.edu/training/Panel101.dta")

# Create a dummy variable to indicate the time when the treatment started.
mydata$time = ifelse(mydata$year >= 1994, 1, 0)

# Create a dummy variable to identify the group exposed to the treatment.
mydata$treated = ifelse(mydata$country == "E" |
                          mydata$country == "F" |
                          mydata$country == "G", 1, 0)

# Create an interaction between time and treated. We will call this interaction ‘did’.
mydata$did = mydata$time * mydata$treated
summary(mydata)

# Estimating the DID estimator
didreg = lm(y ~ treated + time + did, data = mydata)
summary(didreg)

# The coefficient for ‘did’ is the differences-in-differences estimator. 
# The effect is significant at 10% with the treatment having a negative effect.

# Estimating the DID estimator (using the multiplication method, no need to generate the interaction)
didreg1 = lm(y ~ treated*time, data = mydata)
summary(didreg1)


#****************************************************************************************************
#                DID my way ####
#****************************************************************************************************
df <- read_dta("http://dss.princeton.edu/training/Panel101.dta")
count(df, country)
# attributes(df$country)
# labelled(c("M", "M", "F"), c(Male = "M", Female = "F"))

df2 <- df %>%
  mutate(country=as_factor(country),
         treated_period=ifelse(year>=1994, 1, 0),
         treated_group=ifelse(country %in% c("E", "F", "G"), 1, 0),
         did=treated_period * treated_group)
summary(df2)

dreg = lm(y ~ treated_period + treated_group + did, data = df2)
summary(dreg)


#****************************************************************************************************
#                the did package ####
#****************************************************************************************************
# https://www.rdocumentation.org/packages/did/versions/1.2.2
library(did)
data(mpdta)
glimpse(mpdta)
out <- mp.spatt(lemp ~ treat, xformla=~lpop, data=mpdta,
                panel=TRUE, first.treat.name="first.treat",
                idname="countyreal", tname="year",
                bstrap=FALSE, se=TRUE, cband=FALSE)
summary(out)

library(gridExtra)
ggdid(out, ylim=c(-.25,.1))

summary(out$aggte, type="dynamic")
ggdid(out, type="dynamic", ylim=c(-.25,.1))


