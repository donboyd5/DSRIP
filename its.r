



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
#                ITS ####
#****************************************************************************************************
# Interrupted time series regression for the evaluation of public health interventions: a tutorial
# James Lopez Bernal, Steven Cummins, Antonio Gasparrini

data <- read_csv("./data/its/sicily.csv")
ht(data)
data <- data %>%
  mutate(date=as.Date(paste0(year, "-", month, "-01")),
         rate=aces / stdpop * 10^5)
# View(data)
summary(data)

data %>% 
  ggplot(aes(date, rate)) +
  geom_point() +
  geom_vline(xintercept = as.Date("2004-12-15"))

# This dataset includes the following variables:
# year
# month
# time = elapsed time since the start of the study
# aces = count of acute coronary episodes in Sicily per month (the outcome)
# smokban = smoking ban (the intervention) coded 0 before intervention, 1 after
# pop = the population of Sicily (in 10000s)
# stdpop =  age standardised population

mod <- lm(rate ~ time + smokban, data=data)
summary(mod)




data <- data %>%
  mutate(rate12=ma(rate, 12), sb12=ma(smokban, 12))
mod12 <- lm(rate12 ~ time + sb12, data=data)
mod12x <- lm(rate12 ~ time + smokban, data=data)

summary(mod)
summary(mod12)
summary(mod12x)
# mod12 coeff looks pretty good, tval improves, why?? is this an issue
# mod12x coeff magnitude is way too small - this is what Erika etc did



