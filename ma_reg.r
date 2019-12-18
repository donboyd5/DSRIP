

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
#                ma data ####
#****************************************************************************************************

# create some data
# y = b0 + b1*time + b0p*policy + b1p*policy*time + err
time <- -36:24
t2 <- 1:length(time)
b0test <- 5
b1test <- 1

b0testp <- 2 # policy intercept shift
b1testp <- .5 # policy slope shift

set.seed(1234)
df <- tibble(time=time,
             t2=t2,
             policy=ifelse(time > 0, 1, 0),
             shift_effect=b0testp * policy,
             slope_effect=b1testp * policy * time,
             err=rnorm(length(time))*1.5) %>%
  mutate(err=err,
         y = b0test + b1test*time + shift_effect + slope_effect + err,
         y_ma=ma(y, 12),
         policy_ma=ma(policy, 12),
         phase_in=ifelse(time %in% 1:11, 1, 0),
         phase_in2=ifelse(time %in% 1:11, policy_ma, 0),
         post_phase=ifelse(time>=12, 1, 0),
         time_ma=ma(time, 12),
         joint_ma=ma(time * policy, 12))

df %>%
  gather(variable, value, y, y_ma) %>%
  ggplot(aes(time, value, color=variable)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept=0.5, linetype="dashed")

df2 <- df %>% filter(time %in% -12:24)

mod_true <- lm(y ~ time + policy + I(policy*time), data=df2)
mod_t2 <- lm(y ~ t2 + policy + I(policy*t2), data=df2)
mod_yma <- lm(y_ma ~ time + policy + I(policy*time), data=df2)
mod_ypma <- lm(y_ma ~ time + policy_ma + I(policy_ma*time), data=df2)
mod_pieces <- lm(y_ma ~ time + phase_in + I(phase_in * time) + post_phase + I(post_phase* time), data=df2)
mod_pieces2 <- lm(y_ma ~ time + phase_in2 + I(phase_in2 * time) + post_phase + I(post_phase* time), data=df2)
mod_pieces2a <- lm(y_ma ~ time + phase_in2 + I(phase_in2 * time) + post_phase + I(post_phase* (time - 11)), data=df2)

mod_hamp <-  lm(y_ma ~ time_ma + policy_ma + joint_ma, data=df2)

library(forecast)  
xvars <- as.matrix(df2[, c("time_ma", "policy_ma", "joint_ma")])

b0testp; b1testp # reminder of the policy effects
fit1 <- arima(df2$y_ma, xreg=xvars, order=c(12,0,0))
fit1 <- arima(df2$y_ma, xreg=xvars, order=c(0,0,4))
summary(fit1); fit1$nobs

mod_ar <- arima(df2$y_ma, xreg=xvars, order=c(1,0,0))
summary(mod_ar)
coef(mod_ar)

fit3 <- auto.arima(df2$y_ma, xreg=xvars)
summary(fit3)
coef(fit3)
names(fit3)
fit3$nobs


mod_trueall <- lm(y ~ time + policy + I(policy*time), data=df)
summary(mod_trueall)
summary(mod_yma)
summary(mod_hamp)
mod_ar

coef(mod_trueall)[1]
f <- function(mod, time){
  coef(mod)[3] + coef(mod)[4]*time
}

# get predictions of the dsrip effect from true, hamp, and arima
b0testp; b1testp # reminder of the policy effects
pred <- tibble(time=1:24) %>%
  mutate(p.params=b0testp + b1testp * time,
         p.true=f(mod_trueall, time),
         p.erika=f(mod_yma, time),
         p.hamp=f(mod_hamp, time),
         p.ar=coef(mod_ar)[4] + coef(mod_ar)[5]*time)
pred

pred %>%
  gather(variable, value, -time) %>%
  mutate(value=value / df2$y[df2$time==0] * 100) %>%
  ggplot(aes(time, value, colour=variable)) +
  geom_line() +
  geom_point() +
  ggtitle("Effect of policy intervention estimated by several approaches")


summary(mod_yma)
summary(mod_ypma)
summary(mod_pieces)
summary(mod_pieces2)


b0testp; b1testp # reminder of the policy effects
summary(mod_true)



summary(mod_t2)
summary(mod_yma)
summary(mod_ypma)
summary(mod_pieces)
summary(mod_pieces2)
summary(mod_pieces2a)

summary(mod_trueall)
summary(mod_yma)
summary(mod_hamp)

library("car")
dwt(mod_trueall)
dwt(mod_hamp)

# https://www.econometrics-with-r.org/15-4-hac-standard-errors.html
library(sandwich)
library(lmtest)

N <- nrow(df)
m <- floor(0.75 * N^(1/3))
NW_VCOV <- NeweyWest(mod_trueall, 
                     lag = m - 1, prewhite = FALSE, 
                     adjust = TRUE)
NW_VCOV

# rule of thumb truncation parameter
N <- nrow(df2)
m <- floor(0.75 * N^(1/3))
NW_VCOV <- NeweyWest(mod_hamp, 
                     lag = m - 1, prewhite = FALSE, 
                     adjust = TRUE)
NW_VCOV
coeftest(mod_hamp, vcov = NW_VCOV)
summary(mod_hamp)

N <- nrow(df2)
m <- floor(0.75 * N^(1/3))
NW_VCOV <- NeweyWest(mod_yma, 
                     lag = m - 1, prewhite = FALSE, 
                     adjust = TRUE)
coeftest(mod_yma, vcov = NW_VCOV)




df3 <- df2 %>%
  mutate(yfit=predict(mod_true),
         ymafit=predict(mod_yma),
         ypmafit=predict(mod_ypma),
         ypieces=predict(mod_pieces),
         ypieces2=predict(mod_pieces2),
         ypieces2a=predict(mod_pieces2a),
         yhamp=predict(mod_hamp))

df3 %>%
  gather(variable, value, starts_with("y")) %>%
  # filter(!variable %in% c("y", "yfit", "ypieces2")) %>%
  # filter(variable=="y_ma" | str_detect(variable, "pieces")) %>%
  filter(variable %in% c("y_ma", "ymafit", "yhamp")) %>%
  filter(time %in% -12:24) %>%
  ggplot(aes(time, value, color=variable)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept=0.5, linetype="dashed") +
  geom_vline(xintercept=11.5, linetype="dashed") +
  scale_x_continuous(breaks=seq(-100, 100, 2)) +
  scale_y_continuous(breaks=seq(-20, 100, 5))


coef(mod_true)




# fit with shift only
mod.shift <- lm(y ~ time + policy, data=df2)
mod_yma.shift <- lm(y_ma ~ time + policy, data=df2)
mod_ypma.shift <- lm(y_ma ~ time + policy_ma, data=df2)

mod.slope <- lm(y ~ time + I(policy*time), data=df2)
mod_yma.slope <- lm(y_ma ~ time + I(policy*time), data=df2)
mod_ypma.slope <- lm(y_ma ~ time + I(policy_ma*time), data=df2)

summary(mod.shift)
summary(mod_yma.shift)
summary(mod_ypma.shift)

summary(mod.slope)
summary(mod_yma.slope)
summary(mod_ypma.slope)

