


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("scales")
library("hms") # hms, for times
library("lubridate") # lubridate, for date/times
library("vctrs")

library("readxl") # readxl, for .xls and .xlsx files
library("haven") # haven, for SPSS, SAS and Stata files

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("forecast") # for auto.arima


#****************************************************************************************************
#                functions ####
#****************************************************************************************************
ma <- function(x, period) {
  # create trailing moving average of x of length period
  zoo::rollapply(x, period, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")
}


#****************************************************************************************************
#                get data ####
#****************************************************************************************************
dir <- "D:/Dropbox/DSRIP TSA_CA Team Info/Interim Report Documentation Compiled/statewide analysis by Erika/"
fn <- "2019-07-16 Statewide Time Series.xlsx"
sheet <- "monthlyvalues071419"

df <- read_excel(paste0(dir, fn), sheet=sheet)
df
df <- df %>%
  rename_all(str_to_lower) 
df


#****************************************************************************************************
#                analysis ####
#****************************************************************************************************
# create variables I want with additional months at beginning that I will drop after creating ma variables
df2 <- bind_rows(tibble(time1=-10:0), # add 11 prior values so we can compute ma for the first value in the data
                 df) %>%
  mutate(dsrip=ifelse(time1 >= 14, 1, 0),
         trend=time1 - 13,
         anomaly=ifelse(time1 >= 19, 1, 0),
         joint=trend * dsrip,
         trend_ma=ma(trend, 12),
         dsrip_ma=ma(dsrip, 12),
         joint_ma=ma(joint, 12)) %>%
  filter(time1 >= 1)
df2

df2 %>%
  ggplot(aes(time1, amrres)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 13.5, linetype="dashed") +
  geom_hline(yintercept=df2$amrres[df2$time1==13])

mod1 <- lm(amrres ~ time1 + dsrip + I(dsrip * time1), data=df2)
summary(mod1)
Acf(residuals(mod1))
Pacf(residuals(mod1))

mod2 <- lm(amrres ~ trend + dsrip + joint, data=df2)
summary(mod2)

mod3 <- lm(amrres ~ trend_ma + dsrip_ma + joint_ma, data=df2)
summary(mod3)
Acf(residuals(mod3))
Pacf(residuals(mod3))

mod3a <- lm(amrres ~ trend_ma + dsrip_ma + joint_ma + anomaly, data=df2)
summary(mod3a)
Acf(residuals(mod3a))
Pacf(residuals(mod3a))


xvars <- as.matrix(df2[, c("trend_ma", "dsrip_ma", "joint_ma")])
mod4 <- arima(df2$amrres, xreg=xvars, order=c(1,0,0))
summary(mod4)
Acf(residuals(mod4))
Pacf(residuals(mod4))

xvars2 <- as.matrix(df2[, c("trend_ma", "dsrip_ma", "joint_ma", "anomaly")])
mod4a <- arima(df2$amrres, xreg=xvars2, order=c(1,0,0))
summary(mod4a)
Acf(residuals(mod4a))
Pacf(residuals(mod4a))

mod5 <- auto.arima(df2$amrres, xreg=xvars)
summary(mod5)
Acf(residuals(mod5))
Pacf(residuals(mod5))

effects <- tibble(time=1:24) %>%
  mutate(emod1=coef(mod1)["dsrip"] + coef(mod1)["I(dsrip * time1)"] * (time + 13),
         emod2=coef(mod2)["dsrip"] + coef(mod2)["joint"] * time,
         emod3=coef(mod3)["dsrip_ma"] + coef(mod3)["joint_ma"] * time,
         emod3a=coef(mod3a)["dsrip_ma"] + coef(mod3a)["joint_ma"] * time,
         emod4=coef(mod4)["dsrip_ma"] + coef(mod4)["joint_ma"] * time,
         emod4a=coef(mod4a)["dsrip_ma"] + coef(mod4a)["joint_ma"] * time,
         emod5=coef(mod5)["dsrip_ma"] + coef(mod4)["joint_ma"] * time)
effects

effects %>%
  select(-emod2, -emod5) %>%
  gather(variable, value, -time) %>%
  ggplot(aes(time, value, colour=variable)) +
  geom_line() +
  geom_point() 


#****************************************************************************************************
#                what is going on with amrres?? ####
#****************************************************************************************************
dir <- "D:/Dropbox/DSRIP TSA_CA Team Info/Interim Report Documentation Compiled/DOH analysis of PPV and PPVBH/Data_DOH PPV runs/"
fn <- "msr_mo_pps.csv"
pps <- read_csv(paste0(dir, fn))
glimpse(pps)

amr <- pps %>%
  filter(Msr_Result_ID=="AMRRES")
count(amr, treat)
count(amr, new_date)
count(amr, PPS_ID, PPS_Short_Name, PPS_Name)

amr2 <- amr %>%
  mutate(date=as.Date(new_date, "%m/%d/%Y"))
# count(amr2, date, new_date)

amr2 %>%
  group_by(date, treat) %>%
  summarise(date_cnt=sum(date_cnt)) %>%
  spread(treat, date_cnt)

amr2 %>%
  group_by(date) %>%
  summarise(date_cnt=sum(date_cnt)) %>%
  ggplot(aes(date, date_cnt)) + geom_line() + geom_point()

amr2 %>%
  group_by(date, treat) %>%
  summarise(members=sum(members)) %>%
  spread(treat, members)

amr2 %>%
  group_by(date) %>%
  summarise(result=median(result)) %>%
  ggplot(aes(date, result)) + geom_line() + geom_point()

amr2 %>%
  ggplot(aes(date, result)) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept=as.Date("2015-07-31")) + # DSRIP start?
  geom_vline(xintercept=as.Date("2015-12-31"), linetype="dashed") + # the 19th month, which is when the statewide upward shift occurred
  facet_wrap(~PPS_Short_Name, ncol=5)

# how strange is that change?
chgdat <- amr2 %>%
  group_by(PPS_ID, PPS_Short_Name, PPS_Name) %>%
  arrange(date) %>%
  mutate(change=result - lag(result)) %>%
  ungroup
linedat <- chgdat %>% filter(date==as.Date("2015-12-31"))

chgdat %>%
  ggplot(aes(date, change)) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept=as.Date("2015-07-31")) + # DSRIP start?
  geom_vline(xintercept=as.Date("2015-12-31"), linetype="dashed") + # the 19th month, which is when the statewide upward shift occurred
  geom_hline(yintercept=0) +
  # scale_y_continuous(limits=c(-5, 5)) +
  facet_wrap(~PPS_Short_Name, ncol=5) +
  geom_hline(aes(yintercept=change), data=linedat, linetype="dashed")

# which are the big pps's?
amr2 %>%
  filter(date %in% as.Date(c("2015-09-30", "2015-12-31"))) %>%
  select(date, PPS_ID, PPS_Short_Name, PPS_Name, members, result) %>%
  arrange(PPS_Short_Name, date) %>%
  group_by(PPS_Short_Name) %>%
  mutate(mem_pch=members / lag(members) * 100 - 100,
         result_chg=result - lag(result)) %>%
  ungroup %>%
  filter(date==as.Date("2015-12-31"))


