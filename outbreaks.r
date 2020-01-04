

# outliers and structural breaks


#****************************************************************************************************
#                Globals ####
#****************************************************************************************************

team_dir <- "C:/Users/donbo/Dropbox/DSRIP TSA_CA Team Info/"

chris_source_dir <- paste0(team_dir, "CMA Files/dsrip/chris_workflow/data/") # not used now

don_dir <- paste0(team_dir, "Don's Files/data/don/")
chris_dir <- paste0(team_dir, "Don's Files/data/chris_frozen/")


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

library("grDevices")
library("knitr")
library("kableExtra")

library("btools")

# library("DT")

library("zoo") # for rollapply

library("broom")
library("forecast") # for auto.arima
library("lmtest")

library("strucchange")

library("tsoutliers")

# devtools::install_github("business-science/anomalize")
library("anomalize")


#****************************************************************************************************
#                get data ####
#****************************************************************************************************

ppsdf <- readRDS(paste0(don_dir, "ppsall_ma.rds"))
ppsnames <- ppsdf %>% select(PPS_ID, PPS_NAME) %>% unique %>% mutate(idname=paste0(str_pad(PPS_ID, 2, side="left", "0"), " ", PPS_NAME))

measures <- readRDS(paste0(don_dir, "measures.rds"))
measures

# make a sample of smooth, breaks, outliers and other data
defs <- tribble(
  ~type, ~MSR_RESULT_ID, ~PPS_ID,
  "smooth", "AAPC20RES", 25,
  "smooth", "CAPC12MRES", 14,
  "smooth", "PDI14RES", 36,
  "break", "AMRRES", 40, # 1 break
  "break", "PPVRES", 33,
  "break-sharpfall", "CAPC12RES", 36,
  "outliers2", "AMMACUTRES", 48,
  "break", "AAPC20RES", 16,
  "break", "AAPC20RES", 19,
  "break", "AAPC45RES", 34,
  "break", "AMRRES", 52
) %>%
  mutate(groupnum=row_number()) %>%
  left_join(measures %>% select(MSR_RESULT_ID, MSR_RESULT_NAME)) %>%
  left_join(ppsnames)
defs

samp <- ppsdf %>% 
  select(MSR_RESULT_ID, PPS_ID, PER_END_DT, time, rownum, MSR_RESULT) %>%
  right_join(defs) %>%
  select(groupnum, type, MSR_RESULT_ID, PPS_ID, idname, PER_END_DT, time, rownum, MSR_RESULT) %>%
  mutate(gtype=paste0(groupnum, "-", type))
samp

brks <- c(seq(0, -30, -3) %>% rev, seq(3, 60, 3))
samp %>%
  ggplot(aes(time, MSR_RESULT)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=brks) +
  geom_vline(xintercept = 1, colour="blue", linetype="solid") +
  geom_vline(xintercept = 12, colour="blue", linetype="dashed") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
  theme_bw() +
  facet_wrap(~gtype + MSR_RESULT_ID + idname, ncol=3, scales = "free") +
  theme(legend.position = "none") +
  ggtitle("types")


#****************************************************************************************************
#                structural breaks ####
#****************************************************************************************************
# this seems to pick up smooth transitions, which is not what I want

data <- samp %>% filter(groupnum==4) %>% .[["MSR_RESULT"]]
bvals <- breakpoints(data ~ 1)
str(bvals)
bvals$breakpoints

f <- function(gnum){
  # plot breakpoints
  pdata <- samp %>% filter(groupnum==gnum)
  
  ## compute breakdates corresponding to the
  ## breakpoints of minimum BIC segmentation
  bvals <- breakpoints(pdata$MSR_RESULT ~ 1, h=12, breaks=3)
  
  brks <- c(seq(0, -30, -3) %>% rev, seq(3, 60, 3))
  
  p <- pdata %>%
    ggplot(aes(time, MSR_RESULT)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks=brks) +
    geom_vline(xintercept = 1, colour="blue", linetype="solid") +
    geom_vline(xintercept = 12, colour="blue", linetype="dashed") +
    geom_vline(xintercept=bvals$breakpoints - 13, colour="red") +
    theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
    theme_bw()
  return(p)
}

f(1)
f(2)
f(3)
f(4)
f(5)
f(6)
f(7)


#****************************************************************************************************
#                changepoint analysis ####
#****************************************************************************************************
library(changepoint)
# A changepoint is denoted as the first observation of the new segment / regime.

set.seed(1)
x <- c(rnorm(50,0,1),
       rnorm(50,5,1),
       rnorm(50,10,1),
       rnorm(50,3,1))
cpt.mean(x, penalty="SIC", method="BinSeg", Q=5, class=FALSE)
cpt.mean(x, penalty="BIC", method="BinSeg", Q=5, class=FALSE)
cpt.mean(x, penalty="MBIC", method="BinSeg", Q=5, class=FALSE)
cpt.mean(x, penalty="AIC", method="BinSeg", Q=5, class=FALSE)
cpt.mean(x, penalty="Hannan-Quinn", method="BinSeg", Q=5, class=FALSE)
plot(x)
(tmp <- cpt.mean(x, penalty="Manual", pen.value="2*log(n)", method="BinSeg", Q=5, class=FALSE))
plot(tmp)
# returns optimal number of changepoints is 3, locations are 50,100,150.

# Example of using the CROPS penalty in data set above
set.seed(1)
x=c(rnorm(50,0,1),rnorm(50,5,1),rnorm(50,10,1),rnorm(50,3,1))
plot(x)
out=cpt.mean(x, pen.value = c(4,1500),penalty = "CROPS",method = "PELT") 
cpts.full(out)  # returns 7 segmentations for penalty values between 4 and 1500.  
# We find segmentations with 7, 5, 4, 3, 2, 1 and 0 changepoints. 
# Note that the empty final row indicates no changepoints.
pen.value.full(out) # gives associated penalty transition points
# CROPS does not give an optimal set of changepoints thus we may wish to explore further
plot(out,diagnostic=TRUE) 
# looks like the segmentation with 3 changepoints, 50,100,150 is the most appropriate
plot(out,ncpts=3) 


data <- samp %>% filter(groupnum==4) %>% .[["MSR_RESULT"]]
plot(data)
cpt.mean(data, class=FALSE)
ansmean <- cpt.mean(data)
ansmean
str(ansmean)
ansmean@cpts
plot(ansmean, cpt.col='blue')
cpt.mean(data, penalty="Manual", pen.value="2*log(n)", method="BinSeg", Q=4, minseglen=12, class=TRUE)
vals <- cpt.mean(data, penalty="Manual", pen.value="2*log(n)", method="BinSeg", Q=4, minseglen=12, class=FALSE)
(vals <- cpt.mean(data, penalty="SIC", method="BinSeg", Q=5, minseglen=6, class=FALSE))
(vals <- cpt.mean(data, penalty="Asymptotic", pen.value=.01, method="BinSeg", Q=5, minseglen=6, class=FALSE))
(vals <- cpt.mean(data, penalty="Manual", pen.value="2*log(n)", method="BinSeg", Q=2, class=FALSE))
(ll <- cpt.mean(data, penalty="Manual", pen.value="2*log(n)", method="BinSeg", Q=2, class=TRUE))
str(ll)
samp %>% 
  filter(groupnum==4) %>%
  ggplot(aes(time, MSR_RESULT)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=brks) +
  geom_vline(xintercept = 1, colour="blue", linetype="solid") +
  geom_vline(xintercept = 12, colour="blue", linetype="dashed") +
  geom_vline(xintercept=vals - 13, colour="red") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
  theme_bw()


#****************************************************************************************************
#                outliers level shift analysis ####
#****************************************************************************************************
# 4 and 5 are unacceptable others maybe ok

# An Additive Outlier (AO) represents an isolated spike.
# A Level Shift (LS) represents an abrupt change in the mean level and it may be seasonal (Seasonal Level Shift, SLS) or not.
# A Transient Change (TC) represents a spike that takes a few periods to disappear.
# An Intervention Outlier (IO) represents a shock in the innovations of the model.

df <- samp %>% filter(groupnum==4); data <- df %>% .[["MSR_RESULT"]]
t2 <- ts(data, frequency=1)
plot(t2)
df %>% ggplot(aes(rownum, MSR_RESULT)) + geom_line() + geom_point() + scale_x_continuous(breaks=seq(0, 80, 5))

# innovational outliers ("IO")
# additive outliers ("AO")
# level shifts ("LS")
# temporary changes ("TC")
# seasonal level shifts ("SLS").

out <- tso(t2, 
    types = c("AO", "LS", "TC", "IO"), # c("AO", "LS", "TC", "IO"),
    cval = 2.5, # lower is more sensitive to AO outliers
    delta = 0.1,
    maxit = 1,
    maxit.iloop = 6, maxit.oloop = 6,
    tsmethod="arima", 
    args.tsmethod = list(order=c(1, 1, 0)))

worst <- function(t2){
  critval <- NULL # default
  critval <- 2.75 # lower is more sensitive -- under 3 seems useful
  dval <- 0.7 # default
  # dval <- 0.3
  aom <- tso(t2, types = "AO", cval = critval, delta = dval, maxit = 1, maxit.iloop = 10, maxit.oloop = 6, tsmethod="arima",  args.tsmethod = list(order=c(1, 0, 0)))
  iom <- tso(t2, types = "AO", cval = critval, delta = dval, maxit = 1, maxit.iloop = 10, maxit.oloop = 6, tsmethod="arima",  args.tsmethod = list(order=c(1, 0, 0)))
  lsm <- tso(t2, types = "AO", cval = critval, delta = dval, maxit = 1, maxit.iloop = 10, maxit.oloop = 6, tsmethod="arima",  args.tsmethod = list(order=c(1, 0, 0)))
  tcm <- tso(t2, types = "AO", cval = critval, delta = dval, maxit = 1, maxit.iloop = 10, maxit.oloop = 6, tsmethod="arima",  args.tsmethod = list(order=c(1, 0, 0)))
  outdf <- bind_rows(aom$outliers, iom$outliers, lsm$outliers, tcm$outliers)
  return(outdf)
}

# djb SUCCESS!! ----
# 4 and 5 are bad, 1:3, 6:7 ok
gnum <- 11; df <- samp %>% filter(groupnum==gnum); data <- df %>% .[["MSR_RESULT"]]; t2 <- ts(data, frequency=1)
df %>% ggplot(aes(rownum, MSR_RESULT)) + geom_line() + geom_point() + scale_x_continuous(breaks=seq(0, 80, 5))

w <- worst(t2)
count(w, ind)

df %>% ggplot(aes(rownum, MSR_RESULT)) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks=seq(0, 80, 5)) + 
  geom_vline(xintercept = unique(w$ind), colour="red")

critval <- 2.5 # lower is more sensitive
dval <- 0.7 # default

critval <- NULL # default
dval <- 0.7 # default
# ind is the start of a new level
tso(t2, types = "AO", cval = critval, delta = dval, maxit = 1, maxit.iloop = 10, maxit.oloop = 6, tsmethod="arima",  args.tsmethod = list(order=c(1, 0, 0)))
tso(t2, types = "IO", cval = critval, delta = dval, maxit = 1, maxit.iloop = 10, maxit.oloop = 6, tsmethod="arima",  args.tsmethod = list(order=c(1, 0, 0)))
tso(t2, types = "LS", cval = critval, delta = dval, maxit = 1, maxit.iloop = 10, maxit.oloop = 6, tsmethod="arima",  args.tsmethod = list(order=c(1, 0, 0)))
tso(t2, types = "TC", cval = critval, delta = dval, maxit = 1, maxit.iloop = 10, maxit.oloop = 6, tsmethod="arima",  args.tsmethod = list(order=c(1, 0, 0)))


# note that 19 is AO, IO, and LS but does not show as both if we do this (and we lose #12)
tso(t2, types = c("AO", "IO", "LS", "TC"), cval = critval, delta = dval, maxit = 1, maxit.iloop = 10, maxit.oloop = 6, tsmethod="arima",  args.tsmethod = list(order=c(1, 0, 0)))

out <- tso(t2, 
           # types = c("AO", "LS", "TC", "IO"), # c("AO", "LS", "TC", "IO"),
           types = c("IO"), # seems like innovation is what we want
           # cval = 2.5, # lower is more sensitive to AO outliers and other kinds
           delta = 0.1,
           maxit = 1,
           maxit.iloop = 6, maxit.oloop = 6,
           tsmethod="arima",
           args.tsmethod = list(order=c(1, 0, 0))
           )

str(out)
out$outliers

otype <- "IO"; vals <- out$outliers %>% filter(type==otype) %>% .$ind
samp %>% 
  filter(groupnum==4) %>%
  ggplot(aes(time, MSR_RESULT)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=brks) +
  geom_vline(xintercept = 1, colour="blue", linetype="solid") +
  geom_vline(xintercept = 12, colour="blue", linetype="dashed") +
  geom_vline(xintercept=vals - 13, colour="red") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
  theme_bw()

gnum <- 1
f <- function(gnum){
  # plot breakpoints
  pdata <- samp %>% filter(groupnum==gnum)
  t2 <- ts(pdata$MSR_RESULT, frequency=1)
  
  out <- tso(t2, 
             # types = c("AO", "LS", "TC", "IO"), # c("AO", "LS", "TC", "IO"),
             types = c("IO"), # seems like innovation is what we want
             # cval = 2.5, # lower is more sensitive to AO outliers and other kinds
             delta = 0.1,
             maxit = 1,
             maxit.iloop = 6, maxit.oloop = 6,
             tsmethod="arima",
             args.tsmethod = list(order=c(1, 0, 0))
             )
  
  bvals <- out$outliers %>% .$ind
  
  brks <- c(seq(0, -30, -3) %>% rev, seq(3, 60, 3))
  
  p <- pdata %>%
    ggplot(aes(time, MSR_RESULT)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks=brks) +
    geom_vline(xintercept = 1, colour="blue", linetype="solid") +
    geom_vline(xintercept = 12, colour="blue", linetype="dashed") +
    geom_vline(xintercept=bvals - 13, colour="red") +
    theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
    theme_bw()
  return(p)
}

f(1)
f(2)
f(3)
f(4)
f(5)
f(6)
f(7)
