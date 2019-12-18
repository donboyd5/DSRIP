



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
library("haven")

library("grDevices")
library("knitr")
library("kableExtra")

library("zoo") # for rollapply

library("forecast") # for auto.arima

library("masteringmetrics")

#****************************************************************************************************
#                functions ####
#****************************************************************************************************
# https://jrnold.github.io/masteringmetrics/rand-health-insurance-experiment-hie.html

# Function to calculate clustered standard errors and return a tidy data frame of the coefficients and standard errors.

ns <- function(df) {names(df) %>% sort } # names sort

cluster_se <- function(mod, cluster, type = "CR2") {
  vcov <- vcovCR(mod, cluster = cluster, type = type)
  coef_test(mod, vcov = vcov) %>%
    rownames_to_column(var = "term") %>%
    as_tibble() %>%
    select(term, estimate = beta, std.error = SE)
}

#****************************************************************************************************
#                analysis ####
#****************************************************************************************************
# https://jrnold.github.io/masteringmetrics/
data(package = "masteringmetrics")

#.. chapter 1 NHIS National Health Interview Survey ----
data("NHIS2009", package = "masteringmetrics")
glimpse(NHIS2009)
ns(NHIS2009)

count(NHIS2009, marradult)
count(NHIS2009, year)
count(NHIS2009, sex, fml)
count(NHIS2009, uninsured)
count(NHIS2009, hlth)
count(NHIS2009, empstat)
count(NHIS2009, age, age2)
count(NHIS2009, adltempl)
count(NHIS2009, uninsured)

tmp <- NHIS2009 %>% filter(serial==3)

# create table 1.1 basic characteristics of insured and uninsured couples
# couples, aged 26-59, at least one working
df <- NHIS2009 %>%
  filter(marradult, age %in% 25:59, adltempl >= 1) %>%
  group_by(serial) %>%
  filter(n()==2) %>%
  ungroup %>%
  arrange(serial, pernum) %>%
  select(serial, pernum, perweight, age, educ, empl, empstat, adltempl, famsize, fml, hi, hi_hsb1, uninsured, health)

glimpse(df)
summary(df[, ns(df)])

df %>%
  filter(!fml) %>%
  group_by(uninsured) %>%
  summarise(health=sum(health * perweight) / sum(perweight))


# arnold
arn <- NHIS2009 %>%
  filter(marradult, perweight != 0) %>%
  group_by(serial) %>%
  mutate(hi_hsb = mean(hi_hsb1, na.rm = TRUE)) %>%
  filter(!is.na(hi_hsb), !is.na(hi)) %>%
  mutate(female = sum(fml)) %>%
  filter(female == 1) %>%
  select(-female) %>%
  filter(between(age, 26, 59),
         marradult, adltempl >= 1) %>%
  group_by(serial) %>%
  filter(length(serial) > 1L) %>%
  ungroup()

arn %>%
  group_by(fml) %>%
  # normalize person weights to match number of observations in each
  # group
  mutate(perweight = perweight / sum(perweight) * n()) %>%
  group_by(fml, hi) %>%
  summarise(n_wt = sum(perweight)) %>%
  group_by(fml) %>%
  mutate(prop = n_wt / sum(n_wt))

 varlist <- c("hlth", "nwhite", "age", "yedu", "famsize", "empl", "inc")
 arn_diff <- arn %>%
  # rlang::set_attrs with NULL removes attributes from columns.
  # this avoids a warning from gather about differing attributes
  map_dfc(~ rlang::set_attrs(.x, NULL)) %>%
  select(fml, hi, one_of(varlist)) %>%
  gather(variable, value, -fml, -hi) %>%
  group_by(fml, hi, variable) %>%
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE)) %>%
  gather(stat, value, -fml, -hi, -variable) %>%
  unite(stat_hi, stat, hi) %>%
  spread(stat_hi, value) %>%
  mutate(diff = mean_1 - mean_0)
kable(arn_diff, digits = 3) %>% kable_styling()


#.. chapter 3 Minneapolis domestic violence experiment ----

data("mdve", package = "masteringmetrics")
glimpse(mdve)
mdve <- mutate(mdve,
               assigned = case_when(
                 T_RANDOM == 1 ~ "Arrest",
                 T_RANDOM == 2 ~ "Advise",
                 T_RANDOM == 3 ~ "Separate"
               ),
               outcome = case_when(
                 T_FINAL == 1 ~ "Arrest",
                 T_FINAL == 2 ~ "Advise",
                 T_FINAL == 3 ~ "Separate",
                 T_FINAL == 4 ~ "Other"
               ),
               coddled_a = assigned != "Arrest",
               coddled_o = outcome != "Arrest") %>%
  filter(outcome != "Other")
glimpse(mdve)


#****************************************************************************************************
#                Data ####
#****************************************************************************************************
# http://economics.mit.edu/faculty/angrist/data1
# http://economics.mit.edu/faculty/angrist/data1/data
# https://dataverse.harvard.edu/dataverse/JAngrist
# http://economics.mit.edu/faculty/angrist/data1/mhe

# http://www.masteringmetrics.com/resources/ # this is key - use it to get data
# http://www.masteringmetrics.com/sample-page/instructors-corner/

# http://masteringmetrics.com/wp-content/uploads/2014/12/Ec220-Pischke-Reading-List1.pdf
# http://masteringmetrics.com/wp-content/uploads/2014/12/Pischke-PS-for-MM.pdf
# http://masteringmetrics.com/wp-content/uploads/2015/02/bm.dta
# http://masteringmetrics.com/wp-content/uploads/2015/02/cps.dta
# http://masteringmetrics.com/wp-content/uploads/2015/02/ajr.dta
# http://masteringmetrics.com/wp-content/uploads/2015/02/minwage.dta


# Also see: https://jrnold.github.io/masteringmetrics/
# https://github.com/jrnold/masteringmetrics


#****************************************************************************************************
#                Get data files ####
#****************************************************************************************************
# Get data from here
# https://github.com/jrnold/masteringmetrics
# devtools::install_github("jrnold/masteringmetrics", subdir = "masteringmetrics") # empty line for updates
data(package = "masteringmetrics")
data("NHIS2009", package = "masteringmetrics")
glimpse(NHIS2009)

load(file="./mm/rdata/ak91.rda")
readRDS(file="./mm/rdata/ak91.rda")
load(file="https://github.com/jrnold/masteringmetrics/blob/master/masteringmetrics/data/ak91.rda")

# https://github.com/jrnold/masteringmetrics/blob/master/masteringmetrics/data/ak91.rda


# alternatively get data from the mm website

bmu <- "http://masteringmetrics.com/wp-content/uploads/2015/02/bm.dta" # Bertrand Mullainathan
bm <- read_dta(bmu)
glimpse(bm)
comment(bm) <- "Bertrand Mullainathan"

cpsu <- "http://masteringmetrics.com/wp-content/uploads/2015/02/cps.dta" # 
cps <- read_dta(cpsu)
glimpse(cps)

ajru <- "http://masteringmetrics.com/wp-content/uploads/2015/02/ajr.dta" # 
ajr <- read_dta(ajru)
comment(ajr) <- "Acemoglu, Johnson, Robinson"
glimpse(ajr)

minwageu <- "http://masteringmetrics.com/wp-content/uploads/2015/02/minwage.dta" # 
minwage <- read_dta(minwageu)
glimpse(minwage)


# RAND HIE ----
library("tidyverse")
library("broom")
library("haven")
library("rlang")
library("clubSandwich")

data("rand_sample", package = "masteringmetrics")
glimpse(rand_sample)






