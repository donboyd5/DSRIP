

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
#                SPARCS ####
#****************************************************************************************************
sparcsd <- "D:/Data/DSRIP/NYSDOH_HospitalInpatientDischarges_SPARCS_De-Identified_2014/"
hifn <- "NYSDOH_HospitalInpatientDischarges_SPARCS_De-Identified_2014.csv"

df <- read_csv(paste0(sparcsd, hifn))

glimpse(df)

count(df, `Emergency Department Indicator`)

count(df, `APR MDC Code`, `APR MDC Description`) %>%
  arrange(-n)

df %>%
  summarise(charges=sum(`Total Charges`), 
            costs=sum(`Total Costs`),
            ratio_mdn=median(`Ratio of Total Costs to Total Charges`)) %>%
  mutate(r2=costs / charges)
# A tibble: 1 x 4
# charges        costs ratio_mdn    r2
# <dbl>        <dbl>     <dbl> <dbl>
#   1 95146813935. 32289724919.     0.344 0.339


#****************************************************************************************************
#                Summary data ####
#****************************************************************************************************
# Medicaid Potentially Preventable Emergency Visit (PPV) Rates by Patient County: Beginning 2011
# https://health.data.ny.gov/Health/Medicaid-Potentially-Preventable-Emergency-Visit-P/cr7a-34ka/data
dd <- "D:/Data/DSRIP/"
fn <- "Medicaid_Potentially_Preventable_Emergency_Visit__PPV__Rates_by_Patient_County__Beginning_2011.csv"

df <- read_csv(paste0(dd, fn))

glimpse(df)

vnames <- c("year", "county", "events", "pop", "observed", "expected", "riskadj", "diff", "dualstatus")

df2 <- df %>%
  setNames(vnames) %>%
  mutate(dualstatus=ifelse(str_detect(dualstatus, "Non"), "nondual", dualstatus))
glimpse(df2)
count(df2, dualstatus)
count(df2, county)
count(df2, year)

df2 %>%
  filter(dualstatus=="Total") %>%
  ggplot(aes(year, observed)) +
  geom_line() +
  facet_wrap(~county, ncol=6, scales="free_y") 


fn <- "All_Payer_Potentially_Preventable_Emergency_Visit__PPV__Rates_by_Patient_County__SPARCS___Beginning_2011.csv"
df <- read_csv(paste0(dd, fn))

glimpse(df)

vnames <- c("year", "version", "county", "observed", "expected", "riskadj", "diff")

df2 <- df %>%
  setNames(vnames)
glimpse(df2)
count(df2, county)
count(df2, year)

df2 %>%
  ggplot(aes(year, observed)) +
  geom_line() +
  facet_wrap(~county, ncol=6, scales="free_y") 






