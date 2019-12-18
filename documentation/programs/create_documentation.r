

#****************************************************************************************************
#                libraries ####
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

library("grDevices")
library("knitr")
library("kableExtra")


#****************************************************************************************************
#                globals ####
#****************************************************************************************************
dbox <- "C:/Users/donbo/Dropbox/"
pps_dir <- paste0(dbox, "DSRIP TSA_CA Team Info/Interim Report Documentation Compiled/DOH analysis of PPV and PPVBH/Data_DOH PPV runs/")

pps_fn <- "msr_mo_pps.csv"


#****************************************************************************************************
#                data ####
#****************************************************************************************************
pps <- read_csv(paste0(pps_dir, pps_fn))
glimpse(pps)


# pps names
count(pps, PPS_ID, PPS_Name, PPS_Short_Name)
pps %>%
  select(PPS_ID, PPS_Short_Name, PPS_Name) %>%
  unique %>%
  write_csv("./documentation/finished_files/ppsnames.csv")

# pps measures
pps %>%
  select(Msr_Result_ID, Msr_Result_Name, Unit_Lbl, UOM) %>%
  unique %>%
  write_csv("./documentation/finished_files/measures.csv")


count(pps, Msr_Result_ID, Msr_Result_Name, Unit_Lbl)

count(pps, PPS_ID, PPS_Name, PPS_Short_Name)
