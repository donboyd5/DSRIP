
# Examine Erika's table ----


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


#****************************************************************************************************
#                data ####
#****************************************************************************************************
dir <- "C:/Users/donbo/Downloads/"
fn <- "2019-09-11 Possible DSRIP outcome measures_djb.xlsx"
sheet <- "MeasureMatrix"


df <- read_excel(paste0(dir, fn), sheet=sheet)
glimpse(df)

df2 <- df %>%
  slice(2:n()) %>%
  select(mid=TOC, mname=`...2`, freq=`...10`, npps=`...11`, mystart=`...13`, myend=`...14`,
         notes=`...15`, hyp1=`...27`, hyp2=`...28`, hyp3=`...29`, hyp4=`...30`)
glimpse(df2)
ht(df2)
count(df2, freq)
count(df2, freq, npps)

df3 <- df2 %>%
  gather(hyp, hypid, starts_with("hyp")) %>%
  filter(!is.na(hypid))
glimpse(df3)
ht(df3)

# create sort order
# hypsort <- count(df3, hypid)
# write_csv(hypsort, "d:/temp/hypsort.csv")
(hypsort <- read_csv("d:/temp/hypsort2.csv"))

unique(df3$mname)
gsub('"', '', df3$mname)

# monthly measures?
hypsort %>%
  select(hypid) %>%
  filter(hypid!="No associated hypothesis") %>%
  left_join(df3) %>% 
  filter(is.na(freq) | freq=="Monthly") %>%
  mutate(hypid=factor(hypid,levels=hypsort$hypid),
         mname=str_sub(mname, 1, 50) %>% str_trim(),
         mname=gsub('"', '', mname)) %>%
  group_by(hypid, freq) %>%
  mutate(nmeasures=sum(!is.na(npps))) %>%
  arrange(hypid, freq) %>%
  ungroup %>%
  select(hypid, mid, mname, npps, nmeasures)

# unique monthly measures
df3 %>% 
  filter(hypid!="No associated hypothesis") %>%
  filter(freq=="Monthly") %>%
  group_by(mid, mname) %>%
  summarise(n=n())
  unique()
  mutate(hypid=factor(hypid,levels=hypsort$hypid),
         mname=str_sub(mname, 1, 50) %>% str_trim(),
         mname=gsub('"', '', mname)) %>%
  group_by(hypid, freq) %>%
  mutate(nmeasures=sum(!is.na(npps))) %>%
  arrange(hypid, freq) %>%
  ungroup %>%
  select(hypid, mid, mname, npps, nmeasures)


