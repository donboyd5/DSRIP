

# I think we count "2015-10-1" as start of DSRIP
# if so, that would be in MY2 - so we would compare pre MY2 to MY2+

# From interim report:
# Demonstration Years Measurement Years
# DY0 April 2014 – March 2015   MY0 June 2014
# DY1 April 2015 – March 2016   MY1 July 2014 – June 2015
# DY2 April 2016 – March 2017   MY2 July 2015 – June 2016
# DY3 April 2017 – March 2018   MY3 July 2016 – June 2017
# DY4 April 2018 – March 2019   MY4 July 2017 – June 2018
# DY5 April 2019 – March 2020   MY5 July 2018 – June 2019

# It is important to note the PPS project implementation timeframe compared to the measurement years. 
# The PPS applications and valuation awards were finalized in April 2015, the start of DY1, while MY2 
# began in July 2015 when much of the infrastructure for implementation was being initiated. Further, 
# the last measurement period of MY5 ends 9 months before the end of DY5 when implementation is still
# occurring. Because the DSRIP program is still ongoing and the data available on performance measures
# was limited to a small number of years before and after initiation of the DSRIP program, it is too 
# early to draw conclusions about the impact of the DSRIP program on quality, cost, service utilization, 
# and overall system transformation.

# And from Interim report p.62:
# 
# Following standard practice, this analysis considered the start of MY2 (July 2015) to be the first
# month of the post-DSRIP initiation period, with all prior months assigned to the pre-DSRIP
# initiation period. This provided 13 months of pre-DSRIP initiation measurement time and 24
# months of post-DSRIP initiation measurement time.70 Using the start of MY2 as the post-DSRIP
# initiation period, rather than selecting a month in the middle of a MY, also allowed for
# consistent time periods when evaluating monthly and annual measures. This decision was
# vetted with NYS DOH and is also consistent with findings from the implementation and process
# study, which identified delays in implementation times.  


# MSR_YR_NUM     n mean       min        max       
# <chr>      <int> <date>     <date>     <date>    
# 1 0           1927 2014-06-30 2014-06-30 2014-06-30
# 2 1           4744 2015-06-30 2015-06-30 2015-06-30  this is just the end date
# 3 2           4674 2016-06-30 2016-06-30 2016-06-30
# 4 3           4923 2017-06-30 2017-06-30 2017-06-30
# 5 4           4507 2018-06-30 2018-06-30 2018-06-30


#****************************************************************************************************
#                libraries ####
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

library("ggrepel")


#****************************************************************************************************
#                globals ####
#****************************************************************************************************
ddir <- "C:/Users/donbo/Downloads/shiny/shiny/" # location of Chris's Shiny data from 11/6/2019
ddir <- "C:/Users/donbo/Dropbox/RPrograms PC/ProjectsCurrent/DSRIP/data/ChrisShiny_2019-11-06/"

annwide_fn <- "v_mapp_pps_msr_ann.rds"
annlong_fn <- "v_mapp_pps_msr_ann_long.rds"

mwide_fn <- "V_mapp_pps_msr_mo.rds"
mlong_fn <- "V_mapp_pps_msr_mo_long.rds"

pps_fn <- "Boyd_PPS_characteristics.xlsx"


#****************************************************************************************************
#                functions ####
#****************************************************************************************************
ma <- function(x, period) {
  # note that this requires zoo, which is on the Depends line in the Description file
  zoo::rollapply(x, period, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")
}

ns <- function(df) {names(df) %>% sort}

ht <- function(df, nrecs=6) {head(df, nrecs); tail(df, nrecs)} 


#****************************************************************************************************
#                Create a df with PPS characteristics ####
#****************************************************************************************************
# get pps short names from other project
# dbox <- "C:/Users/donbo/Dropbox/"
# pps_dir <- paste0(dbox, "DSRIP TSA_CA Team Info/Interim Report Documentation Compiled/DOH analysis of PPV and PPVBH/Data_DOH PPV runs/")
# pps_fn <- "msr_mo_pps.csv"
# pps <- read_csv(paste0(pps_dir, pps_fn))
# pps_xwalk <- pps %>%
#   select(PPS_ID, PPS_Name, PPS_Short_Name, PPS_Name_Old, PPS_Short_Name_Old) %>%
#   unique
# write_csv(pps_xwalk, here::here("data", "pps_xwalk.csv"))
# saveRDS(pps_xwalk, here::here("data", "pps_xwalk.rds"))

(pps_xwalk <- read_excel(here::here("data", pps_fn), sheet="pps_ids", range="A6:F31"))
(pps_counties <- read_excel(here::here("data", pps_fn), sheet="pps_counties", range="A5:E30") %>% select(PPS_ID, counties, ncounties))
(pps_summary <- read_excel(here::here("data", pps_fn), sheet="pps_summary", range="A5:I30") %>% select(-preferred_name, -ncounties)) # I checked before and ncounties match
(pps_demographics <- read_excel(here::here("data", pps_fn), sheet="pps_demographics", range="A5:I30") %>% select(-preferred_name))
(pps_swimlanes <- read_excel(here::here("data", pps_fn), sheet="pps_swimlanes", range="A5:G30") %>% select(-preferred_name))

pps_all <- pps_xwalk %>%
  left_join(pps_counties) %>%
  left_join(pps_summary) %>%
  left_join(pps_demographics) %>%
  left_join(pps_swimlanes)

# pps_xwalk <- readRDS(here::here("data", "pps_xwalk.rds"))

#****************************************************************************************************
#                get data ####
#****************************************************************************************************
# mwide <- readRDS(paste0(ddir, mwide_fn))
# str(mwide)
# mlong <- readRDS(paste0(ddir, mlong_fn))
# str(mlong)

# annwide <- readRDS(paste0(ddir, annwide_fn))
# str(annwide)

annlong <- readRDS(paste0(ddir, annlong_fn))
str(annlong)
glimpse(annlong)
ht(annlong)
count(annlong, PPS_ID, PPS_NAME) # 26 including 51 DOH Demonstration PPS
count(annlong, MSR_YR_NUM)
count(annlong, DSRIP_PROJ_ID, MSR_RESULT_ID)

# how many pps had each project-measure combo?

annlong %>%
  mutate(date=as.Date(PER_END_DT)) %>%
  group_by(MSR_YR_NUM) %>%
  summarise(n=n(), mean=mean(date), min=min(date), max=max(date))
# we should drop all of the NA MSR_YR_NUM

# make reference lists of unique PPS, unique projects, unique measures
(measures <- count(annlong, MSR_RESULT_ID, MSR_RESULT_NAME)) # 179 measures
(projects <- count(annlong, DSRIP_PROJ_ID, DSRIP_PROJ_TITLE)) # 39 projects plus domain 1
count(annlong, PPS_ID, PPS_NAME) # 25 pps's


# For each measure, how many projects, how many pps's
projcounts <- annlong %>%
  group_by(MSR_RESULT_ID, MSR_RESULT_NAME) %>%
  summarise(npps=length(unique(PPS_ID)),
            nproj=length(unique(DSRIP_PROJ_ID))) %>%
  ungroup

# 150 measures
count(projcounts, npps)
# 9 measures have only 1 pps
# 56 measures have all 25 pps

count(projcounts, nproj)
# 63 measures are for only 1 project
# 3 measures are for 39 projects

# which measures were used for all PPSs
projcounts %>% filter(npps>=25) # we still have pps 51 in the data

# which measures were used for only 1 project
projcounts %>% filter(nproj==1) 

# which measures were used for all 39 projects
projcounts %>% filter(nproj==39) # Health homes -- HHCMRES, HHCPRES, HHOERES -- engagement with home health members

measures %>% filter(str_detect(MSR_RESULT_NAME, coll("preventab", ignore_case=TRUE)))


# create a data frame of measures, dropping some bad info and keeping only 1 copy of each measure per pps
df <- annlong %>%
  filter(PPS_ID != 51, !is.na(MSR_YR_NUM)) %>%
  select(PPS_ID, MSR_YR_NUM, MSR_RESULT_ID, MSR_RESULT_NAME, MSR_RESULT) %>%
  unique %>%
  left_join(pps_xwalk %>% select(PPS_ID, PPS_Name, PPS_Short_Name, preferred_name), by="PPS_ID") %>%
  mutate(MSR_YR_NUM=as.numeric(MSR_YR_NUM),
         PPS_Name_wrap=str_wrap(str_sub(paste0(PPS_Short_Name, ": ", PPS_Name), 1, 40), 24))
count(df, PPS_ID, PPS_Name, PPS_Short_Name, PPS_Name_wrap)

(ppss <- count(df, PPS_ID, PPS_Name, PPS_Short_Name, PPS_Name_wrap, preferred_name)) # 25 pps's


# analytic graph for PPRRES ----
df %>%
  filter(MSR_RESULT_ID=="PPRRES") %>%
  ggplot(aes(MSR_YR_NUM, MSR_RESULT)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 1.5) +
  facet_wrap(~PPS_Name_wrap, ncol=4, scales="free")

# presentation graph
minlim <- 0
maxlim <- 1400
p2 <- df %>%
  filter(MSR_RESULT_ID=="PPRRES") %>%
  mutate(mygroup=ifelse(MSR_YR_NUM %in% 0:1, "pre", "post")) %>%
  group_by(mygroup, PPS_ID, PPS_Name, PPS_Short_Name, PPS_Name_wrap) %>%
  summarise(result=mean(MSR_RESULT)) %>%
  pivot_wider(names_from = mygroup, values_from = result) %>%
  mutate(badgood=ifelse(post > pre, "Deterioration", "Improvement")) %>%
  ggplot(aes(pre, post, label=PPS_Short_Name)) +
  geom_point(aes(colour=badgood), size=0.75) +
  scale_colour_manual(values=c("red", "blue")) +
  geom_text_repel(size=2) +
  geom_abline(slope=1, colour="darkgrey") +
  scale_x_continuous(name="Pre: Average of MY0 and MY1", breaks=seq(0, 2000, 250), labels=scales::comma, limits=c(minlim, maxlim)) +
  scale_y_continuous(name="Post: Average of MY2 through MY4", breaks=seq(0, 2000, 250), labels=scales::comma, limits=c(minlim, maxlim)) +
  coord_equal() +
  ggtitle("Potentially Avoidable Readmissions",
          subtitle="Before and after start of DSRIP") +
  theme_bw() +
  annotate("text", x=1000, y=375, label="Improvement", hjust=0, vjust=0, size=3, colour="blue") +
  annotate("text", x=250, y=1000, label="Deterioration", hjust=0, vjust=0, size=3, colour="red") +
  labs(caption="\nNote: Change from pre to post is not necessarily a result of DSRIP") +
  theme(plot.caption = element_text(hjust=0, size=rel(.8))) +
  theme(legend.position = "none")
p2
ggsave(here::here("results", "PPRRES_scatter.png"), p2, width=6, height=6, scale=1) # scale parameter helps adjust text relative to size of other plot elements


# analytic graph for PPVBHRES ----
df %>%
  filter(MSR_RESULT_ID=="PPVBHRES") %>%
  ggplot(aes(MSR_YR_NUM, MSR_RESULT)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 1.5) +
  facet_wrap(~PPS_Name_wrap, ncol=4, scales="free")

# presentation graph
minlim <- 35
maxlim <- 140
p3 <- df %>%
  filter(MSR_RESULT_ID=="PPVBHRES") %>%
  mutate(mygroup=ifelse(MSR_YR_NUM %in% 0:1, "pre", "post")) %>%
  group_by(mygroup, PPS_ID, PPS_Name, PPS_Short_Name, PPS_Name_wrap) %>%
  summarise(result=mean(MSR_RESULT)) %>%
  pivot_wider(names_from = mygroup, values_from = result) %>%
  mutate(badgood=ifelse(post > pre, "Deterioration", "Improvement")) %>%
  ggplot(aes(pre, post, label=PPS_Short_Name)) +
  geom_point(aes(colour=badgood), size=0.75) +
  scale_colour_manual(values=c("red", "blue")) +
  geom_text_repel(size=2) +
  geom_abline(slope=1, colour="darkgrey") +
  scale_x_continuous(name="Pre: Average of MY0 and MY1", breaks=seq(0, 200, 25), labels=scales::comma, limits=c(minlim, maxlim)) +
  scale_y_continuous(name="Post: Average of MY2 through MY4", breaks=seq(0, 200, 25), labels=scales::comma, limits=c(minlim, maxlim)) +
  coord_equal() +
  ggtitle("Potentially Preventable Emergency Department Visits\nfor persons with BH diagnosis",
          subtitle="Before and after start of DSRIP") +
  theme_bw() +
  annotate("text", x=112.5, y=62.5, label="Improvement", hjust=0, vjust=0, size=3, colour="blue") +
  annotate("text", x=50, y=125, label="Deterioration", hjust=0, vjust=0, size=3, colour="red") +
  labs(caption="\nNote: Change from pre to post is not necessarily a result of DSRIP") +
  theme(plot.caption = element_text(hjust=0, size=rel(.8))) +
  theme(legend.position = "none")
p3
ggsave(here::here("results", "PPVBHRES_scatter.png"), p3, width=6, height=6, scale=1.1) # scale parameter helps adjust text relative to size of other plot elements


# Let's look at selected measures that all PPS's had ----
# which measures were used for all PPSs
(common_measures <- projcounts %>% filter(npps>=25))
# can this really be? take a look at a few
meas <- "AAPC20RES"
df %>% filter(MSR_RESULT_ID==meas, MSR_YR_NUM %in% c(0:4)) %>%
  select(MSR_RESULT_ID, MSR_YR_NUM, PPS_Short_Name, MSR_RESULT) %>%
  pivot_wider(names_from = MSR_YR_NUM, values_from = MSR_RESULT)

# make a list of measures available for all/most PPS
measure_list <- common_measures %>%
  mutate(improvement = case_when(str_sub(MSR_RESULT_ID, 1, 4)=="AAPC" ~ "+",  # Adult Access to Preventive or Ambulatory Care
                                 str_sub(MSR_RESULT_ID, 1, 3)=="ADD"  ~ "+", # Follow-up care for Children Prescribed ADHD Medications (+ = improvement)
                                 MSR_RESULT_ID %in% c("Age3", "Age5")  ~ "+",
                                 str_sub(MSR_RESULT_ID, 1, 3)=="AMM" ~ "+",
                                 str_sub(MSR_RESULT_ID, 1, 4)=="CAPC" ~ "+",
                                 MSR_RESULT_ID=="IETIRES" ~ "+",
                                 MSR_RESULT_ID=="PPRRES" ~ "-",
                                 MSR_RESULT_ID=="PPVRES" ~ "-",
                                 MSR_RESULT_ID=="SMCRES" ~ "+",
                                 MSR_RESULT_ID=="SMDRES" ~ "+",
                                 TRUE ~ NA_character_)) %>%
  filter(!is.na(improvement))
measure_list

measure_pps_summary <- df %>%
  filter(MSR_RESULT_ID %in% measure_list$MSR_RESULT_ID) %>%
  mutate(mygroup=ifelse(MSR_YR_NUM %in% 0:1, "pre", "post")) %>%
  group_by(mygroup, MSR_RESULT_ID, PPS_ID, preferred_name) %>%
  summarise(result=mean(MSR_RESULT)) %>%
  pivot_wider(names_from = mygroup, values_from = result) %>%
  left_join(measure_list %>% select(MSR_RESULT_ID, improvement)) %>%
  mutate(better=case_when(improvement=="+" & (post > pre) ~ 1,
                          improvement=="-" & (post < pre) ~ 1,
                          TRUE ~ 0)) %>%
  group_by(PPS_ID, preferred_name) %>%
  summarise(better=mean(better))
measure_pps_summary %>% arrange(-better)

q1 <- measure_pps_summary %>% 
  ggplot() +
  geom_point(aes(x=better, y=reorder(preferred_name, better)), colour="blue", size=2) +
  scale_x_continuous(name="Percentage of measures that improved", breaks=seq(0, 1, .1), labels=scales::percent) +
  scale_y_discrete(name=NULL) +
  theme_bw() +
  theme(axis.text.y=element_text(hjust=0)) +
  ggtitle("Summary of results for 18 measures that are available for all PPS's",
          subtitle="Before DSRIP defined as average of MY0 and MY1, after start defined as average of MY2 through MY4") +
  labs(caption="\nNote: Change from pre to post is not necessarily a result of DSRIP") +
  theme(plot.caption = element_text(hjust=0, size=rel(.8))) +
  geom_vline(xintercept=.5, colour="darkgrey")
q1
ggsave(here::here("results", "MultiMeasure_dotplot.png"), q1, width=8, height=6, scale=1.25)


# take a single PPS and show multiple measures ----
measure_pps <- df %>%
  filter(PPS_ID==1, MSR_RESULT_ID %in% measure_list$MSR_RESULT_ID) %>%
  mutate(mygroup=ifelse(MSR_YR_NUM %in% 0:1, "pre", "post")) %>%
  group_by(mygroup, MSR_RESULT_ID, PPS_ID, preferred_name) %>%
  summarise(result=mean(MSR_RESULT)) %>%
  pivot_wider(names_from = mygroup, values_from = result) %>%
  left_join(measure_list %>% select(MSR_RESULT_ID, improvement, MSR_RESULT_NAME)) %>%
  mutate(better=case_when(improvement=="+" & (post > pre) ~ 1,
                          improvement=="-" & (post < pre) ~ 1,
                          TRUE ~ 0)) %>%
  mutate(diff=abs(post - pre) * ifelse(better==1, 1, -1),
         pdiff=diff / pre)

q2 <- measure_pps %>% 
  mutate(mname=paste0(MSR_RESULT_NAME, " (", MSR_RESULT_ID, ")")) %>%
  ggplot() +
  geom_point(aes(x=pdiff, y=reorder(mname, pdiff), colour=as.factor(better)), size=2) +
  scale_colour_manual(values=c("red", "blue")) +
  scale_x_continuous(name="Improvement or deterioration as a percentage of pre-DSRIP average", breaks=seq(-1, 1, .02), labels=scales::percent) + # 
  scale_y_discrete(name=NULL) +
  theme_bw() +
  theme(axis.text.y=element_text(hjust=0)) +
  ggtitle("Better Health for Northeast New York (PPS_ID 1): Results for 18 common measures",
          subtitle="Before DSRIP defined as average of MY0 and MY1, after start defined as average of MY2 through MY4") +
  labs(caption="\nNote: Change from pre to post is not necessarily a result of DSRIP") +
  theme(plot.caption = element_text(hjust=0, size=rel(.8))) +
  geom_vline(xintercept=0, colour="darkgrey") +
  theme(legend.position = "none")
q2
ggsave(here::here("results", "pps1_dotplot.png"), q2, width=11, height=6, scale=1.25)




