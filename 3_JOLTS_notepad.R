# TKTK
setwd("/Users/mkonczal/Documents/GitHub/BLS-JOLTS-Analysis//")
library(janitor)
library(tidyverse)
library(ggtext)

source("1_load_jolts_data.R")
#source("1_b_JOLTS_ECI_data.R")


jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R", industry_code == "000000") %>%
  mutate(years = if_else(year==2022,TRUE,FALSE)) %>%
  filter(series_id %in% c("JTS000000000000000QUR", "JTS000000000000000JOR")) %>%
  select(date, dataelement_text, value, years) %>% pivot_wider(names_from = dataelement_text, values_from = value) %>%
  ggplot(aes(`Job openings`, Quits)) + geom_point() + theme_classic() + geom_smooth(method="lm")




b <- jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R", industry_code == "000000") %>%
  filter(date == max(date))

JoltsMerge <- jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2) %>%
  filter(dataelement_text == "Job openings" | dataelement_text == "Quits") %>%
  select(industry_text, Joltsvalue = value, date, dataelement_text)


ECIMerge <- eci %>% filter(period != "M13", seasonal == "S", periodicity_code == "Q") %>%
  filter(display_level.x == 2, estimate_code == "02") %>%
  rename(industry_text = industry_title) %>%
  select(industry_text, ECI_growth = value, date)

Merged <- JoltsMerge %>% left_join(ECIMerge, by=c("date","industry_text")) %>%
  filter(!is.na(ECI_growth)) %>%
  group_by(industry_text) %>% mutate(Vchange = Joltsvalue - lag(Joltsvalue,1)) %>%
  ungroup() %>%
  mutate(pandemic = (date >= "2021-01-01"))


Merged %>%
ggplot(aes(Joltsvalue, ECI_growth, color=pandemic)) + geom_point() + theme_classic() +
  facet_wrap(~dataelement_text) + geom_smooth(method = "lm")

Merged %>%
  ggplot(aes(Joltsvalue, ECI_growth, color=dataelement_text)) + geom_point() + theme_classic() +
  facet_wrap(~industry_text) + geom_smooth(method = "lm")

Merged %>% pivot_wider(names_from = "data")


#### State level JOLTS! ####

unique(jolts$state_text)

b <- jolts %>% filter(!is.na(date), seasonal == "S") %>% group_by(state_text, seasonal) %>%
  summarize(n())
a <- jolts %>% filter(state_text == "Illinois", !is.na(date)) %>% filter(date == max(date))

View((a[date==max(date)])