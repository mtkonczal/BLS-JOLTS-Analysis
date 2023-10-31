###############################################################
# Code to read in JOLTS data from CPS website and begin analysis.
# This file reads in and store all the JOLTS monthly jobs data.
#
# Recent fix: added email to calls to work with new BLS protocols.
#
# Mike Konczal
# Last updated 5/1/2023

library(janitor)
library(tidyverse)
library(ggtext)
library(lubridate)
library(data.table)
library(httr)
library(quantmod)

############### SECTION 1: READ IN AND CLEAN UP DATA #####################
jolts_data <- GET("https://download.bls.gov/pub/time.series/jt/jt.data.1.AllItems", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()
jolts_data <- jolts_data %>%
  clean_names()
jolts_data$value <- as.numeric(jolts_data$value)
jolts_data$series_id <- str_trim(jolts_data$series_id)
jolts_data$date <- paste(substr(jolts_data$period, 2,3), "01", jolts_data$year, sep="/")
jolts_data$date <- as.Date(jolts_data$date, "%m/%d/%Y")

series <- GET("https://download.bls.gov/pub/time.series/jt/jt.series", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()
series <- series %>% clean_names()
series$series_id <- str_trim(series$series_id)

Jindustry <- GET("https://download.bls.gov/pub/time.series/jt/jt.industry", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()
Jindustry <- Jindustry %>% clean_names()

Jsizecode <- GET("https://download.bls.gov/pub/time.series/jt/jt.sizeclass", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()
Jsizecode <- Jsizecode %>% clean_names()

data_element <- GET("https://download.bls.gov/pub/time.series/jt/jt.dataelement", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()
data_element <- data_element %>% clean_names()

Jstate <- GET("https://download.bls.gov/pub/time.series/jt/jt.state", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()

jolts <- jolts_data %>%
  inner_join(series, by = c("series_id")) %>%
  inner_join(data_element, by = c("dataelement_code"),suffix = c(".series", ".data_element"),) %>%
  inner_join(Jindustry, by = c("industry_code"),suffix = c(".x", ".industry_code"),) %>%
  inner_join(Jsizecode, by = "sizeclass_code") %>%
  inner_join(Jstate, by="state_code")

rm(series, data_element, Jsizecode, Jstate)


######## GET UNEMPLOYMENT NUMBER ########

ur <- getSymbols(Symbols = "UNRATE", src = "FRED", auto.assign = FALSE)
ur <- as_tibble(data.frame(date = index(ur), ur$UNRATE)) %>%
  rename(urate = UNRATE) %>%
  mutate(urate = urate/100)

jolts <- jolts %>%
  left_join(ur, by="date")

############################################


rm(ur, jolts_data)
