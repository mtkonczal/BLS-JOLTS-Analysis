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

jolts <- jolts_data %>%
  inner_join(series, by = c("series_id")) %>%
  inner_join(data_element, by = c("dataelement_code"),suffix = c(".series", ".data_element"),) %>%
  inner_join(Jindustry, by = c("industry_code"),suffix = c(".x", ".industry_code"),) %>%
  inner_join(Jsizecode, by = "sizeclass_code")

rm(series, data_element, Jsizecode)


######## GET UNEMPLOYMENT NUMBER ########
unemployment_number <- GET("https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()
unemployment_number <- unemployment_number %>%
  clean_names() %>%
  mutate(series_id = str_trim(series_id)) %>%
  filter(series_id == "LNS14000000") %>%
  mutate(value = as.numeric(value)) %>%
  mutate(date = paste(substr(period, 2,3), "01", year, sep="/")) %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  arrange(date) %>%
  select(date, urate = value)
############################################

jolts <- jolts %>%
  left_join(unemployment_number, by="date")


rm(unemployment_number, jolts_data)
