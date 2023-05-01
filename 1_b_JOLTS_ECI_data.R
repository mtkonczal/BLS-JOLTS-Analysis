# Mike Konczal

library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)
library(janitor)
library(data.table)
library(httr)

#### LOAD ECI DATA ########

eci_wages_start <- GET("https://download.bls.gov/pub/time.series/ci/ci.data.1.AllData", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread() %>%
  clean_names()
eci_wages_start$value <- as.numeric(eci_wages_start$value)
# Do Date

eci_wages_next <- eci_wages_start %>%
  mutate(month = case_when(
    period == "Q01" ~ 3,
    period == "Q02" ~ 6,
    period == "Q03" ~ 9,
    period == "Q04" ~ 12))
eci_wages_next$date <- paste(eci_wages_next$month, "01", eci_wages_next$year, sep="/")
eci_wages_next$date <- as.Date(eci_wages_next$date, "%m/%d/%Y")

eci <- eci_wages_next
##### INITIAL ANALYSIS ####
rm(eci_wages_next, eci_wages_start)


rm(ces_data)