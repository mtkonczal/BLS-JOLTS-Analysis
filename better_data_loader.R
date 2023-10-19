# Improved R Code to Load and Process JOLTS Data
# Last updated: 5/1/2023

# Load Required Libraries
library(janitor)
library(tidyverse)
library(lubridate)
library(data.table)
library(httr)

# Define Constants
BASE_URL <- "https://download.bls.gov/pub/time.series/jt/jt."
#EMAIL <- Sys.getenv("USER_EMAIL")  # Recommended to store sensitive info securely
EMAIL <- "rortybomb@gmail.com"

# General Function to Get and Process Data
get_and_process_data <- function(endpoint) {
  data <- GET(paste0(BASE_URL, endpoint), user_agent(EMAIL)) %>%
    content(as = "text") %>%
    fread() %>%
    clean_names()
  
  return(data)
}

# Specific Function to Process JOLTS Data
process_jolts_data <- function(data) {
  data %>%
    mutate(
      value = as.numeric(value),
      series_id = str_trim(series_id),
      date = as.Date(paste(substr(period, 2,3), "01", year, sep="/"), "%m/%d/%Y")
    )
}

# Fetch and Process Data
jolts_data_raw <- get_and_process_data("data.1.AllItems")
jolts_data <- process_jolts_data(jolts_data_raw)

# List of endpoints
endpoints <- c("series","industry","sizeclass","dataelement","state")

# Using a loop to fetch and process data for each endpoint
for (i in endpoints){
  # Assign the data to a variable named after the endpoint
  assign(i, get_and_process_data(i))
}


# Merge Datasets
jolts <- jolts_data %>%
  inner_join(series, by = c("series_id")) %>%
  inner_join(dataelement, by = c("dataelement_code"),suffix = c(".series", ".data_element")) %>%
  inner_join(industry, by = c("industry_code"),suffix = c(".x", ".industry_code")) %>%
  inner_join(sizeclass, by = "sizeclass_code") %>%
  inner_join(state, by="state_code")

# Clean Up Workspace
rm(series, dataelement, sizeclass, state, industry, jolts_data_raw)
