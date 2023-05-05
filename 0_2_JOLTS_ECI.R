###############################################################
# Code to read in JOLTS data from CPS website and begin analysis.
# This file reads in and store all the JOLTS monthly jobs data.
#
# Mike Konczal
# Last updated 8/30/22

library(janitor)
library(tidyverse)
library(ggtext)
library(lubridate)
library(data.table)
library(httr)


source("1_b_JOLTS_ECI_data.R")
##### SET UP SOME THINGS #####
theme_lass <-   theme_modern_rc(ticks = TRUE) + theme(legend.position = "none", legend.title = element_blank(),
                                                      panel.grid.major.y = element_line(size=0.5),
                                                      panel.grid.minor.y = element_blank(),
                                                      plot.title.position = "plot",
                                                      axis.title.x = element_blank(),
                                                      axis.title.y = element_blank(),
                                                      plot.title = element_text(size = 25, face="bold"),
                                                      plot.subtitle = element_text(size=15, color="white"),
                                                      plot.caption = element_text(size=10, face="italic"),
                                                      legend.text = element_text(size=12),
                                                      axis.text.y = element_text(size=12, face="bold"),
                                                      axis.text.x = element_text(size=12, face="bold"),
                                                      strip.text = element_text(face = "bold", color="white", hjust = 0.5, size = 10),
                                                      panel.grid.major.x = element_blank(),
                                                      panel.grid.minor.x = element_blank(),
                                                      strip.background = element_blank()) +
  theme(text = element_text(family = "Larsseit"),
        plot.title = element_text(family = "Larsseit"),
        plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"),
        strip.text = element_text(family="Larsseit"))

#### JUST TOTAL PRIVATE ####
ces_data <- GET("https://download.bls.gov/pub/time.series/ce/ce.data.0.AllCESSeries", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread() %>%
  clean_names()
ces_data$series_id <- str_trim(ces_data$series_id)
ces_data <- ces_data %>% filter(series_id == "CES0000000001")
ces_data$value <- as.numeric(ces_data$value)
ces_data$date <- paste(substr(ces_data$period, 2,3), "01", ces_data$year, sep="/")
ces_data$date <- as.Date(ces_data$date, "%m/%d/%Y")
ces_data <- ces_data %>% select(employment_level_CES = value, date)

jolts <- jolts %>%
  left_join(ces_data, by="date")



# Graphic Set 1 : Sets Up Calculations
# Openings and Quits versus ECI Wages
JoltsMerge <- jolts %>% filter(series_id == "JTS000000000000000QUL") %>% mutate(value = value/employment_level_CES) %>%
  select(industry_text, quitsR = value, date, dataelement_text)

ECIMerge <- eci %>% filter(series_id == "CIS2020000000000I") %>% mutate(value = value/lag(value)-1) %>%
  select(ECI_growth = value, date)

merged <- JoltsMerge %>% filter(dataelement_text == "Quits") %>% left_join(ECIMerge, by=c("date")) %>%
  filter(!is.na(ECI_growth)) %>%
  mutate(Is_2021_to_2022 = (date >= "2021-01-01"))

merged_prior <- merged %>% filter(date < "2021-01-01")


a <- lm(ECI_growth ~ quitsR, data=merged_prior)
summary(a)


merged %>%
  mutate(values_last = if_else(date >= "2022-01-01", date, as.Date(NA))) %>% mutate(values_last2 = as.character(format(values_last, "%b\n%Y"))) %>%
  mutate(Is_2021_to_2022 = (date >= "2021-01-01")) %>%
  mutate(Is_2021_to_2022_v = if_else(date >= "2022-01-01",ECI_growth,as.numeric(NA))) %>%
  ggplot(aes(quitsR, ECI_growth, color=Is_2021_to_2022, label=values_last2)) + geom_point() + theme_lass +
  geom_abline(intercept = a$coefficients[1], slope=a$coefficients[2], color="#2D779C") +
  geom_text_repel(size=3) +
  geom_path(aes(quitsR,Is_2021_to_2022_v)) +
  labs(x = "Quit Rate",
       y = "ECI Private Wage Growth, Quarterly",
       title = "Wages Are Back Toward Their Predicted Values",
       subtitle = "Quit Rate (horizontal) versus ECI private wages growth rate, quarterly rate. Regression line and blue points are 2001 to 2020.",
       caption ="BLS, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  theme(axis.title.x = element_text(size=14, color="white", vjust=-1.5), axis.title.y = element_text(size=14, angle = 90, vjust = 3),
        plot.subtitle = element_text(size=12, color="white")) +
  scale_color_manual(values=c("#2D779C", "#CC79A7"))

ggsave("graphics/regression_line.png", dpi="retina", width = 12, height=6.75, units = "in")


#a <- jolts %>% filter(series_id == "JTS100000000000000QUR") %>% select(DATE = date, quits_prior = value)
#prior_eci <- read_csv("data/ecI_prior.csv") %>% left_join(a, by="DATE")
#write.csv(prior_eci, "data/eci_prior.csv")

prior_eci <- read_csv("data/eci_prior.csv") %>%
  rename(eci_prior = ECIWAG_20230131, date = DATE) %>%
  mutate(date = date %m+% months(2), eci_prior = as.numeric(eci_prior))


merged_before <- merged %>% filter(year(date)<2021) %>% mutate(names_col = "2001-2020 Values", values_col = ECI_growth, values_last = NA)

merged %>% left_join(prior_eci, by="date") %>% mutate(eci_prior = eci_prior/lag(eci_prior,1) - 1) %>%
  filter(year(date) > 2021) %>%
  pivot_longer(cols=c("ECI_growth","eci_prior"), names_to = "names_col",values_to = "values_col") %>%
  mutate(names_col = str_replace_all(names_col,"ECI_growth","Current ECI Growth, 2022-")) %>%
  mutate(names_col = str_replace_all(names_col,"eci_prior","Previous ECI Estimate, 2022-")) %>%
  mutate(values_last = if_else(date == max(date) %m-% months(3) | date == max(date), date, as.Date(NA))) %>% mutate(values_last = as.character(format(values_last, "%b\n%Y"))) %>%
  ggplot(aes(quitsR,values_col,color=names_col, label=values_last)) + geom_point() + geom_path(show.legend = FALSE) +
  theme_lass +
  geom_point(data=merged_before,aes(quitsR, ECI_growth),alpha=0.5) +
  geom_abline(intercept = a$coefficients[1], slope=a$coefficients[2], color="#2D779C", alpha=0.5) + geom_text_repel(show.legend = FALSE) +
  labs(x = "Quit Rate",
       y = "ECI Private Wage Growth, Quarterly",
       title = "Seasonal Adjustment Revisions Change ECI Picture",
       subtitle = "Quit Rate (horizontal) versus ECI private wages growth rate, quarterly rate.",
       caption ="BLS, seasonally adjusted. Previous ECI estimate is from January 2023. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  theme(axis.title.x = element_text(size=14, color="white", vjust=-1.5), axis.title.y = element_text(size=14, angle = 90, vjust = 3),
        plot.subtitle = element_text(size=12, color="white"), legend.position = c(0.3,0.8)) +
  scale_color_manual(values=c("#2D779C", "#1F968BFF", "#CC79A7"))


ggsave("graphics/regression_line_contrast.png", dpi="retina", width = 12, height=6.75, units = "in")

prior_quits <- read_csv("data/eci_prior.csv") %>% select(date = DATE, quits_prior)

jolts %>% filter(series_id == "JTS100000000000000QUR") %>% left_join(prior_quits, by="date") %>% select(date, value, quits_prior) %>%
  filter(!is.na(quits_prior)) %>% summarize(sum(value - quits_prior))