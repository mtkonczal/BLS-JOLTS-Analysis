# TKTK
setwd("/Users/mkonczal/Documents/GitHub/BLS-JOLTS-Analysis//")
library(janitor)
library(tidyverse)
library(ggtext)
library(hrbrthemes)
library(scales)
library(ggrepel)

theme_lass <-   theme_modern_rc() + theme(legend.position = "none", legend.title = element_blank(),
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
                                          strip.background = element_blank()) +
  theme(text = element_text(family = "Larsseit"),
        plot.title = element_text(family = "Larsseit"),
        plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"),
        strip.text = element_text(family="Larsseit"))


eci_data <- eci %>% filter(display_level.x == 2, periodicity_code == "Q", estimate_code == "02", owner_code == 2,
                         seasonal == "S") %>%
  select(industry_text, date, quarterly_wage_change = value)

combined_eci_jolts <- jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2) %>%
  filter(dataelement_text == "Quits" | dataelement_text == "Job openings") %>%
  inner_join(eci_data, by=c("date","industry_text")) %>%
  select(date, value, quarterly_wage_change, industry_text, dataelement_text)

combined_eci_jolts %>%
  mutate(pandemic = if_else(date > "2019-12-01", "2020-Now", "2005-2019")) %>%
  ggplot(aes(value, quarterly_wage_change, color=pandemic)) + geom_point() + facet_wrap(~dataelement_text, scales = "free_x") +
  geom_smooth(method="lm") + theme_classic() + theme(legend.position = "bottom") +
  labs(title="Prepandemic JOLTs data doesn't well predict pandemic gains") +
  theme(panel.grid.major.y = element_line(size=0.5),
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
        strip.text = element_text(face = "bold", color="black", hjust = 0.5, size = 10),
        strip.background = element_blank())

ggsave("graphics/wages_JOLTS.png", dpi=400)

########

#### GRAPHIC 2: SAME BUT TOP LEVEL
eci_data <- eci %>% filter(series_id == "CIS2020000000000Q") %>%
  select(industry_text, date, quarterly_wage_change = value)

combined_eci_jolts <- jolts %>% filter(series_id == "JTS100000000000000QUR" | series_id == "JTS100000000000000JOR") %>%
  inner_join(eci_data, by=c("date")) %>%
  select(date, value, quarterly_wage_change, dataelement_text)

combined_eci_jolts %>%
  mutate(pandemic = if_else(date > "2019-12-01", "2020-Now", "2005-2019")) %>%
  ggplot(aes(value, quarterly_wage_change, color=pandemic)) + geom_point() + facet_wrap(~dataelement_text, scales = "free_x") +
  geom_smooth(method="lm") + theme_classic() + theme(legend.position = "bottom") +
  labs(y="ECI Growth", x="Rate")









pre_quits <- combined_eci_jolts %>%
  filter(date < "2020-01-01", dataelement_text == "Quits")

pre_quits <- lm(quarterly_wage_change ~ value + industry_text, data=pre_quits)
summary(pre_quits)

pre_openings <- combined_eci_jolts %>%
  filter(date < "2020-01-01", dataelement_text == "Job openings")

pre_openings <- lm(quarterly_wage_change ~ value + industry_text, data=pre_openings)
summary(pre_openings)

post_quits <- combined_eci_jolts %>%
  filter(date > "2020-01-01", dataelement_text == "Quits")

post_quits <- lm(quarterly_wage_change ~ value + industry_text, data=post_quits)
summary(post_quits)

post_openings <- combined_eci_jolts %>%
  filter(date > "2020-01-01", dataelement_text == "Job openings")

post_openings <- lm(quarterly_wage_change ~ value, data=post_openings)
summary(post_openings)

both <- pre_quits <- combined_eci_jolts %>%
  filter(dataelement_text == "Quits") %>% select(date, industry_text, quits = value)

teter2 <- combined_eci_jolts %>%
  filter(date < "2020-01-01", dataelement_text == "Job openings") %>%
  inner_join(both, by=c("date", "industry_text")) %>% rename(openings = value) %>%
  lm(quarterly_wage_change ~ openings + quits + industry_text, data=.)
summary(teter2)






a <- jolts %>% filter(period != "M13", seasonal == "S") %>%
  filter(dataelement_text == "Job openings") %>% filter(date == max(date))

# CHECK ME OUT FIRST FOR A SUMMARY OF CHANGES
jolts_august %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(series_id == "JTS000000000000000QUR" | series_id == "JTS000000000000000JOR") %>%
  select(series_id, date, last_value = value) %>%
  left_join(jolts, by=c("series_id","date")) %>%
  filter(date > "2022-01-01") %>% 
  select(dataelement_text, date, current_value = value, last_value) %>% mutate(diff = current_value - last_value)

#source("1_load_jolts_data.R")
#source("1_b_JOLTS_ECI_data.R")

#two_months <- jolts %>% filter(period != "M13", seasonal == "S") %>% select(date) %>%
#  summarize(max(date) %m-% months(1))
#two_months <- as_date(two_months)
month_delay <- 0

# GRAPHIC 1: BEVERIDGE CURVE
jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  #mutate(last_date = if_else(date >= max(date) %m-% months(month_delay),date,as_date(NA))) %>%
  mutate(decade = floor(year/10),0) %>%
  mutate(decade = decade*10, decade = paste(as.character(decade), "s", sep=""), decade = as.factor(decade)) %>%
  filter(series_id %in% c("JTS000000000000000JOR")) %>%
  mutate(value = value/100, urate = urate/100) %>%
  ggplot(aes(x=urate, y=value, color=decade)) + geom_point() + geom_path() +
  labs(title="The Beveridge Curve (Is Not a Structural Relationship)",
       subtitle="Job opening rate verus unemployment rate. Made with ggplot2 in R's tidyverse.",
       x="Unemployment Rate", y="",
       caption="BLS, ECI, seasonally-adjusted, author's calculations, Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + theme_lass +
  theme(axis.text.x = element_text(size=15, face="bold"),
        axis.text.y = element_text(size=15, face="bold"),
        axis.title.x = element_text(size=15, family="Larsseit", margin = margin(t = 20, r = 0, b = 0, l = 0))
        ) +
  theme(panel.grid.minor = element_blank()) + theme(panel.grid.major = element_blank()) +
  #geom_text_repel(aes(label = format(last_date, format = "%B \n%Y")),color="skyblue", force=1) +
  theme(legend.position = "bottom")

ggsave("graphics/jolts1_bc.png", width = 12, height=6.75, dpi="retina")

# GRAPHIC 2: Job Openings Across Industries
jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2 | series_id == "JTS100000000000000JOR") %>%
  filter(year >= 2019) %>%
  filter(dataelement_text == "Job openings") %>%
  filter(industry_text != "Federal") %>%
  mutate(value = value/100) %>%
  ggplot(aes(date, value)) + geom_line() + facet_wrap(~industry_text) +
  theme_lass +
  labs(title="Job Openings are decreasing across industries",
       subtitle="Job Openings by industry, JOLTS",
       caption="BLS, ECI, seasonally-adjusted, author's calculations, Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = scales::percent)

ggsave("graphics/jolts2_openings_industry.png", width = 9.5, height=5.34, dpi="retina")

# GRAPHIC 3: Change
a <- jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2 | series_id == "JTS100000000000000QUR") %>%
  filter(dataelement_text == "Quits") %>%
  filter(date == "2019-12-01" | date == max(date) | date == "2021-12-01") %>%
  group_by(industry_text) %>% arrange(date) %>% mutate(change = value - lag(value,1)) %>%
  ungroup() %>%
  filter(industry_text != "State and local" & industry_text != "Federal") %>%
  select(date, year, value, dataelement_text, industry_text, change) %>%
  filter(!is.na(change))

b <- a %>% filter(year == 2021) %>% select(v2021 = change, industry_text)

a %>% filter(year == 2022) %>% left_join(b, by="industry_text") %>% rename(v2022 = value) %>%
  ggplot(aes(change, v2021, label=industry_text)) + geom_point() +
  geom_text_repel(color="skyblue") +
  labs(title="Quits are falling across industries in proportion to how they rose",
       subtitle="Change in Quit Rate, 12/2019 to 12/21, and 12/21 to 9/22",
       x="Change from 12/21 to 9/22",
       y="Change from 12/19 to 12/21",
       caption="BLS, ECI, seasonally-adjusted, author's calculations, Mike Konczal, Roosevelt Institute") +
  theme_lass + 
  theme(text = element_text(family = "Larsseit"),
        plot.title = element_text(family = "Larsseit"),
        plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"),
        strip.text = element_text(family="Larsseit")) +
  theme(axis.title.x = element_text(size=15, family="Larsseit", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size=15, family="Larsseit", margin = margin(t = 0, r = 20, b = 20, l = 0), angle = 90))
                                    

ggsave("graphics/jolts3_openings_change.png", width = 12, height=6.75, units = "in")


### GRAPHIC 4: Quits versus Job Openings

jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(series_id == "JTS100000000000000QUR" | series_id == "JTS100000000000000JOR") %>%
  filter(year >= 2017) %>%
  mutate(value = value/100) %>%
  ggplot(aes(date, value, color=dataelement_text)) + geom_line() +
  theme_lass +
  labs(title="Openings Slightly Revert, Quits Continue to Decline",
  subtitle="Total private, job openings and quits",
  caption="BLS, ECI, seasonally-adjusted, author's calculations, Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom")

ggsave("graphics/jolts4_quits_openings.png", width = 19.39138, height=10.9, dpi="retina")


### THIS IS CORRECT BUT PAUSE
jolts_august %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  mutate(value = value*1.1) %>%
  filter(series_id == "JTS100000000000000QUR" | series_id == "JTS100000000000000JOR") %>%
  select(series_id, date, last_value = value) %>%
  left_join(jolts, by=c("series_id","date")) %>%
  mutate(change = value - last_value) %>%
  filter(date > "2022-01-01") %>%
  ggplot(aes(date, change)) + geom_bar(stat="identity") + facet_wrap(~dataelement_text)

jolts_august %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(series_id == "JTS100000000000000QUR" | series_id == "JTS100000000000000JOR") %>%
  select(series_id, date, last_value = value) %>%
  left_join(jolts, by=c("series_id","date")) %>%
  filter(date > "2022-01-01") %>%
  ggplot(aes(date, value)) + geom_line() + facet_wrap(~dataelement_text) +
  geom_line(aes(date, last_value), color="red") + theme_lass

# NEXT LOOK TO WAGES VERSUS TKTKTK:

jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(year > 2018) %>%
  filter(dataelement_text == "Job openings") %>%
  filter(industry_text != "State and local" & industry_text != "Federal") %>%
  ggplot(aes(date, value)) + geom_line() + facet_wrap(~industry_text)


# CHECK SIZE
jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(dataelement_text == "Job openings") %>%
  filter(industry_text == "Total private", date == "2022-05-01")


# TRY
jolts %>% filter(series_id == "JTS100000000000000JOR") %>%
  












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


# GRAPHIC TKTKTK: Do Quits Predict Openings
jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2) %>%
  filter(year >= 2019) %>%
  filter(dataelement_text == "Job openings" | dataelement_text == "Quits") %>%
  filter(industry_text != "Federal" & industry_text != "State and local") %>%
  mutate(value = value/100) %>%
  ggplot(aes(date, value, color=dataelement_text)) + geom_line() + facet_wrap(~industry_text) +
  theme_lass +
  labs(title="Job Openings are decreasing across industries") +
  scale_y_continuous(labels = scales::percent)


baseline_2019 <- jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2 | series_id == "JTS100000000000000HIR") %>%
  filter(dataelement_text == "Hires") %>%
  filter(industry_text != "Federal") %>%
  mutate(value = value/100) %>%
  filter(year == 2019) %>%
  group_by(industry_text) %>%
  summarize(avg_2019 = mean(value)) %>%
  ungroup()

##### HIRING RATE ####
jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2 | series_id == "JTS100000000000000HIR") %>%
# GRAPHIC 2: Job Openings Across Industries
  filter(dataelement_text == "Hires") %>%
  filter(industry_text != "Federal") %>%
  mutate(value = value/100) %>%
  filter(year >= 2022) %>%
  left_join(baseline_2019, by="industry_text") %>%
  mutate(industry_text = str_replace_all(industry_text, "Professional and business services", "Professional/business services")) %>%
  mutate(industry_text = str_replace_all(industry_text, "Trade, transportation, and utilities", "Trade and transportation")) %>%
  ggplot(aes(date, value)) + geom_line() + facet_wrap(~industry_text, scales = "free_y") +
  theme_lass +
  geom_line(aes(date, avg_2019), color="red", linetype="dashed") +
  labs(title="Hiring Rate is Decreasing Across Industries",
       subtitle="Blue line is within 2022; Red line is average across 2019",
       caption="BLS, JOLTS, seasonally-adjusted, author's calculations, Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L))

ggsave("graphics/jolts2_hiring_industry.png", width = 12, height=6.75, dpi="retina")