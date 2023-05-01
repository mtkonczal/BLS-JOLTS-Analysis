# TKTK
setwd("/Users/mkonczal/Documents/R Code/")
library(janitor)
library(tidyverse)
library(ggtext)



jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2) %>%
  filter(dataelement_text != "Layoffs and discharges", dataelement_text != "Other separations") %>%
  filter(date == max(date)) %>%
  ggplot(aes(industry_text, value)) + geom_bar(stat="identity") + facet_wrap(~ dataelement_text) +
  coord_flip()

a <- jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2) %>%
  filter(dataelement_text == "Quits" | dataelement_text == "Job openings") %>%
  filter(year == 2019 | year == 2022) %>%
  pivot_wider(id_cols = c("date","industry_text"), names_from = dataelement_text, values_from = value) %>%
  mutate(year = factor(year(date)))

ggplot(a, aes(Quits, `Job openings`, color = year)) + geom_point() + theme_classic() + facet_wrap(~ industry_text)

a <- jolts %>% filter(series_id == "JTS000000000000000JOR") %>%
  select(date, value, urate) %>%
  mutate(SixMonthChange = value - lag(value, 6))

a %>% ggplot(aes(urate,SixMonthChange)) + geom_point()

historical_jolts <- read_csv(file = "historical jolts.csv") %>% mutate(jo_ratio = URATE/job_opening_rate)
historical_jolts <- historical_jolts %>% filter(date < "2000-01-01")
#unemployment_rate <- read_csv(file = "UNRATE.csv")
#historical_jolts$date2 <- as.Date(historical_jolts$Date)
ggplot(historical_jolts, aes(x=date, y=job_opening_rate)) + geom_line()
ggplot(historical_jolts, aes(x=date, y=jo_ratio)) + geom_line() + hline()


job_openingsL <- jolts %>%
  filter(seasonal == "S", state_code == "00") %>%
  filter(dataelement_text == "Job openings", ratelevel_code == "R", industry_code == "000000") %>%
  arrange(date) %>%
  select(date, value) %>%
  rename(JoltsOpening = value)

long_bevers <- full_join(historical_jolts, job_openingsL, by="date") %>% arrange(date)

ggplot(long_bevers, aes(x=date)) + geom_line(aes(y=JoltsOpening, color="red")) +
  geom_line(aes(y=job_opening_rate, color="blue")) + theme_light() +   expand_limits(y = 0)

ggplot(long_bevers, aes(x=date)) +
  geom_line(aes(y=JoltsOpening), color="red", size=1.1) +
  geom_line(aes(y=job_opening_rate), color="blue", size=1.1) +
  expand_limits(y = 0) +
  theme_minimal() +
  labs(title = "Do Job Openings Level Out in the Business Cycle?",
       caption = "JOLTS after 2000, Regis Barnichon 'Building a composite Help-Wanted Index' (2010) prior. Mike Konczal, Roosevelt Institute",
       x="", y="") +
  theme(
    plot.title.position = "plot",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(size = 18, colour = "darkred", face = "italic"),
    plot.caption = element_text(size = 12, lineheight = 1.2))

tail

long_bevers
long_bevers_sub <- long_bevers %>% mutate(lagged = job_opening_rate - lag(job_opening_rate,12))
ggplot(long_bevers_sub, aes(x=date, y=lagged)) + geom_path()


Bevers <- jolts %>%
  filter(seasonal == "S", state_code == "00", ratelevel_code == "R", industry_code == "000000") %>%
  filter(dataelement_code == "UO")
ggplot(Bevers, aes(x=date, y=value)) + geom_line()

Bevers2 <- Bevers %>%
  select(date, dataelement_text, value)

job_openingsL2 <- job_openingsL %>%
  mutate(jo_label = 0) %>%
  left_join(long_bevers, by="date")

job_openingsL$jo_label[254] = 1
job_openingsL$jo_label[248] = 1
job_openingsL$jo_label <- job_openingsL$jo_label*job_openingsL$JoltsOpening
job_openingsL$jo_label <- na_if(job_openingsL$jo_label, 0)

ggplot(job_openingsL, aes(x=date)) +
  geom_line(aes(y=JoltsOpening), color="steelblue", size=1.2) +

  expand_limits(y = 0) +
  theme_minimal() +
  labs(title = "After Rising Dramatically, Job Openings Stay Level Over Past Seven Months",
       subtitle = "This is after yearly revisions; unemployment has fallen 1.6 percent during this time.",
       caption = "BLS, JOLTS, Seasonally-Adjusted. Author's Calculation. @rortybomb",
       x="", y="") +
  theme(
    plot.title.position = "plot",
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    plot.title = element_text(size = 30, hjust = 0.5),
    panel.grid = element_blank(),
    plot.subtitle = element_text(size = 28, colour = "darkred", face = "italic", hjust = 0.5),
    plot.caption = element_text(size = 20, lineheight = 1.2)) +
    geom_text(aes(x=date, y=JoltsOpening, label=(round(jo_label, 2))), nudge_y = .3, size = 5, color = "steelblue")


ggsave("openings.pdf")


# LOCATION DIVE
eci_location <- eci %>% filter(date == max(date)) %>%
  filter(industry_code == "000000", estimate_code == "02", periodicity_code=="Q",
         occupation_code == "000000", owner_code == "2", seasonal == "U")

#
cpi_location <- cpi_data %>% filter(period != "M13", seasonal == "U") %>%
  filter(item_code == "SA0") %>%
  filter(date == max(date))

ces_location <- ces_wages_data  %>% filter(period != "M13", seasonal == "U") %>%
  filter(date == max(date)) %>%
  filter(data_type_code == "08")

cps_location <- cps_jobs_data %>% filter(period != "M13", seasonal == "U") %>%
  filter(date == max(date))