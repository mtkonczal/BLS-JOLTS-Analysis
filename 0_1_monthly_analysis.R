# TKTK
setwd("/Users/mkonczal/Documents/GitHub/BLS-JOLTS-Analysis/")
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



source("1_a_load_jolts_data.R")

#### THE KEY INDICATORS VERSUS 2018-2019 ###
toplines_rates <- c("JTS100000000000000HIR","JTS000000000000000JOR","JTS000000000000000LDR","JTS000000000000000OSR","JTS000000000000000QUR","JTS000000000000000TSR","JTS000000000000000UOR")



MI_dates <- jolts %>% filter(date > "2020-12-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates = MI_dates[seq(1, length(MI_dates), 6)]


baseline_2019 <- jolts %>% filter(period != "M13", seasonal == "S",
                                  series_id %in% toplines_rates) %>%
  mutate(value = value/100) %>%
  filter(year == 2019) %>%
  group_by(dataelement_text) %>%
  summarize(avg_2019 = mean(value)) %>%
  ungroup()


##### FIRST CHECK GRAPHIC OVERALL RATE ####
jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R",
                 series_id %in% toplines_rates) %>%
  mutate(value = value/100) %>%
  filter(year >= 2021) %>%
  left_join(baseline_2019, by="dataelement_text") %>%
  ggplot(aes(date, value)) + geom_line() + facet_wrap(~dataelement_text, scales = "free_y") +
  theme_lass +
  geom_line(aes(date, avg_2019), color="red", linetype="dashed") +
  labs(title="THIS LETS US SEE EVERYTHING",
       subtitle="Blue line is within 2022; Red line is average across 2019",
       caption="BLS, JOLTS, seasonally-adjusted, author's calculations, Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  #scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b %Y") +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=19)) +
  theme(plot.title = element_text(size = 22, face="bold"))

ggsave("graphics/overall_check_me_graphic.png", width = 12, height=6.75, dpi="retina")


#### FIRST KEY GRAPHIC
jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R",
                 series_id %in% c("JTS100000000000000HIR","JTS100000000000000QUR","JTS100000000000000JOR")) %>%
  mutate(value = value/100) %>%
  filter(year >= 2021) %>%
  left_join(baseline_2019, by="dataelement_text") %>%
  mutate(dataelement_textF = factor(dataelement_text, levels = c("Hires", "Quits", "Job openings"))) %>%
  ggplot(aes(date, value)) + geom_line() + facet_wrap(~dataelement_textF, scales = "free") +
  theme_lass +
  geom_line(aes(date, avg_2019), color="red", linetype="dashed") +
  labs(title="Key JOLTs Measures Remain Lower than Earlier 2022",
       subtitle="Blue line is actual monthly value, total private; Red line is average value across 2019",
       caption="BLS, JOLTS, seasonally-adjusted, author's calculations, Mike Konczal, Roosevelt Institute") +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b %y", breaks = MI_dates) +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=19)) +
  theme(plot.title = element_text(size = 22, face="bold"),
        strip.text = element_text(face = "bold", size = rel(2)))
        

ggsave("graphics/three_measures.png", width = 12, height=6.75, dpi="retina")




# GRAPHIC 1: BEVERIDGE CURVE
jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  #mutate(last_date = if_else(date >= max(date) %m-% months(month_delay),date,as_date(NA))) %>%
  mutate(decade = floor(year/10),0) %>%
  mutate(decade = decade*10, decade = paste(as.character(decade), "s", sep=""), decade = as.factor(decade)) %>%
  filter(series_id %in% c("JTS000000000000000JOR")) %>%
  mutate(value = value/100, urate = urate/100) %>%
  ggplot(aes(x=urate, y=value, color=decade)) + geom_point() + geom_path() +
  labs(title="The Beveridge Curve (Is Not a Structural Relationship)",
       subtitle="Job opening rate verus unemployment rate.",
       x="Unemployment Rate", y="",
       caption="BLS, JOLTS, seasonally-adjusted, author's calculations, Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + theme_lass +
  theme(axis.text.x = element_text(size=15, face="bold"),
        axis.text.y = element_text(size=15, face="bold"),
        axis.title.x = element_text(size=15, family="Larsseit", margin = margin(t = 20, r = 0, b = 0, l = 0))
        ) +
  theme(panel.grid.minor = element_blank()) + theme(panel.grid.major = element_blank()) +
  #geom_text_repel(aes(label = format(last_date, format = "%B \n%Y")),color="skyblue", force=1) +
  theme(legend.position = "bottom")

ggsave("graphics/jolts_bc.png", width = 12, height=6.75, dpi="retina")

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


###### HIRING BY INDUSTRY
baseline_2019i <- jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
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
  filter(year >= 2021) %>%
  left_join(baseline_2019i, by="industry_text") %>%
  mutate(industry_text = str_replace_all(industry_text, "Professional and business services", "Professional/business services")) %>%
  mutate(industry_text = str_replace_all(industry_text, "Trade, transportation, and utilities", "Trade and transportation")) %>%
  ggplot(aes(date, value)) + geom_line() + facet_wrap(~industry_text, scales = "free_y") +
  theme_lass +
  geom_line(aes(date, avg_2019), color="red", linetype="dashed") +
  labs(title="Hiring Rate is Decreasing Across Industries",
       subtitle="Blue line is monthly hiring rate within 2022; Red line is average across 2019",
       caption="BLS, JOLTS, seasonally-adjusted, author's calculations, Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  scale_x_date(date_labels = "%b %y")

ggsave("graphics/jolts2_hiring_industry.png", width = 12, height=6.75, dpi="retina")

#### QUITS BY INDUSTRY ###
baseline_2019i <- jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2 | series_id == "JTS100000000000000QUR") %>%
  filter(dataelement_text == "Quits") %>%
  filter(industry_text != "Federal") %>%
  mutate(value = value/100) %>%
  filter(year == 2019) %>%
  group_by(industry_text) %>%
  summarize(avg_2019 = mean(value)) %>%
  ungroup()

##### QUITS RATE ####
jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2 | series_id == "JTS100000000000000QUR") %>%
  filter(dataelement_text == "Quits") %>%
  filter(industry_text != "Federal") %>%
  mutate(value = value/100) %>%
  filter(year >= 2022) %>%
  left_join(baseline_2019i, by="industry_text") %>%
  mutate(industry_text = str_replace_all(industry_text, "Professional and business services", "Professional/business services")) %>%
  mutate(industry_text = str_replace_all(industry_text, "Trade, transportation, and utilities", "Trade and transportation")) %>%
  ggplot(aes(date, value)) + geom_line() + facet_wrap(~industry_text, scales = "free_y") +
  theme_lass +
  geom_line(aes(date, avg_2019), color="red", linetype="dashed") +
  labs(title="Quit Rates Are Slowly Decreasing Across Industries",
       subtitle="Blue line is monthly quit rate within 2022; Red line is average across 2019",
       caption="BLS, JOLTS, seasonally-adjusted, author's calculations, Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L))

ggsave("graphics/jolts2_quits_industry.png", width = 12, height=6.75, dpi="retina")



#### OPENINGS BY INDUSTRY ###
baseline_2019i <- jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2 | series_id == "JTS100000000000000JOR") %>%
  filter(dataelement_text == "Job openings") %>%
  filter(industry_text != "Federal") %>%
  mutate(value = value/100) %>%
  filter(year == 2019) %>%
  group_by(industry_text) %>%
  summarize(avg_2019 = mean(value)) %>%
  ungroup()

##### HIRING RATE ####
jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2 | series_id == "JTS100000000000000JOR") %>%
  filter(dataelement_text == "Job openings") %>%
  filter(industry_text != "Federal") %>%
  mutate(value = value/100) %>%
  filter(year >= 2021) %>%
  left_join(baseline_2019i, by="industry_text") %>%
  mutate(industry_text = str_replace_all(industry_text, "Professional and business services", "Professional/business services")) %>%
  mutate(industry_text = str_replace_all(industry_text, "Trade, transportation, and utilities", "Trade and transportation")) %>%
  ggplot(aes(date, value)) + geom_line() + facet_wrap(~industry_text, scales = "free_y") +
  theme_lass +
  geom_line(aes(date, avg_2019), color="red", linetype="dashed") +
  labs(title="Job Opening Rate Across Industries",
       subtitle="Red line is average across 2019",
       caption="BLS, JOLTS, seasonally-adjusted, author's calculations, Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L)) +
  scale_x_date(date_labels = "%b\n%y")

ggsave("graphics/jolts2_openings_industry.png", width = 12, height=6.75, dpi="retina")