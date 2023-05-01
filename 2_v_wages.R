# TKTK
setwd("/Users/mkonczal/Documents/GitHub/BLS-JOLTS-Analysis/")
library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)

source("1_a_load_jolts_data.R")
source("1_b_JOLTS_ECI_data.R")
source("1_c_load_cpi_data.R")


# Graphic Set 1 : Sets Up Calculations
# Openings and Quits versus ECI Wages
JoltsMerge <- jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2) %>%
  filter(dataelement_text == "Quits") %>%
  select(industry_text, Joltsvalue = value, date, dataelement_text)

ECIMerge <- eci %>% filter(period != "M13", seasonal == "S", periodicity_code == "Q") %>%
  filter(display_level.x == 2, estimate_code == "02") %>%
  select(industry_text, ECI_growth = value, date)

Merged <- JoltsMerge %>% filter(dataelement_text == "Quits") %>% left_join(ECIMerge, by=c("date","industry_text")) %>%
  filter(!is.na(ECI_growth)) %>%
  mutate(Is_2021_to_2022 = (date >= "2021-01-01"))

Merged %>%
ggplot(aes(Joltsvalue, ECI_growth, color=Is_2021_to_2022)) + geom_point() + theme_classic() +
  facet_wrap(~dataelement_text) + geom_smooth(method = "lm")


#THIS IS THE GRAPHIC
Merged %>%
  ggplot(aes(Joltsvalue, ECI_growth, color=Is_2021_to_2022)) + geom_point() + theme_classic() +
  facet_wrap(~industry_text, scales = "free") + geom_smooth(method = "lm") + theme(legend.position = "bottom") +
  labs(y="ECI growth", x="Quit Rate", title="3-month ECI Growth and Quits, 2001-2022 (Green is 2021-2022)",
       subtitle="Regressions find the recovery period has higher wage growth controlling for quits, job openings, and industry")

ggsave("graphics/quits_industry.png", width = 14.25, height=8.01, dpi="retina")

#### JUST TOTAL PRIVATE ####

# Graphic Set 1 : Sets Up Calculations
# Openings and Quits versus ECI Wages
JoltsMerge <- jolts %>% filter(series_id == "JTS000000000000000QUR") %>%
  select(industry_text, Joltsvalue = value, date, dataelement_text)

ECIMerge <- eci %>% filter(series_id == "CIS2020000000000Q") %>%
  select(industry_text, ECI_growth = value, date)

Merged <- JoltsMerge %>% filter(dataelement_text == "Quits") %>% left_join(ECIMerge, by=c("date")) %>%
  filter(!is.na(ECI_growth)) %>%
  mutate(Is_2021_to_2022 = (date >= "2021-01-01"))

Merged %>%
  mutate(values_last = fifelse(date >= "2021-12-22", date, as.Date(NA))) %>%
  ggplot(aes(Joltsvalue, ECI_growth, color=Is_2021_to_2022, label=values_last)) + geom_point() + theme_classic()+ geom_smooth(method = "lm") + geom_label()


Merged %>%
  mutate(values_last = fifelse(date == max(date), date, as.Date(NA))) %>%
  ggplot(aes(Joltsvalue, ECI_growth, label=values_last)) + geom_point() + theme_classic() + geom_smooth(method="lm") + geom_label(nudge_y = 0.05)

ggsave("graphics/quits_total_private.png", width = 14.25, height=8.01, dpi="retina")

JoltsMerge$Joltsvalue









a <- lm(ECI_growth ~ Joltsvalue + Is_2021_to_2022 + industry_text, data=Merged)
summary(a)

a <- Merged %>% mutate(Joltsvalue2 = (1+Joltsvalue/100)*(1+Joltsvalue/100)-1) %>% lm(ECI_growth ~ Joltsvalue2 + Is_2021_to_2022 + industry_text, data=.)
summary(a)


Merged %>% mutate(ECI_growthA = (ECI_growth/100 +1)^4-1) %>%
  ggplot(aes(Joltsvalue, ECI_growthA, color=pandemic)) + geom_point() + theme_classic() + geom_smooth(method = "lm")


###### OPENINGS
JoltsMerge <- jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
  filter(display_level.industry_code == 2) %>%
  filter(dataelement_text == "Job openings") %>%
  select(industry_text, Joltsvalue = value, date, dataelement_text)

ECIMerge <- eci %>% filter(period != "M13", seasonal == "S", periodicity_code == "Q") %>%
  filter(display_level.x == 2, estimate_code == "02") %>%
  select(industry_text, ECI_growth = value, date)

Merged <- JoltsMerge %>% filter(dataelement_text == "Quits") %>% left_join(ECIMerge, by=c("date","industry_text")) %>%
  filter(!is.na(ECI_growth)) %>%
  mutate(pandemic = (date >= "2021-01-01"))

Merged %>%
  ggplot(aes(Joltsvalue, ECI_growth, color=pandemic)) + geom_point() + theme_classic() +
  facet_wrap(~dataelement_text) + geom_smooth(method = "lm")


#THIS IS THE GRAPHIC
Merged %>%
  ggplot(aes(Joltsvalue, ECI_growth, color=pandemic)) + geom_point() + theme_classic() +
  facet_wrap(~industry_text, scales = "free") + geom_smooth(method = "lm")




industry_title
# Graphic Set 2: 
# Services and Goods Inflation Verus ECI Wages
ECIMerge <- eci %>% filter(period != "M13", seasonal == "S", periodicity_code == "Q") %>%
  filter(industry_title %in% c("Goods-producing", "Service-providing"),
         estimate_code == "02", owner_text == "Private industry workers") %>%
  filter(date > 2021-12-01)
# industry_title is the name

#SET UP DATA:
cpi <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
  mutate(Wchange3 = (Pchange3*weight)/100) %>%
  mutate(Wchange3a = (1 + Wchange3)^4 - 1) %>%
  mutate(Pchange6 = (value/lag(value, 6)-1)) %>%
  mutate(Wchange6 = (Pchange3*weight)/100) %>%
  mutate(Wchange6a = (1 + Wchange3)^2 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  ungroup()

cpiGS <- cpi %>% filter(item_name %in% c("Services less energy services", "Commodities less food and energy commodities")) %>%
  mutate(merge_name = case_when(
    item_name == "Services less energy services" ~ "Service-providing",
    item_name == "Commodities less food and energy commodities" ~ "Goods-producing")) %>%
  select(date, merge_name, Wchange3)

ECIMerge2 <- ECIMerge %>% left_join(cpiGS, by=c("date"="date", "industry_title"="merge_name")) %>%
  mutate(value = value/100)

ECIMerge2 %>% ggplot(aes(value, Wchange3)) + geom_point() + theme_classic() +
  facet_wrap(~industry_title, scales = "free") + geom_smooth(method="lm") +
  labs(x = "3-month change in relevant ECI compensation", y="3-month change in relevant CPI inflation")


a <- ECIMerge2 %>% filter(industry_title == "Service-providing") %>%
  lm(Wchange3 ~ value, data=.)
summary(a)

b <- ECIMerge2 %>%
  lm(Wchange3 ~ value + industry_title, data=.)
summary(b)



### ALL wages

ECIMerge <- eci %>% filter(period != "M13", seasonal == "S", periodicity_code == "Q") %>%
  # Wages and salaries for Private industry workers in All industries and occupations, 3-month percent change
  filter(series_id == "CIS2020000000000Q")


cpiGS <- cpi %>% filter(item_name %in% c("Services less energy services")) %>%
  mutate(merge_name = case_when(
    item_name == "Services less energy services" ~ "Service-providing",
    item_name == "Commodities less food and energy commodities" ~ "Goods-producing")) %>%
  select(date, merge_name, Wchange3)

ECIMerge2 <- ECIMerge %>% left_join(cpiGS, by=c("date"="date")) %>%
  mutate(value = value/100) %>% mutate(lagged = lag(Wchange3,1))

ECIMerge2 %>% ggplot(aes(value, lagged)) + geom_point() + theme_classic() + geom_smooth(method="lm") +
  labs(x = "3-month change in relevant ECI compensation", y="3-month change in relevant CPI inflation")


# Break into housing and not housing
CPI_merge <- cpi %>% filter(item_name %in% c("Services less energy services", "Shelter")) %>%
  select(date, item_name, Wchange3) %>%
  pivot_wider(names_from = item_name, values_from = Wchange3) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Rest of core services`), names_to = "item_name", values_to = "Wchange3") %>%
  mutate(item_name = factor(item_name, levels = c("Shelter", "Rest of core services")))

ECIMerge2 <- ECIMerge %>% left_join(CPI_merge, by=c("date"="date")) %>%
  mutate(value = value/100) %>% mutate(lagged = lag(value,2))

ECIMerge2 %>% ggplot(aes(lagged, Wchange3)) + geom_point() + theme_classic() + geom_smooth(method="lm") + facet_wrap(~item_name) +
  labs(x = "3-month change in wages and salaries for private industry workers, ECI", y="3-month change in relevant CPI contribution to inflation",
       title="CPI inflation versus overall ECI wages, Wages lagged 6 months, 2001-2022", caption="Preliminary results, Mike Konczal, Roosevelt Institute")
ggsave("ECI_shelter_services_lagged.png", dpi="retina", width = 12, height=6.75, units = "in")

ECIMerge2 %>% ggplot(aes(value, Wchange3)) + geom_point() + theme_classic() + geom_smooth(method="lm") + facet_wrap(~item_name) +
  labs(x = "3-month change in wages and salaries for private industry workers, ECI", y="3-month change in relevant CPI contribution to inflation",
       title="CPI inflation versus overall ECI wages,  2001-2022", caption="Preliminary results, Mike Konczal, Roosevelt Institute")
ggsave("ECI_shelter_services.png", dpi="retina", width = 12, height=6.75, units = "in")

ECIMerge2LM <- ECIMerge2 %>% pivot_wider(names_from = item_name, values_from = Wchange3) %>% rename(rest_services = `Rest of core services`)
a <- lm(rest_services ~ lagged, data=ECIMerge2LM)
summary(a)
a <- lm(Shelter ~ lagged, data=ECIMerge2LM)
summary(a)

%>%
  mutate(num_label = round(100*Wchange1a, 2)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "Monthly Core Services Contribution to Inflation, Annualized",
       subtitle = "Services are up this month, driven largely but not entirely by housing.",
       caption ="BLS, CPI, 2022 Weights, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_x_date(date_labels = "%b %Y", breaks = "2 month")

ggsave("graphics/g3_test_services.png", dpi="retina", width = 12, height=6.75, units = "in")


#### DASHBOARD ####

eci_data <- eci %>% filter(series_id == "CIS1020000000000Q") %>%
  select(value, date) %>% mutate( type = "ECI Wage Growth") %>%
  mutate(eci_A = (1+value/100)^4-1) %>% select(-value)

jolts %>% filter(series_id == "JTS100000000000000QUR") %>%
  left_join(eci_data, by="date") %>% filter(!is.na(eci_A)) %>% mutate(pandemic = (year > 2020)) %>%
  ggplot(aes(value,eci_A, color=pandemic)) + geom_point() + theme_classic() + geom_path()


jolts %>% filter(series_id == "JTS100000000000000QUR") %>%
  left_join(eci_data, by="date") %>% filter(!is.na(eci_A)) %>% mutate(pandemic = (year > 2020)) %>%
  ggplot(aes(value,eci_A, color=pandemic)) + geom_point() + theme_classic() + geom_smooth(method = "lm")