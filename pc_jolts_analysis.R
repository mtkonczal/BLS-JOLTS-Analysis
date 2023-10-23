# TKTK
setwd("/Users/mkonczal/Documents/GitHub/BLS-JOLTS-Analysis/")
library(janitor)
library(tidyverse)
library(ggtext)
library(hrbrthemes)
library(scales)
library(ggrepel)
library(quantmod)

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


#### Phillips Curve - Overall ####
jolts_pc <- jolts %>% filter(series_id == "JTS000000000000000JOR") %>%
  mutate(v_u = value/urate) %>%
  select(date, v_u)

getSymbols("CPILFESL", src = "FRED")
core_cpi_df <- data.frame(date=index(CPILFESL), core_cpi=coredata(CPILFESL))
core_cpi_tibble <- as_tibble(core_cpi_df) %>% rename( core_cpi = CPILFESL) %>%
  mutate(core_cpi = core_cpi/lag(core_cpi,3), core_cpi = core_cpi^4-1)

jolts_pc <- jolts_pc %>% left_join(core_cpi_tibble, by="date")
supply <- read_csv("data/supply_chains.csv") %>% select(-date_full)
jolts_pc <- jolts_pc %>% left_join(supply, by="date")

jolts_pc_lm <- lm(core_cpi ~ I(v_u^2), data=jolts_pc)
summary(jolts_pc_lm)

#jolts_pc_lm <- lm(core_cpi ~ v_u + supply_chains, data=jolts_pc)
#summary(jolts_pc_lm)


jolts_pc$predicted <- predict(jolts_pc_lm, jolts_pc)

jolts_pc %>% mutate(recent = date > max(date) %m-% months(4)) %>%
  mutate(recent_v_u = if_else(recent, v_u, as.double(NA))) %>%
  mutate(label = if_else(date==max(date), format(date, "%b,\n%Y"), as.character(NA))) %>%
  mutate(color_pandemic = if_else(year(date)>=2021, "2021-","2000-2020")) %>%
  mutate(last_value = if_else(date==max(date),v_u, as.double(NA))) %>%
  
  ggplot(aes(v_u,core_cpi,label=label)) + geom_point(aes(color=color_pandemic)) + theme_lass +
  geom_line(aes(v_u, predicted), show.legend = FALSE) +
  geom_path(aes(recent_v_u,core_cpi), color="pink") + geom_text_repel(color="pink") +
  geom_point(aes(last_value,core_cpi), color="pink",show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  labs(title="Sliding off the Nonlinear Beveridge-Phillips Curve",
       subtitle="3-month core CPI change versus Vacancies-to-Unemployment Ratio, 2000 - Current",
       x="Vacancy-to-unemployment ratio", y="3-month core CPI change",
       caption="BLS, Seasonally Adjusted, Mike Konczal, Roosevelt Institute") +
  theme(axis.text.x = element_text(size=15, face="bold"),
        axis.text.y = element_text(size=15, face="bold"),
        axis.title.x = element_text(size=15, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size=14, angle = 90, color="white", vjust = 3)) +
  theme(panel.grid.minor = element_blank()) + theme(panel.grid.major = element_blank()) +
  theme(legend.position = c(0.7,0.8))

ggsave("graphics/jolts_PC.png", width = 12, height=12, dpi="retina")


#### Same but with quits rate ####

#### Phillips Curve - Overall ####
jolts_pc <- jolts %>% filter(series_id == "JTS000000000000000QUR") %>%
  mutate(quits = value) %>%
  select(date, quits)

jolts_pc <- jolts %>% filter(series_id == "JTS000000000000000QUL") %>%
  mutate(quits = value) %>%
  select(date, quits)

getSymbols("PAYEMS", src = "FRED")
employ <- data.frame(date=index(PAYEMS), core_cpi=coredata(PAYEMS))


getSymbols("CPILFESL", src = "FRED")
core_cpi_df <- data.frame(date=index(CPILFESL), core_cpi=coredata(CPILFESL))
core_cpi_tibble <- as_tibble(core_cpi_df) %>% rename( core_cpi = CPILFESL) %>%
  mutate(core_cpi = core_cpi/lag(core_cpi,3), core_cpi = core_cpi^4-1)

jolts_pc <- jolts_pc %>% left_join(core_cpi_tibble, by="date")
supply <- read_csv("data/supply_chains.csv") %>% select(-date_full)
jolts_pc <- jolts_pc %>%
  left_join(supply, by="date") %>%
  left_join(employ, by="date") %>%
  mutate(quits = quits/PAYEMS) 

jolts_pc_lm <- lm(core_cpi ~ quits + I(quits^2), data=jolts_pc)
summary(jolts_pc_lm)

#jolts_pc_lm <- lm(core_cpi ~ v_u + supply_chains, data=jolts_pc)
#summary(jolts_pc_lm)


jolts_pc$predicted <- predict(jolts_pc_lm, jolts_pc)

jolts_pc %>% mutate(recent = date > max(date) %m-% months(4)) %>%
  mutate(recent_quits = if_else(recent, quits, as.double(NA))) %>%
  mutate(label = if_else(date==max(date), format(date, "%b,\n%Y"), as.character(NA))) %>%
  mutate(color_pandemic = if_else(year(date)>=2021, "2021-","2000-2020")) %>%
  mutate(last_value = if_else(date==max(date),quits, as.double(NA))) %>%
  
  ggplot(aes(quits,core_cpi,label=label)) + geom_point(aes(color=color_pandemic)) + theme_lass +
  geom_line(aes(quits, predicted), show.legend = FALSE) +
  geom_path(aes(recent_quits,core_cpi), color="white") + geom_text_repel(color="white") +
  geom_point(aes(last_value,core_cpi), color="white",show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  labs(title="Sliding off the Nonlinear Beveridge-Phillips Curve",
       subtitle="3-month core CPI change versus Quits Rate, 2000 - Current",
       x="Quit Rate", y="3-month core CPI change",
       caption="BLS, Seasonally Adjusted, Mike Konczal, Roosevelt Institute") +
  theme(axis.text.x = element_text(size=15, face="bold"),
        axis.text.y = element_text(size=15, face="bold"),
        axis.title.x = element_text(size=15, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size=14, angle = 90, color="white", vjust = 3)) +
  theme(panel.grid.minor = element_blank()) + theme(panel.grid.major = element_blank()) +
  theme(legend.position = c(0.7,0.8)) +
  scale_x_continuous(labels = percent)

ggsave("graphics/jolts_PC_quits.png", width = 12, height=12, dpi="retina")
















#### Phillips Curve - Factors ####
jolts_pc <- jolts %>% filter(series_id == "JTS000000000000000JOR") %>%
  mutate(v_u = value/urate) %>%
  select(date, v_u)

load("data/last_cpi_data.RData")

core_cpi_df <- data.frame(date=index(CPILFESL), core_cpi=coredata(CPILFESL))
core_cpi_tibble <- as_tibble(core_cpi_df) %>% rename( core_cpi = CPILFESL) %>%
  mutate(core_cpi = core_cpi/lag(core_cpi,3), core_cpi = core_cpi^4-1)

jolts_pc <- jolts_pc %>% left_join(core_cpi_tibble, by="date")

jolts_pc_lm <- lm(core_cpi ~ I(v_u^2), data=jolts_pc)
summary(jolts_pc_lm)
jolts_pc$predicted <- predict(jolts_pc_lm, jolts_pc)

jolts_pc %>% mutate(recent = date > max(date) %m-% months(4)) %>%
  mutate(recent_v_u = if_else(recent, v_u, as.double(NA))) %>%
  mutate(label = if_else(date==max(date), format(date, "%b,\n%Y"), as.character(NA))) %>%
  mutate(color_pandemic = if_else(year(date)>=2021, "2021-","2000-2020")) %>%
  mutate(last_value = if_else(date==max(date),v_u, as.double(NA))) %>%
  
  ggplot(aes(v_u,core_cpi,label=label)) + geom_point(aes(color=color_pandemic)) + theme_lass +
  geom_line(aes(v_u, predicted), show.legend = FALSE) +
  geom_path(aes(recent_v_u,core_cpi), color="pink") + geom_text_repel(color="pink") +
  geom_point(aes(last_value,core_cpi), color="pink",show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  labs(title="Sliding off the Nonlinear Beveridge-Phillips Curve",
       subtitle="3-month core CPI change versus Vacancies-to-Unemployment Ratio, 2000 - Current",
       x="Vacancy-to-unemployment ratio", y="3-month core CPI change",
       caption="BLS, Seasonally Adjusted, Mike Konczal, Roosevelt Institute") +
  theme(axis.text.x = element_text(size=15, face="bold"),
        axis.text.y = element_text(size=15, face="bold"),
        axis.title.x = element_text(size=15, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size=14, angle = 90, color="white", vjust = 3)) +
  theme(panel.grid.minor = element_blank()) + theme(panel.grid.major = element_blank()) +
  theme(legend.position = c(0.7,0.8))

ggsave("graphics/jolts_PC.png", width = 12, height=12, dpi="retina")




nhs <-
  cpi_data %>% filter(date > "1999-12-01", seasonal == "S", item_name %in% c("Services less energy services", "Shelter", "Commodities less food and energy commodities")) %>%
  mutate(item_name = str_replace_all(item_name, "Commodities less food and energy commodities","Core_goods")) %>%
  select(date, item_name, value, weight) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Pchange1a = (1 + Pchange1)^12 - 1) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  ungroup() %>%
  group_by(date) %>%
  summarize(nhsWP1 = Wchange1[item_name == "Services less energy services"] - Wchange1[item_name == "Shelter"],
            nhs_weight = weight[item_name == "Services less energy services"] - weight[item_name == "Shelter"],
            nhs_weight = nhs_weight/100
  ) %>%
  ungroup() %>%
  mutate(nhsWP1A = nhsWP1/nhs_weight) %>%
  mutate(nhsWP1A = (nhsWP1A+1)^12-1) %>%
  mutate(index = nhsWP1/nhs_weight+1) %>% filter(!is.na(index)) %>%
  mutate(index = cumprod(index)) %>%
  select(date, cpi_value = nhsWP1A) %>% mutate(type = "Non-housing services")
  
  
nhs


factor_df <- cpi_data %>% filter(seasonal == "S", item_name == "Commodities less food and energy commodities") %>%
  mutate(cpi_value = value/lag(value,1), cpi_value = cpi_value^12-1) %>%
  select(date, cpi_value) %>% mutate(type = "Core goods") %>%
  rbind(nhs) %>%
  left_join(jolts_pc, by="date") %>%
  na.omit()
  
a <- lm(cpi_value ~ I(v_u^2), data=factor_df[type=="Non-housing services"])
summary(a)
factor_df$predicted <- predict(a, factor_df)

factor_df %>%
  mutate(recent = date > max(date) %m-% months(4)) %>%
  mutate(recent_v_u = if_else(recent, v_u, as.double(NA))) %>%
  mutate(label = if_else(date==max(date), format(date, "%b,\n%Y"), as.character(NA))) %>%
  mutate(color_pandemic = if_else(year(date)>=2021, "2021-","2000-2020")) %>%
  mutate(last_value = if_else(date==max(date),v_u, as.double(NA))) %>%
  
  ggplot(aes(v_u,cpi_value,label=label)) + geom_point(aes(color=color_pandemic)) + theme_lass + facet_wrap(~type) +
  geom_line(aes(v_u, predicted), show.legend = FALSE) +
  geom_path(aes(recent_v_u,cpi_value), color="pink") + geom_text_repel(color="pink") +
  geom_point(aes(last_value,cpi_value), color="pink",show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  labs(title="Sliding off the Nonlinear Beveridge-Phillips Curve",
       subtitle="3-month core CPI change versus Vacancies-to-Unemployment Ratio, 2000 - Current",
       x="Vacancy-to-unemployment ratio", y="3-month core CPI change",
       caption="BLS, Seasonally Adjusted, Mike Konczal, Roosevelt Institute") +
  theme(axis.text.x = element_text(size=15, face="bold"),
        axis.text.y = element_text(size=15, face="bold"),
        axis.title.x = element_text(size=15, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size=14, angle = 90, color="white", vjust = 3)) +
  theme(panel.grid.minor = element_blank()) + theme(panel.grid.major = element_blank()) +
  theme(legend.position = c(0.7,0.8))