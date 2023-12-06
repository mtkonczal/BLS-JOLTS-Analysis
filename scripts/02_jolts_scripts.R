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
library(hrbrthemes)



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


# Function for downloading data from FRED
prep_FRED_data <- function(x) {
  getSymbols(x, src = "FRED")
  df <- get(x)
  df <- as_tibble(data.frame(Date = index(df))) %>%
    bind_cols(setNames(list(as.numeric(df[, x])), x))
  colnames(df) <- tolower(colnames(df))
  return(df)
}



draw_three_headlines <- function(jolts, graphic_title = "Default title", date_breaks_skip = 6){
  
  topline_series <- c("JTS100000000000000HIR", "JTS100000000000000QUR", "JTS100000000000000JOR")
  
  date_breaks <- sort(na.omit(unique(jolts$date)), decreasing = TRUE)
  date_breaks <-  date_breaks[seq(1, length(date_breaks), by = date_breaks_skip)]
  
  baseline_2019 <- jolts %>%
    filter(
      period != "M13", seasonal == "S",
      series_id %in% topline_series
    ) %>%
    mutate(value = value / 100) %>%
    filter(year == 2019) %>%
    group_by(series_id) %>%
    summarize(avg_2019 = mean(value)) %>%
    ungroup()
  
  jolts %>%
    filter(
      period != "M13", seasonal == "S", ratelevel_code == "R",
      series_id %in% topline_series
    ) %>%
    mutate(value = value / 100) %>%
    filter(year >= 2021) %>%
    left_join(baseline_2019, by = "series_id") %>%
    mutate(dataelement_textF = factor(dataelement_text, levels = c("Hires", "Quits", "Job openings"))) %>%
    ggplot(aes(date, value, color=dataelement_textF)) +
    geom_line(size = 1.2) +
    facet_wrap(~dataelement_textF, scales = "free") +
    theme_lass +
    geom_line(aes(date, avg_2019), color = "#f7dc6f", linetype = "dashed", size = 1.2) +
    labs(
      title = graphic_title,
      subtitle = "Blue line is actual monthly value, total private; Red line is average value across 2019",
      caption = "BLS, JOLTS, seasonally-adjusted, author's calculations, Mike Konczal, Roosevelt Institute"
    ) +
    scale_y_continuous(labels = percent) +
    scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks) +
    scale_color_brewer(palette = "Dark2") +
    theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 19)) +
    theme(
      plot.title = element_text(size = 22, face = "bold"),
      strip.text = element_text(face = "bold", size = rel(2))
    )
}


draw_beveridge_curve <- function(jolts, graphic_title, type = "Job Openings") {
  
  if(type == "Job Openings")
    type_id <- "JTS000000000000000JOR"
  else if(type == "Quits")
    type_id <- "JTS100000000000000QUR"
  else
    return("Incorrect type of JOLTS data")
  
  jolts %>%
    filter(series_id == type_id) %>%
    mutate(maxdate = date == max(date)) %>%
    mutate(maxdate = if_else(maxdate == TRUE, date, as.Date(NA))) %>%
    mutate(maxdate = as.character(format(maxdate, "%b\n%Y"))) %>%
    mutate(decade = floor(year / 10), 0) %>%
    mutate(decade = decade * 10, decade = paste(as.character(decade), "s", sep = ""), decade = as.factor(decade)) %>%
    mutate(value = value / 100) %>%
    ggplot(aes(x = urate, y = value, color = decade, label = maxdate)) +
    geom_path() +
    geom_point(size = 0.6, show.legend = FALSE) +
    geom_point(aes(urate[date == max(date)], value[date == max(date)]), size = 2, show.legend = FALSE) +
    labs(
      title = graphic_title,
      subtitle = paste0(type, " rate verus unemployment rate."),
      x = "Unemployment Rate", y = paste0(type, " Rate"),
      caption = "BLS, JOLTS, seasonally-adjusted, author's calculations, Mike Konczal, Roosevelt Institute"
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    theme_lass +
    theme(
      axis.text.x = element_text(size = 15, face = "bold"),
      axis.text.y = element_text(size = 15, face = "bold"),
      axis.title.x = element_text(size = 15, margin = margin(t = 20, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(size = 14, angle = 90, color = "white", vjust = 3)
    ) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(legend.position = c(0.7, 0.8)) +
    geom_text_repel(show.legend = FALSE, size = 4)
}


job_openings_by_industry <- function(jolts, graphic_title = "Default title"){
  baseline_2019i <- jolts %>% filter(period != "M13", seasonal == "S", ratelevel_code == "R") %>%
    filter(display_level.industry_code == 2 | series_id == "JTS100000000000000JOR") %>%
    filter(dataelement_text == "Job openings") %>%
    filter(industry_text != "Federal") %>%
    mutate(value = value/100) %>%
    filter(year == 2019) %>%
    group_by(industry_text) %>%
    summarize(avg_2019 = mean(value)) %>%
    ungroup()
  
  
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
    labs(title=graphic_title,
         subtitle="Red line is average across 2019",
         caption="BLS, JOLTS, seasonally-adjusted, author's calculations, Mike Konczal, Roosevelt Institute") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L))
}


# Want to clean this ups
draw_bc_pc_curve <- function(jolts, graphic_title = "Hello", type = "Job openings", cpi_lag = 3, nonlinear = TRUE, v_u = TRUE){
  
  if (type == "Job openings") {
    type_id <- "JTS000000000000000JOR"
  } else if (type == "Quits") {
    type_id <- "JTS100000000000000QUR"
  } else {
    return("Incorrect type of JOLTS data")
  }
  
  
  jolts_pc <- jolts %>%
    filter(series_id == type_id) %>%
    mutate(value = value/100)
  
  type_label <- type
  if(type == "Job openings" & v_u == TRUE){
    jolts_pc$value <- jolts_pc$value / jolts_pc$urate
    type_label <- "Vacancy-to-unemployment"
  }
  
  getSymbols("CPILFESL", src = "FRED")
  core_cpi_df <- data.frame(date = index(CPILFESL), core_cpi = coredata(CPILFESL))
  core_cpi_tibble <- as_tibble(core_cpi_df) %>%
    rename(core_cpi = CPILFESL) %>%
    mutate(core_cpi = core_cpi / lag(core_cpi, cpi_lag), core_cpi = core_cpi^(12/cpi_lag) - 1)
  
  jolts_pc <- jolts_pc %>% left_join(core_cpi_tibble, by = "date")
  
  if(nonlinear){
    jolts_pc_lm <- lm(core_cpi ~ I(value^2), data = jolts_pc)
  } else
    jolts_pc_lm <- lm(core_cpi ~ value, data = jolts_pc)
  
  jolts_pc$predicted <- predict(jolts_pc_lm, jolts_pc)
  
  jolts_pc %>%
    mutate(recent = date > max(date) %m-% months(4)) %>%
    mutate(recent_v_u = if_else(recent, value, as.double(NA))) %>%
    mutate(label = if_else(date == max(date), format(date, "%b,\n%Y"), as.character(NA))) %>%
    mutate(color_pandemic = if_else(year(date) >= 2021, "2021-", "2000-2020")) %>%
    mutate(last_value = if_else(date == max(date), value, as.double(NA))) %>%
    ggplot(aes(value, core_cpi, label = label)) +
    geom_point(aes(color = color_pandemic)) +
    theme_lass +
    geom_line(aes(value, predicted), show.legend = FALSE) +
    geom_path(aes(recent_v_u, core_cpi), color = "pink") +
    geom_text_repel(color = "pink") +
    geom_point(aes(last_value, core_cpi), color = "pink", show.legend = FALSE) +
    scale_y_continuous(labels = percent) +
    labs(
      title = graphic_title,
      subtitle = paste0("3-month core CPI change versus", type_label,", 2000 - Current"),
      x = type_label, y = "3-month core CPI change",
      caption = "BLS, Seasonally Adjusted, Mike Konczal, Roosevelt Institute"
    ) +
    theme(
      axis.text.x = element_text(size = 15, face = "bold"),
      axis.text.y = element_text(size = 15, face = "bold"),
      axis.title.x = element_text(size = 15, margin = margin(t = 20, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(size = 14, angle = 90, color = "white", vjust = 3)
    ) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(legend.position = c(0.7, 0.8))
}





draw_efficiency_curve <- function(jolts, graphic_title = "Default title", legend_length = 60){
  
  matching <- jolts %>% filter(series_id %in% c("JTS100000000000000HIR", "JTS100000000000000JOR")) %>%
    select(date, dataelement_text, value, urate) %>%
    pivot_wider(names_from = dataelement_text, values_from = value) %>%
    rename(job_openings = `Job openings`, hires = Hires) %>%
    mutate(alpha = 0.4) %>%
    mutate(matching_efficiency = hires/((urate^alpha)*job_openings^(1-alpha))) %>%
    mutate(matching_efficiencyN = matching_efficiency/matching_efficiency[date == "2019-12-01"])
  
  # This is how Domesh Blanchard and Summers define their NAIRU estimate:
  nairu_est <- matching %>%
    mutate(v_u_alpha = (job_openings/urate)^(1-alpha),
           v_u_alpha_anchor = v_u_alpha[date == "2019-12-01"],
           nairu = hires/(matching_efficiency*v_u_alpha_anchor))
  
  
  date_breaks <- sort(unique(nairu_est$date), decreasing = TRUE)
  date_breaks <- date_breaks[seq(1, length(date_breaks), legend_length)]
  
  matching %>% 
    ggplot(aes(date, matching_efficiencyN, color="myline")) +
    geom_line(size=1.2) +
    theme_lass +
    labs(title = graphic_title,
         subtitle = TeX(r'(Line is matching efficency = $\frac{u^\alpha_t * v^{(1-\alpha)}_t}{h^{t}}$. With alpha = 0.4 and normalized to December 2019 = 1.)'),
         caption = "See: 'Bad News for the Fed from the Beveridge Space' (Blanchard, Domash, and Summers) for more. Mike Konczal, Roosevelt Institute") +
    scale_color_manual(values = c(myline="#E57A77")) +
    scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks)
}




# List of variables to download
fred_variables <- c("UNRATE", "CPILFESL", "JTSJOR", "JTSQUR", "PCEPILFE", "JTSQUL", "PAYEMS")

# Download process, doing some manipulations so the characters become variable names
for (i in fred_variables) {
  prep_FRED_data(i)
  data <- prep_FRED_data(i)
  assign(tolower(i), data, envir = .GlobalEnv)
  rm(data)
}

pc_data <- get(tolower(fred_variables[1]))
# Joining them all into one dataset. This dataset is monthly, with quarterly values missing dates as NA.
for (i in fred_variables[-1]) {
  pc_data <- full_join(pc_data, get(tolower(i)), by = "date")
}


pc_data <- pc_data %>%
  arrange(date) %>%
  rename(
    core_cpi = cpilfesl,
    job_openings = jtsjor,
    quits = jtsqur,
    core_pce = pcepilfe,
    quits_level = jtsqul,
    employment_level = payems
  ) %>%
  mutate(
    core_cpi = (core_cpi / lag(core_cpi, 3))^4 - 1,
    core_pce = (core_pce / lag(core_pce, 3))^4 - 1,
    v_u = job_openings / unrate,
    quits = quits / 100,
    my_quits = quits_level / employment_level
  ) %>%
  na.omit() %>%
  select(-core_cpi)


jo_pc_lm <- lm(core_pce ~ my_quits + I(my_quits^2), data = pc_data)
summary(jo_pc_lm)

# jolts_pc_lm <- lm(core_cpi ~ v_u + supply_chains, data=jolts_pc)
# summary(jolts_pc_lm)


pc_data$predicted <- predict(jo_pc_lm, pc_data)

pc_data %>%
  mutate(recent = date > max(date) %m-% months(4)) %>%
  mutate(recent_quits = if_else(recent, my_quits, as.double(NA))) %>%
  mutate(label = if_else(date == max(date), format(date, "%b,\n%Y"), as.character(NA))) %>%
  mutate(color_pandemic = if_else(year(date) >= 2021, "2021-", "2000-2020")) %>%
  mutate(last_value = if_else(date == max(date), my_quits, as.double(NA))) %>%
  ggplot(aes(my_quits, core_pce, label = label)) +
  theme_lass +
  geom_point(aes(color = color_pandemic)) +
  geom_line(aes(my_quits, predicted), show.legend = FALSE) +
  geom_path(aes(recent_quits, core_pce), color = "palegreen") +
  geom_text_repel(color = "palegreen") +
  geom_point(aes(last_value, core_pce), color = "palegreen", show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "Back to where we started?",
    subtitle = "3-month core PCE change versus Quits, 2000 - Current, Regression line is quits + quits^2.",
    x = "Quits Rate", y = "3-month core PCE change",
    caption = "BLS, Seasonally Adjusted, Quits rate taken as quits level over CES employment level. Mike Konczal, Roosevelt Institute"
  ) +
  theme(
    axis.text.x = element_text(size = 15, face = "bold"),
    axis.text.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(size = 15, margin = margin(t = 20, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 14, angle = 90, color = "white", vjust = 3)
  ) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(legend.position = c(0.7, 0.8))

ggsave("graphics/jolts_PC_quits.png", width = 12, height = 8, dpi = "retina")
