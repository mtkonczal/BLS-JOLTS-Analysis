
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(viridis)
library(quantmod)
library(broom)
library(httr)
library(data.table)
library(ggrepel)
library(hrbrthemes)
library(stargazer)


##### SET UP SOME THINGS #####
theme_lass <- theme_modern_rc(ticks = TRUE) + theme(
  legend.position = "none", legend.title = element_blank(),
  panel.grid.major.y = element_line(size = 0.5),
  panel.grid.minor.y = element_blank(),
  plot.title.position = "plot",
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  plot.title = element_text(size = 25, face = "bold"),
  plot.subtitle = element_text(size = 15, color = "white"),
  plot.caption = element_text(size = 10, face = "italic"),
  legend.text = element_text(size = 12),
  axis.text.y = element_text(size = 12, face = "bold"),
  axis.text.x = element_text(size = 12, face = "bold"),
  strip.text = element_text(face = "bold", color = "white", hjust = 0.5, size = 10),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  strip.background = element_blank()
) +
  theme(
    text = element_text(family = "Larsseit"),
    plot.title = element_text(family = "Larsseit"),
    plot.subtitle = element_text(family = "Larsseit"),
    plot.caption = element_text(family = "Larsseit"),
    strip.text = element_text(family = "Larsseit")
  )


# Function for downloading data from FRED
prep_FRED_data <- function(x) {
  getSymbols(x, src = "FRED")
  df <- get(x)
  df <- as_tibble(data.frame(Date = index(df))) %>%
    bind_cols(setNames(list(as.numeric(df[, x])), x))
  colnames(df) <- tolower(colnames(df))
  return(df)
}

# List of variables to download
fred_variables <- c("UNRATE", "CPILFESL", "JTSJOR", "JTSQUR", "PCEPILFE", "JTSQUL", "PAYEMS", "JTSJOL", "UNEMPLOY")

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
    job_openings = jtsjol,
    quits = jtsqur,
    core_pce = pcepilfe,
    quits_level = jtsqul,
    employment_level = payems
  ) %>%
  mutate(
    core_cpi = (core_cpi / lag(core_cpi, 3))^4 - 1,
    core_pce = (core_pce / lag(core_pce, 3))^4 - 1,
    v_u = job_openings / unemploy,
    u_v = unemploy / job_openings,
    quits = quits_level / employment_level
  ) %>%
  na.omit() %>%
  select(-core_cpi)




jo_pc_lm <- lm(core_pce ~ I(v_u^2), data = pc_data)
summary(jo_pc_lm)

# jolts_pc_lm <- lm(core_cpi ~ v_u + supply_chains, data=jolts_pc)
# summary(jolts_pc_lm)

pc_data$predicted <- predict(jo_pc_lm, pc_data)

pc_data %>%
  mutate(recent = date > max(date) %m-% months(6)) %>%
  mutate(recent_v_u = if_else(recent, v_u, as.double(NA))) %>%
  mutate(label = if_else(date == max(date) | date == max(date) %m-% months(5), format(date, "%b,\n%Y"), as.character(NA))) %>%
  mutate(color_pandemic = if_else(year(date) >= 2021, "2021-", "2000-2020")) %>%
  mutate(last_value = if_else(date == max(date), v_u, as.double(NA))) %>%
  ggplot(aes(v_u, core_pce, label = label)) +
  theme_lass +
  geom_point(aes(color = color_pandemic)) +
  geom_line(aes(v_u, predicted), show.legend = FALSE) +
  geom_path(aes(recent_v_u, core_pce), color = "pink") +
  geom_text_repel(color = "pink") +
  geom_point(aes(last_value, core_pce), color = "pink", show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Sliding off the Nonlinear Beveridge-Phillips Curve",
    subtitle = "3-month core PCE change versus vacancies-to-unemployment ratio, 2000 - Current. Line is pi versus (v/u)^2.",
    x = "Vacancy-to-unemployment ratio", y = "3-month core PCE change",
    caption = "BLS, seasonally adjusted. v/u is ratio of total nonfarm job opening level to unemployment level. Mike Konczal, Roosevelt Institute"
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

ggsave("graphics/jolts_PC_job_openings.png", width = 12, height = 8, dpi = "retina")





quits_pc_lm <- lm(core_pce ~ quits + I(quits^2), data = pc_data)
summary(quits_pc_lm)

# jolts_pc_lm <- lm(core_cpi ~ v_u + supply_chains, data=jolts_pc)
# summary(jolts_pc_lm)

pc_data$quits_predicted <- predict(quits_pc_lm, pc_data)


pc_data %>%
  mutate(recent = date > max(date) %m-% months(6)) %>%
  mutate(recent_v_u = if_else(recent, quits, as.double(NA))) %>%
  mutate(label = if_else(date == max(date) | date == max(date) %m-% months(5), format(date, "%b,\n%Y"), as.character(NA))) %>%
  mutate(color_pandemic = if_else(year(date) >= 2021, "2021-", "2000-2020")) %>%
  mutate(last_value = if_else(date == max(date), quits, as.double(NA))) %>%
  ggplot(aes(quits, core_pce, label = label)) +
  theme_lass +
  geom_point(aes(color = color_pandemic)) +
  geom_line(aes(quits, quits_predicted), show.legend = FALSE) +
  geom_path(aes(recent_v_u, core_pce), color = "#32FFFF") +
  geom_text_repel(color = "#32FFFF", size=7) +
  geom_point(aes(last_value, core_pce), color = "#32FFFF", show.legend = FALSE, size=2) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "It's Over: We're Well Within Historical Ranges of a Nonlinear Beveridge-Phillips Curve",
    subtitle = "3-month core PCE change versus quit rate, 2000 - Current. Line is inflation versus quits + (quits)^2.",
    x = "Quit rate", y = "3-month core PCE change",
    caption = "BLS, seasonally adjusted. v/u is ratio of total nonfarm job opening level to unemployment level. Mike Konczal, Roosevelt Institute"
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