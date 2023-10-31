# TKTK
library(janitor)
library(tidyverse)
library(ggtext)
library(hrbrthemes)
library(scales)
library(ggrepel)
library(quantmod)
library(scales)
library(latex2exp)


source("scripts/01_read_jolts.R")
source("scripts/02_jolts_scripts.R")


headline_title <- "All down across"
draw_three_headlines(jolts, headline_title)
ggsave("graphics/three_measures.png", width = 12, height=6.75, dpi="retina")


# GRAPHIC 1: BEVERIDGE CURVE
bc_title <- "This thing"
draw_beveridge_curve(jolts, bc_title)
ggsave("graphics/jolts_bc.png", width = 10, height=6.75, dpi="retina")

bc_title <- "This thing"
draw_beveridge_curve(jolts, bc_title, "Quits")
ggsave("graphics/jolts_quits_bc.png", width = 12, height=6.75, dpi="retina")


jo_industries_title <- "This thing"
job_openings_by_industry(jolts, jo_industries_title)
ggsave("graphics/jolts_openings_industry.png", width = 12, height=6.75, dpi="retina")


draw_bc_pc_curve(jolts, "Yello", type = "Quits")
draw_bc_pc_curve(jolts, "My jo title", type = "Job openings")

matching_title <- "This didn't do anything."
draw_efficiency_curve(jolts, matching_title)
ggsave("graphics/jolts_matching.png", width = 12, height=8, dpi="retina")
