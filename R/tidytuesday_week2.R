# TidyTuesday: Week 2 (2018-04-10) ----------------------------------------
# Author: Frank Farach (@frankfarach)
# TidyTuesday repo: https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(readxl)

df <- "data/tidy_tuesday_week2.xlsx" %>%
  read_excel() %>%
  gather(position, salary, Cornerback:`Wide Receiver`) %>%
  mutate(
    # Only need the last two digits of year
    year = str_match(year, "\\d\\d$") %>% as.integer(),
    # DE and DT not in data; included ST which was not in original
    pos_abbr = case_when(
      position == "Cornerback" ~ "CB",
      position == "Defensive Lineman" ~ "DL",
      position == "Linebacker" ~ "LB",
      position == "Offensive Lineman" ~ "OL",
      position == "Quarterback" ~ "QB",
      position == "Running Back" ~ "RB",
      position == "Safety" ~ "S",
      position == "Special Teamer" ~ "ST",
      position == "Tight End" ~ "TE",
      position == "Wide Receiver" ~ "WR"
    ),
    # Ordering of levels controls order of facet panels
    pos_abbr = factor(
      pos_abbr,
      levels = c("RB", "QB", "OL", "TE", "WR",
                 "CB", "DL", "LB", "S", "ST"),
      ordered = TRUE
    ),
    # Just in case we want to facet by type of position
    # Special teams are not exclusively offense or defense
    pos_type = case_when(
      pos_abbr %in% c("RB", "QB", "OL", "TE", "WR") ~ "Offense",
      pos_abbr %in% c("CB", "DL", "DT", "LB", "S") ~ "Defense",
      pos_abbr %in% "ST" ~ "Special Teams"
    )
  )

# Top 16 highest-paid players in each position each year
df16 <- df %>%
  group_by(year, pos_abbr) %>%
  top_n(16, salary)

# Plot
df16 %>%
  ggplot(aes(x = year,
             # Only need millions
             y = salary / 1e6,
             fill = pos_abbr)) +
  geom_point(alpha = 0.1) +
  geom_smooth(
    method = "loess",
    # Default span was not as wiggly as original
    span = .5,
    se = FALSE,
    color = "orangered"
  ) +
  facet_wrap(~ pos_abbr, nrow = 2) +
  # No legend
  scale_fill_discrete(guide = FALSE) +
  # Hack: Years aren't dollars, but the prefix argument is handy
  scale_x_continuous(labels = scales::dollar_format(prefix = "'")) +
  # Lower limit ensures 0 is always shown on y axis
  scale_y_continuous(
    labels = scales::dollar_format(suffix = "M"),
    breaks = seq(0, 40, 5),
    limits = c(0, NA)
  ) +
  labs(
    title = "The average pay for top running backs has stalled",
    subtitle = "Average cap value of 16 highest-paid players in each position",
    x = "",
    y = "Average cap value",
    caption = "Sources: FiveThirtyEight, ESPN Stats & Information Group  |  Graphic: @frankfarach"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.text = element_text(color = "#999999"),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 13, 
                                 color = "gray30",
                                 vjust = 0.5)
  )

ggsave("plots/tt2_nfl_avg_cap.png", dpi = 300)