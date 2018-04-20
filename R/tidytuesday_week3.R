# TidyTuesday: Week 3 (2018-04-20) ----------------------------------------
# Author: Frank Farach (@frankfarach)
# TidyTuesday repo: https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(readxl)
library(skimr)
library(tweenr)
library(gganimate)

# Import data to tibble
df <- "data/global_mortality.xlsx" %>% 
  read_excel() 

# Quick EDA - note missing values for terrorism and conflict
skim(df)

# Tidy, clean country names, and aggregate for plot
share_by_yr_cause <- df %>% 
  gather(cause, 
         share, 
         `Cardiovascular diseases (%)`:`Terrorism (%)`) %>% 
  mutate(cause = str_remove(cause, "[[:space:]]\\(%\\)"),
         year = as.integer(year)) %>% 
  group_by(year, cause) %>% 
  summarize(avg_share = mean(share, na.rm = TRUE))

# Renaming some columns for convenience in tweenr
# We want cause of death to be the constant grouping across animation frames
# `ease` will tell tweenr what interpolation method to use; nothing fancy here
mydf <- share_by_yr_cause %>% 
  rename(x = avg_share, y = cause, time = year, id = cause) %>%
  mutate(ease = "linear")

# The magic: Animation by interpolation!
# nframes: We have 27 years of data and want, say, 2 frames per year
# round the time column because it has interpolated values for year
# Join the aggregated data to the "tweened" dataset to get original
# columns alongside the 
mydf_tween <- tween_elements(mydf, "time", "id", "ease", nframes = 54) %>%
  mutate(year = round(time), cause = .group) %>%
  left_join(share_by_yr_cause, by = c("year", "cause"))

# Getting the year to display in the title is tricky because there are
# multiple values of .frame per year, yet we want the frame aesthetic
# to be .frame. I chose to concatenate year and frame, but would love
# to find a better way.
p <- ggplot(mydf_tween, aes(x = x/100, 
                            y = reorder(cause, avg_share, na.rm = TRUE), 
                            frame = paste0(year, " (#", .frame, ")"))) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(title = "Share of deaths by cause, World,",
       subtitle = "Data refers to the specific cause of death, which is distinguished from risk\nfactors for death, such as air pollution, diet and other lifestyle factors.\nThis is shown by cause of death as the percentage of total deaths.",
       x = "Share of deaths (%)",
       y = NULL,
       caption = "Sources: IHME, Global Burden of Disease & ourworldindata.org | Graphic: @frankfarach") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 9),
        axis.text = element_text(size = 8))

# Requires ImageMagick
g <- gganimate(p, "plots/tt_week3.gif", saver = "gif", interval = 0.05)