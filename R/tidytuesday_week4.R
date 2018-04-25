  # TidyTuesday: Week 4 (2018-04-24) ----------------------------------------
  # Author: Frank Farach (@frankfarach)
  # TidyTuesday repo: https://github.com/rfordatascience/tidytuesday
  
  library(tidyverse)
  library(skimr)
  library(ggalt)
  
  # Import data to tibble
  df <- read_csv("data/week4_australian_salary.csv")
  
  # EDA
  df %>%
    skim(gender, individuals, average_taxable_income)
  
  # Organize data for plotting
  plot_df <- df %>%
    select(-X1,-gender_rank,-individuals) %>%
    spread(gender, average_taxable_income) %>%
    rename(female = Female, male = Male) %>%
    mutate(fdiff = female - male,
           fdiff_pct = as.integer(fdiff / male * 100)) %>%
    top_n(10, male) %>%
    arrange(desc(male))
  
  # ggalt - Dumbbell!
  # Adapted from blog post by Bob Rudis
  
  # Handy names for specific colors
  male_color <- "red"
  female_color <- "blue"
  dumbbell_color <- "#b2b2b2"
  sidebar_color <- "#efefe3"
  
  # Nudge top row
  label_vjust <- -1.4
 
  # Plot it! 
  ggplot() +
    geom_segment(
      data = plot_df,
      aes(
        y = fct_reorder(occupation, male),
        yend = occupation,
        x = 0,
        xend = max(male) * 1.04
      ),
      color = dumbbell_color,
      size = 0.15
    ) +
    geom_dumbbell(
      data = plot_df,
      aes(y = occupation, x = female, xend = male),
      size = 1.2,
      color = dumbbell_color,
      colour_x = female_color,
      colour_xend = male_color,
      dot_guide = FALSE
    ) +
    geom_text(
      data = filter(plot_df, occupation == "Neurosurgeon"),
      aes(x = female, y = occupation, label = "Women"),
      color = female_color,
      size = 3,
      vjust = label_vjust,
      fontface = "bold"
    ) +
    geom_text(
      data = filter(plot_df, occupation == "Neurosurgeon"),
      aes(x = male, y = occupation, label = "Men"),
      color = male_color,
      size = 3,
      vjust = label_vjust,
      fontface = "bold"
    ) +
    # Difference column
    geom_rect(
      data = plot_df,
      aes(
        xmin = max(male) * 1.07,
        xmax = max(male) * 1.2,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = sidebar_color
    ) +
    geom_text(
      data = plot_df,
      aes(
        label = paste0(fdiff_pct, "%"),
        y = occupation,
        x = max(male) * 1.13
      ),
      fontface = "bold",
      size = 3
    ) +
    geom_text(
      data = filter(plot_df, occupation == "Neurosurgeon"),
      aes(
        x = max(male) * 1.13,
        y = occupation,
        label = "Diff"
      ),
      color = "#7a7d7e",
      size = 3,
      vjust = label_vjust,
      fontface = "bold"
    ) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(
      y = NULL,
      title = "Women earn less than men in Australia's top-paying jobs",
      subtitle = "Gender pay gap in average taxable income (AUD), 2013-14",
      caption = "Source: Australian Government, https://bit.ly/2iTTCIy | Graphic: @frankfarach"
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(size = 8, color = "gray60", vjust = -1)
    )
  
  ggsave("plots/tt_week4.png")
