# Mailroom ----------------------------------------------------------------

library(tidyverse)

# Load --------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2025-08-19')

dat <- tuesdata$scottish_munros

# Do ----------------------------------------------------------------------

dat2 <- dat |>
  pivot_longer(
    cols = `1891`:`2021`,
    names_to = "Year",
    values_to = "Classification"
  ) |>
  select(Name, Year, Classification)


dat3 <- dat2 |>
  summarize(n = n(), .by = c(Classification, Year)) |>
  drop_na(Classification) |>
  mutate(delta = n - lag(n, 1), .by = Classification) |>
  drop_na(delta)

fin <- dat3 |>
  ggplot() +
  aes(x = Year, y = delta, fill = Classification) +
  geom_col(position = position_dodge2(), width = .8) +
  theme_bw(base_size = 16, base_family = "Barlow") +
  geom_text(
    aes(label = ifelse(delta <= 0, delta, str_c("+", delta))),
    nudge_x = ifelse(dat3$Classification == "Munro", -.21, .2),
    nudge_y = ifelse(dat3$delta < 0, -1, ifelse(dat3$delta > 0, 1, 0)),
    size = 5,
    fontface = "bold",
    family = "Barlow"
  ) +
  scale_fill_discrete(type = wesanderson::wes_palette("GrandBudapest1")) +
  scale_y_continuous(
    name = "Change in Size of Category",
    labels = c("-20", "-10", "0", "+10")
  ) +
  theme(
    plot.subtitle = element_text(
      color = "grey40"
    ),
    legend.position = "inside",
    legend.position.inside = c(.142, .209),
    legend.box.background = element_rect(colour = "black", linewidth = .75),
    plot.caption = element_text(vjust = 7),
    plot.margin = unit(c(5.5, 5.5, -12, 5.5), "pt"),
  ) +
  labs(
    title = "How do Scottish Mountain Classifications Change?",
    subtitle = "Not Consistently!",
    caption = "Kevin Baer - 8/19/2025"
  ) +
  ggview::canvas(10, 6)

# Save --------------------------------------------------------------------

ggview::save_ggplot(fin, "2025_08_19/2025_08_19_Munros_Image.png")
