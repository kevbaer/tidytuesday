# Mailroom ----------------------------------------------------------------

library(tidyverse)

# Load --------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2025-07-15')
funding <- tuesdata$bl_funding


f_dat <- funding |>
  select(year, contains("y2000_gbp")) |>
  rename_with(\(x) str_remove(x, "_y2000_gbp_millions"))

# Do ----------------------------------------------------------------------

finished <- f_dat |>
  select(-total) |>
  pivot_longer(cols = -year) |>
  mutate(
    name = factor(
      name,
      levels = c("gia", "services", "voluntary", "investment", "other")
    )
  ) |>
  ggplot() +
  aes(x = year, y = value, fill = fct_rev(fct_infreq(name))) +
  geom_col() +
  theme_bw(base_size = 16, base_family = "Barlow") +
  scale_x_continuous(
    name = "Year",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name = NULL,
    labels = scales::label_currency(prefix = "£", suffix = "m"),
    expand = expansion(mult = c(0, 0)),
    limits = c(0, 150)
  ) +
  scale_fill_manual(
    values = wesanderson::wes_palette("FantasticFox1")
  ) +
  labs(
    title = "British Library Funding has Fallen",
    subtitle = "1998-2023, Adjusted for Inflation",
    caption = "Kevin Baer - 7/20/2025",
    fill = "Source"
  ) +
  theme(
    plot.caption = element_text(vjust = 9),
    plot.caption.position = "plot",
    plot.margin = unit(c(5.5, 5.5, -12, 5.5), "pt"),
    panel.grid.minor.x = element_blank(),
    plot.subtitle = element_text(
      color = "grey40"
    )
  ) +
  ggview::canvas(10, 6)

# ggview::save_ggplot(finished, "tidytuesday/2025_07_15/2025_07_15_Library_Image.png")
