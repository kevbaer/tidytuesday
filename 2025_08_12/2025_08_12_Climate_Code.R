# Mailroom ----------------------------------------------------------------

library(tidyverse)
library(ggh4x)

# Load --------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2025-08-12')

dat <- tuesdata$attribution_studies

# Do ----------------------------------------------------------------------

Murica <- dat |>
  filter(iso_country_code == "USA")

Most_Studied_Countries <- dat |>
  filter(iso_country_code %in% c("AUS", "GBR", "CHN", "USA")) |>
  mutate(
    iso_country_code = factor(
      iso_country_code,
      levels = c("USA", "CHN", "GBR", "AUS")
    )
  ) |>
  summarize(count = n(), .by = c(iso_country_code, event_type)) |>
  mutate(percent = count / sum(count), .by = iso_country_code)


fin <- Most_Studied_Countries |>
  ggplot() +
  aes(y = reorder(event_type, count, \(x) -sum(x)), x = percent) +
  geom_col(
    position = position_dodge2(preserve = "single", reverse = TRUE),
    aes(fill = iso_country_code),
    width = 0.8
  ) +
  scale_x_continuous(
    name = "Percent of Studies on Topic",
    labels = scales::label_percent(),
    expand = expansion(mult = c(0, 0.06))
  ) +
  theme_bw(base_size = 16, base_family = "Barlow") +
  theme(
    panel.grid.minor.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.right = element_line(color = 'black'),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border = element_blank(),
    plot.subtitle = element_text(
      color = "grey40"
    ),
    legend.position = "inside",
    legend.position.inside = c(.8, .9),
    plot.title.position = "plot"
  ) +
  scale_y_discrete(
    name = "Weather Event"
  ) +
  scale_fill_manual(
    values = c(
      "GBR" = "#CE1124",
      "USA" = "#0A3161",
      "CHN" = "#F1C338",
      "AUS" = "#00843D"
    ),
    labels = c(
      "United States",
      "China",
      "Great Britain",
      "Australia"
    ),
    guide = guide_legend(ncol = 1, title = "Country")
  ) +
  labs(
    title = "Which Weather Events Are Being Studied?",
    subtitle = "Mainly Heat and Rain",
    caption = "Kevin Baer - 8/13/2025"
  ) +
  ggview::canvas(8, 10)

# Save --------------------------------------------------------------------

ggview::save_ggplot(fin, "2025_08_12/2025_08_12_Climate_Image.png")
