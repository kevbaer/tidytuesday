# Mailroom ----------------------------------------------------------------

library(tidyverse)

# Load --------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2025-07-29')
movies <- tuesdata$movies |>
  filter(
    !title %in%
      c(
        "Monster",
        "Animals on the Loose: A You vs. Wild Movie",
        "Maria",
        "You vs. Wild: Out Cold"
      )
  )


# Do ----------------------------------------------------------------------

m_25 <- movies |>
  filter(report %in% c("2025Jan-Jun"))

m_24 <- movies |>
  filter(report %in% c("2024Jan-Jun"))

m_combo <- inner_join(
  m_25,
  m_24,
  suffix = c("_25", "_24"),
  by = join_by(title, release_date)
) |>
  select(title, hours_viewed_24, hours_viewed_25) |>
  mutate(hours_viewed_delta = hours_viewed_25 - hours_viewed_24)

top_ten_increase <- m_combo |> slice_max(hours_viewed_delta, n = 10)
top_ten_decrease <- m_combo |> slice_min(hours_viewed_delta, n = 10)

# Plot --------------------------------------------------------------------
theme_set(theme_bw(base_size = 16, base_family = "Barlow"))

a <- top_ten_increase |>
  ggplot() +
  aes(x = hours_viewed_delta, y = fct_reorder(title, hours_viewed_delta, max)) +
  geom_col(fill = "springgreen3") +
  labs(
    title = bquote('10' ~ bold('Best') ~ "Movies in Year over Year Growth"),
    subtitle = "Comparing 1st Half of 2024 to 1st Half of 2025",
    caption = "Kevin Baer - 7/31/2025",
    x = "Growth (in hours watched)",
    y = element_blank()
  ) +
  scale_y_discrete(
    labels = scales::label_wrap(15),
  ) +
  scale_x_continuous(
    limits = c(0, 60000000),
    expand = expansion(mult = c(0, 0)),
    breaks = c(0, 15000000, 30000000, 45000000, 60000000),
    labels = c("+0", "+15M", "+30M", "+45M", "+60M         ")
  ) +
  theme(
    panel.grid.minor.x = element_blank(),
    plot.caption = element_text(vjust = 9),
    plot.margin = unit(c(5.5, 5.5, -12, 5.5), "pt"),
    axis.ticks.y = element_blank(),
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.right = element_line(color = 'black'),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border = element_blank(),
    plot.subtitle = element_text(
      color = "grey40"
    )
  ) +
  ggview::canvas(12, 8)


# Decrease ----------------------------------------------------------------

b <- top_ten_decrease |>
  ggplot() +
  aes(
    x = -hours_viewed_delta,
    y = fct_rev(fct_reorder(title, hours_viewed_delta, min))
  ) +
  geom_col(fill = "red2") +
  labs(
    title = bquote('10' ~ bold('Worst') ~ "Movies in Year over Year Growth"),
    subtitle = "Comparing 1st Half of 2024 to 1st Half of 2025",
    caption = "Kevin Baer - 7/31/2025",
    x = "Growth (in hours watched)",
    y = element_blank()
  ) +
  scale_y_discrete(
    labels = scales::label_wrap(15),
  ) +
  scale_x_continuous(
    limits = c(0, 250000000),
    expand = expansion(mult = c(0, 0)),
    # breaks = c(0, 15000000, 30000000, 45000000, 60000000),
    labels = c("-0", "-50M", "-100M", "-150M", "-200M", "-250M            ")
    # labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  theme(
    panel.grid.minor.x = element_blank(),
    plot.caption = element_text(vjust = 9),
    plot.margin = unit(c(5.5, 5.5, -12, 5.5), "pt"),
    axis.ticks.y = element_blank(),
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.right = element_line(color = 'black'),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border = element_blank(),
    plot.subtitle = element_text(
      color = "grey40"
    )
  ) +
  ggview::canvas(12, 8)


# Save --------------------------------------------------------------------

ggview::save_ggplot(
  a,
  "GitHub/tidytuesday/2025_07_29/2025_07_29_Growth_Image.png"
)

ggview::save_ggplot(
  b,
  "Github/tidytuesday/2025_07_29/2025_07_29_Decline_Image.png"
)
