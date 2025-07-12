# Mailroom ----------------------------------------------------------------

library(tidyverse)
library(waffle)
library(patchwork)

# Load and Explore --------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2025-07-08')
answers <- tuesdata$answers
color_ranks <- tuesdata$color_ranks
users <- tuesdata$users

added <- answers |>
  left_join(color_ranks, by = join_by(rank)) |>
  drop_na(color) |>
  rename(shown_hex = hex.x, guess = color, hex_of_guess = hex.y) |>
  left_join(users, by = join_by(user_id))

summed <- added |>
  count(user_id, monitor, y_chromosome, colorblind, spam_prob, sort = TRUE) |>
  filter(n > 50)

picks <- added |>
  filter(user_id %in% summed$user_id) |>
  filter(spam_prob < 1)

picks |>
  summarize(
    green = mean(guess == "green"),
    blue = mean(guess == "blue"),
    purple = mean(guess == "purple"),
    pink = mean(guess == "pink"),
    brown = mean(guess == "brown"),
    n = n(),
    .by = c(user_id, spam_prob)
  ) |>
  rowwise() |>
  mutate(tot = sum(green, blue, purple, pink, brown), .after = brown) |>
  ungroup() |>
  arrange(desc(pink)) |>
  View()

picks_spec <- picks |> filter(user_id == 134233)

# Waffle ------------------------------------------------------------------
greens <- picks_spec |> filter(guess == "green") |> pull(shown_hex)
blues <- picks_spec |> filter(guess == "blue") |> pull(shown_hex)
purples <- picks_spec |> filter(guess == "purple") |> pull(shown_hex)
pinks <- picks_spec |> filter(guess == "pink") |> pull(shown_hex)
browns <- picks_spec |> filter(guess == "brown") |> pull(shown_hex)

green_picks <- ggplot(picks_spec |> filter(guess == "green")) +
  aes(fill = shown_hex, values = 1) +
  geom_waffle(n_rows = 5, size = 0.33, color = "white", flip = TRUE) +
  scale_fill_manual(name = NULL, labels = NULL, values = greens, guide = NULL) +
  coord_equal() +
  theme_void() +
  ggtitle("GREEN") +
  theme(plot.title = element_text(size = 22, family = "Barlow", hjust = 0.5))

blue_picks <- ggplot(picks_spec |> filter(guess == "blue")) +
  aes(fill = shown_hex, values = 1) +
  geom_waffle(n_rows = 3, size = 0.33, color = "white") +
  scale_fill_manual(name = NULL, labels = NULL, values = blues, guide = NULL) +
  coord_equal() +
  theme_void() +
  ggtitle("BLUE") +
  theme(plot.title = element_text(size = 22, family = "Barlow", hjust = 0.5))

purple_picks <- ggplot(picks_spec |> filter(guess == "purple")) +
  aes(fill = shown_hex, values = 1) +
  geom_waffle(n_rows = 2, size = 0.6, color = "white", flip = TRUE) +
  scale_fill_manual(
    name = NULL,
    labels = NULL,
    values = purples,
    guide = NULL
  ) +
  coord_equal() +
  theme_void() +
  ggtitle("PURPLE") +
  theme(plot.title = element_text(size = 22, family = "Barlow", hjust = 0.5))

pink_picks <- ggplot(picks_spec |> filter(guess == "pink")) +
  aes(fill = shown_hex, values = 1) +
  geom_waffle(n_rows = 6, size = 0.33, color = "white", flip = TRUE) +
  scale_fill_manual(name = NULL, labels = NULL, values = pinks, guide = NULL) +
  coord_equal() +
  theme_void() +
  ggtitle("PINK") +
  theme(plot.title = element_text(size = 22, family = "Barlow", hjust = 0.5))

brown_picks <- ggplot(picks_spec |> filter(guess == "brown")) +
  aes(fill = shown_hex, values = 1) +
  geom_waffle(n_rows = 1, size = 0.1, color = "white") +
  scale_fill_manual(name = NULL, labels = NULL, values = browns, guide = NULL) +
  coord_equal() +
  theme_void() +
  ggtitle("BROWN") +
  theme(plot.title = element_text(size = 22, family = "Barlow", hjust = 0.5))


# patchwork ---------------------------------------------------------------

finished <- (purple_picks | green_picks | brown_picks) /
  (pink_picks | blue_picks) +
  plot_annotation(
    title = "User 134233 Color Identification",
    subtitle = "As the biggest pink fan in the dataset...",
    caption = "Kevin Baer - 7/12/2025",
    theme = theme(
      plot.title = element_text(size = 28, family = "Barlow"),
      plot.subtitle = element_text(
        size = 20,
        family = "Barlow",
        color = "grey40"
      ),
      plot.caption = element_text(size = 16, family = "Barlow")
    )
  ) +
  ggview::canvas(7, 6)

# ggview::save_ggplot(finished, "tidytuesday/2025_07_08/2025_07_08_Colors_Image.png")
