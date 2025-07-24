# Mailroom ----------------------------------------------------------------

library(tidyverse)
library(gganimate)
library(magick)

# Load --------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2025-07-22')
mta_art <- tuesdata$mta_art
station_lines <- tuesdata$station_lines


# Do ----------------------------------------------------------------------

Stops <- tribble(
  ~Stop_Name,
  ~Number_Stop,
  ~time_till_next_stop,
  "34 St-Penn Station",
  1,
  1,
  "42 St-Port Authority Bus Terminal",
  2,
  2,
  "50 St",
  3,
  1,
  "7 Av",
  4,
  2,
  "5 Av/53 St",
  5,
  1,
  "Lexington Av/53rd St",
  6,
  3,
  "Court Sq-23 St",
  7,
  2 #throw away value
) |>
  mutate(
    Stop_Name = as_factor(Stop_Name)
  )

art_on_way <- mta_art |>
  filter(station_name %in% Stops$Stop_Name) |>
  mutate(station_name = factor(station_name, levels = Stops$Stop_Name)) |>
  arrange(station_name) |>
  filter(str_detect(line, "E")) |>
  mutate()


images <- c(
  "https://files.mta.info/s3fs-public/styles/large/public/2021-11/Fischl_NYCT%2034%20St-Penn%20Station_01_Rob%20Wilson.jpg",
  "https://files.mta.info/s3fs-public/styles/large/public/2021-11/Colp_42%20Street_01_Rob%20Wilson.jpg",
  "https://files.mta.info/s3fs-public/styles/large/public/2022-04/Dinhofer_NYCT%2042%20Street-Port%20Authority%20Terminal_01_MTA%20Arts%20%26%20Design%20.jpg",
  "https://files.mta.info/s3fs-public/styles/large/public/2022-02/Mullican_NYCT_50St_01_TrentReeves.jpg",
  "https://wallpapers.com/images/featured/blank-white-7sn5o1woonmklx1h.jpg",
  "https://wallpapers.com/images/featured/blank-white-7sn5o1woonmklx1h.jpg",
  "https://files.mta.info/s3fs-public/styles/large/public/2022-02/Fasanella_5th%20Ave-53rd_01_Arts%20%26%20Design%20.jpg",
  "https://files.mta.info/s3fs-public/styles/large/public/2022-04/Held_NYCT%20Lexington%20Avenue-53rd%20Street_01_Rob%20Wilson%20.jpg",
  "https://files.mta.info/s3fs-public/styles/large/public/2022-04/Held_NYCT%20Lexington%20Avenue-53rd%20Street_01_Rob%20Wilson%20.jpg",
  "https://files.mta.info/s3fs-public/styles/large/public/2022-04/Held_NYCT%20Lexington%20Avenue-53rd%20Street_01_Rob%20Wilson%20.jpg",
  "https://files.mta.info/s3fs-public/styles/large/public/2021-10/Olt_NYCT%2023%20Street-Ely%20Ave-LIC-Court%20Sq_01_BritBunkley.jpg",
  "https://files.mta.info/s3fs-public/styles/large/public/2022-04/Murray_NYCT%20Court%20Sq_01_Rob%20Wilson.jpg",
  "https://files.mta.info/s3fs-public/styles/large/public/2022-04/Murray_NYCT%20Court%20Sq_01_Rob%20Wilson.jpg",
  "https://files.mta.info/s3fs-public/styles/large/public/2022-04/Murray_NYCT%20Court%20Sq_01_Rob%20Wilson.jpg"
) |>
  map(rep, 10) |>
  map(image_read) |>
  map(image_resize, "900x400!") |>
  rbind()

img_animated <- image_animate(image_join(images), fps = 10, dispose = "none")


z <- Stops |>
  ggplot() +
  aes(x = Stop_Name, y = 0.2) +
  geom_col(fill = "#0062CF") +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 22),
    axis.ticks.length.x = unit(c(5, 10), "points"),
  ) +
  scale_y_continuous(
    limits = c(0, 0.8),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_x_discrete(
    name = element_blank(),
    expand = expansion(mult = c(0.1, 0.1)),
    guide = guide_axis(n.dodge = 2)
  ) +
  geom_text(nudge_y = -0.09, label = "E \nTrain", color = "white", size = 9) +
  transition_states(
    Number_Stop,
    transition_length = 0,
    state_length = Stops$time_till_next_stop,
    wrap = FALSE
  ) +
  # shadow_wake(wake_length = 0.05, exclude_layer = 2, wrap = FALSE) +
  ease_aes()


zzz <- animate(
  z,
  nframes = 140,
  device = "ragg_png",
  fps = 10,
  height = 400,
  width = 900,
  end_pause = 20,
  start_pause = 0
)

anim_save("zzz.gif", zzz, path = "GitHub/tidytuesday/2025_07_22")

a_mgif <- image_read("GitHub/tidytuesday/2025_07_22/zzz.gif")
b_mgif <- img_animated


new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = TRUE)
for (i in 2:140) {
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}

new_gif
