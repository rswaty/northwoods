



library(tidyverse)
library(ggridges)

# ----- 0) Month ticks for a 365-day year -----
# First-day-of-month DOY positions (non-leap):
month_breaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
month_labels <- month.abb                      # "Jan", "Feb", ...

# If you need a 366-day axis, use these instead:
# month_breaks <- c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336)

# ----- 1) Ensure continuous DOY per year (counts remain raw; NOT scaled) -----
df_full <- daily_counts_faceted %>%
  mutate(year = as.integer(year)) %>%
  group_by(year) %>%
  tidyr::complete(doy = 1:365, fill = list(count = 0)) %>%
  ungroup()

# ----- 2) Order years and create numeric Y positions for extra separation -----
years_ord <- sort(unique(df_full$year), decreasing = TRUE)  # earliest at top; flip if preferred
gap <- 20                                              # increase for more separation

year_positions <- tibble(
  year = years_ord,
  y_pos = seq_along(years_ord) * gap
)

df_plot <- df_full %>%
  left_join(year_positions, by = "year")

# ----- 3) Plot: raw heights, month labels on x-axis, separated ridges -----
ggplot(df_plot, aes(x = doy, y = y_pos, height = count, group = year)) +
  ggridges::geom_ridgeline(
    fill = "#4e79a7",    # use NA for outline-only
    color = "#2f4e7a",
    linewidth = 0.10,    # use linewidth (not size)
    scale = 1
  ) +
  # Optional crisp white top line
  ggridges::geom_ridgeline(
    color = "grey",
    linewidth = 0.10,
    fill = NA
  ) +
  scale_x_continuous(
    breaks = month_breaks,
    labels = month_labels,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = year_positions$y_pos,
    labels = year_positions$year
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) 

