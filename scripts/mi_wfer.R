

library(scales)
library(tidyverse)


wfe_raw <- read_csv("inputs/wfe_raw_mi.csv")

wfe <- wfe_raw |>
  mutate(across(c('WFE_500m'), round, 2)) |>
  group_by(WFE_500m) |>
  summarize(sum_count = sum(Count)) |>
  mutate(wfe = WFE_500m*100) |>
  mutate(acres = sum_count* 0.2223945) |>
  mutate(quantiles = cut(wfe, 
                         breaks = c(
                           -1,
                           20,
                           40,
                           60,
                           80,
                           100),
                         labels = c(
                           "0 - 20",
                           "20 - 40",
                           "40 - 60",
                           "60 - 80",
                           "80 - 100")))


# Reorder levels of quantiles in reverse order
wfe$quantiles <- factor(wfe$quantiles, levels = rev(levels(wfe$quantiles)))

# group by quantiles for chart
wfe_quantiles <- wfe %>%
  group_by(quantiles) %>%
  summarize(total_acres = sum(acres)) %>%
  mutate(percentage = round(total_acres/sum(total_acres)*100)) |>
  arrange(desc(percentage))

# Reorder levels of quantiles in reverse order
wfe_quantiles$quantiles <- factor(wfe_quantiles$quantiles, levels = (levels(wfe_quantiles$quantiles)))

# make chart

wfe_quantiles_chart <-
  ggplot(wfe_quantiles, aes(x = quantiles, y = total_acres, fill = quantiles)) +
  geom_bar(stat = 'identity', color = '#3d3d3d') +
  coord_flip() +
  labs(
    x = "",
    y = "Total acres per category",
    title = "Wildfire Exposure Risk",
    subtitle = "Colors match map",
    caption = "Categorized; 0 - 20 is the lowest risk category, 80 -100 the highest.") +
  scale_fill_manual(values = c(
    "#FFFFFF",
    "#F3F583",
    "#A5C48C",
    "#5EA79F",
    "#1e344a")) +
  scale_y_continuous(labels = comma) +
  geom_text(aes(label = paste0("  ", percentage, "%")),
            vjust = -0.5, 
            hjust = -0.10, 
            color = "#3d3d3d",
            size = 4) + 
  theme_bw(base_size = 18) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = 'none')+
  # theme(plot.margin = margin(0, #top
  #                            3, #right
  #                            0, # bottom
  #                            0, #left
  #                            "cm")) + 
  expand_limits(y = 5000000) 


wfe_quantiles_chart
