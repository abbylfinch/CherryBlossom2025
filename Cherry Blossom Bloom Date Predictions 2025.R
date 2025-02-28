#Abigail Finch STAT 490
library(mgcv)
library(tidyverse)

cherry <- read.csv("C:/Users/abbyl/Downloads/peak-bloom-prediction-main/peak-bloom-prediction-main/data/washingtondc.csv") |> 
  bind_rows(read.csv("C:/Users/abbyl/Downloads/peak-bloom-prediction-main/peak-bloom-prediction-main/data/liestal.csv")) |> 
  bind_rows(read.csv("C:/Users/abbyl/Downloads/peak-bloom-prediction-main/peak-bloom-prediction-main/data/kyoto.csv")) |> 
  bind_rows(read.csv("C:/Users/abbyl/Downloads/peak-bloom-prediction-main/peak-bloom-prediction-main/data/vancouver.csv")) |> 
  bind_rows(read.csv("C:/Users/abbyl/Downloads/peak-bloom-prediction-main/peak-bloom-prediction-main/data/nyc.csv"))

cherry |> 
  group_by(location) |> 
  slice_tail(n = 3)

cherry |> 
  filter(year >= 1880) |>
  ggplot(aes(x = year, y = bloom_doy)) +
  geom_point() +
  geom_step(linetype = 'dotted', color = 'gray50') +
  scale_x_continuous(breaks = seq(1880, 2020, by = 20)) +
  facet_grid(cols = vars(str_to_title(location))) +
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)")

gam_fit <- gam(bloom_doy ~ s(year, k = 10) + location, data = cherry, method = "REML")

future_years <- expand_grid(location = unique(cherry$location),
                            year = 2023:2025)

preds <- predict(gam_fit, newdata = future_years, se.fit = TRUE)

predictions <- future_years %>%
  mutate(prediction = preds$fit,
         lower = preds$fit - 1.96 * preds$se.fit,
         upper = preds$fit + 1.96 * preds$se.fit)

ggplot() +
  geom_point(data = cherry, aes(x = year, y = bloom_doy), alpha = 0.5) + 
  geom_line(data = predictions, aes(x = year, y = prediction, color = location), size = 1) + 
  geom_ribbon(data = predictions, aes(x = year, ymin = lower, ymax = upper, fill = location), alpha = 0.2) + 
  facet_wrap(~ location) +
  labs(title = "Cherry Blossom Bloom Predictions with Splines",
       x = "Year", y = "Peak Bloom DOY") +
  theme_minimal()

doy_to_date <- function(year, doy) {
  as.Date(doy, origin = paste0(year, "-01-01"))
}

predictions <- future_years %>%
  mutate(prediction = preds$fit,
         lower = preds$fit - 1.96 * preds$se.fit, 
         upper = preds$fit + 1.96 * preds$se.fit) %>%
  mutate(prediction_date = doy_to_date(year, round(prediction)), 
         lower_date = doy_to_date(year, floor(lower)),
         upper_date = doy_to_date(year, ceiling(upper))) %>%
  select(location, year, prediction_date, lower_date, upper_date) 

print(predictions)
