library(ggplot2)
library(dplyr)
library(ggthemes)
library(extrafont)
library(readr)
# font_import()
fonts()

avocado_data = read_csv("avocado_data.csv")

myColors = c("#A6611A", "#DFC27D", "#6e6c6b", "#80CDC1", "#018571")

avocado_data %>%
  mutate(isNortheast = (region == 'Northeast')) %>%
  ggplot(aes(x = Date, y = AveragePrice, color = region)) +
  geom_line(aes(linetype = isNortheast), size = 1.5, alpha = 0.8) + 
  labs(title = "Average Avocado Prices in the US Over Time",
       subtitle = "How do avocado prices differ by region?",
       x = "Date",
       y = "Average Price",
       color = "Region") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(family = "Rubik")) +
  scale_linetype_manual(values = c("dashed", "solid"), guide = "none") + 
  scale_color_manual(values = myColors)
  

