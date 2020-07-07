# install.packages("gganimate")
library(gganimate)
library(ggplot2)
library(dplyr)
library(gapminder)
library(ggthemes)

# Graph 1: Transitioning through time
gapminder

graph1 = gapminder %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, color=continent, size=pop)) +
  geom_point(alpha = 0.7, stroke = 0) +
  theme_fivethirtyeight() +
  scale_size(range=c(2,12), guide="none") +
  scale_x_log10() +
  labs(title = "Life Expectancy vs GDP Per Capita by Country",
       x = "Income per person (GDP / capita)",
       y = "Life expectancy (years)",
       color = "Continent",
       caption = "Source: Gapminder") +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        legend.text=element_text(size=10)) +
  scale_color_brewer(palette = "Set2")

graph1.animation = graph1 +
  transition_time(year) +
  labs(subtitle = "Year: {frame_time}") +
  shadow_wake(wake_length = 0.1)

animate(graph1.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100)
anim_save("gapminder graph.gif")

# Graph 2: Letting data gradually appear
library(readr)
library(tidyr)

game_sales = read_csv("vgsales.csv") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Platform == 'PS3',
         Genre %in% c("Action", "Shooter", "Sports", "Racing", "Simulation")) %>%
  drop_na() %>%
  group_by(Year, Genre) %>%
  summarise(Sales = sum(Global_Sales, na.rm = TRUE))

graph2 = game_sales %>%
  ggplot(aes(x=Year, y=Sales, color=Genre)) +
  geom_line(size = 2, alpha = 0.75) +
  theme_solarized_2(light = FALSE) +
  labs(title = "PS3 Video Game Sales",
       y = "Global Sales ($ Millions USD)") +
  theme(text = element_text(family = "DM Sans Medium", colour = "#EEEEEE"),
        title = element_text(color = "#EEEEEE"),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Pastel1") +
  geom_point() +
  scale_x_continuous(breaks = 0:2100)

graph2.animation = graph2 +
  transition_reveal(Year) + 
  view_follow(fixed_y = TRUE)

animate(graph2.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100)
anim_save("ps3 game sales.gif")

# Graph 3: Transition between states
reviews = read_csv("googleplaystore.csv") %>%
  filter(Category %in% c("GAME", "SOCIAL", "PRODUCTIVITY", "TOOLS", "EDUCATION")) 

graph3 = reviews %>%
  ggplot(aes(x=Category, y=Rating, fill=Category)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  labs(title = "Google Play Store Ratings",
       y = "Rating (out of 5)",
       caption = "Source: Kaggle") +
  theme(legend.position = "none",
        axis.title.y = element_text(),
        text = element_text(family="Poppins SemiBold"),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_fill_tableau()

graph3.animation = graph3 +
  transition_states(Category, wrap = FALSE) +
  shadow_mark(alpha = 0.5) +
  enter_grow() +
  exit_fade() +
  ease_aes('back-out')

animate(graph3.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100)
anim_save("playstore reviews.gif")