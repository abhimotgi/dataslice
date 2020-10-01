library(plotly)
library(dplyr)
library(readr)

# Example 1
states = read_csv("states.csv")

minwage_df = read_csv("Minimum Wage Data.csv") %>%
  inner_join(states, by.x = State, by.x = state) %>%
  select(Year, State, Code, Wage = High.Value) %>%
  mutate(hover = paste0(State, "\n$", Wage))
  

graph_properties <- list(
  scope = 'usa',
  showland = TRUE,
  landcolor = toRGB("white"),
  color = toRGB("white")
)

font = list(
  family = "DM Sans",
  size = 15,
  color = "black"
)

label = list(
  bgcolor = "#EEEEEE",
  bordercolor = "transparent",
  font = font
)


minwage_graph = plot_geo(minwage_df, 
                         locationmode = "USA-states", 
                         frame = ~Year) %>%
  add_trace(locations = ~Code,
            z = ~Wage,
            zmin = 0,
            zmax = max(minwage_df$Wage),
            color = ~Wage,
            colorscale = "Electric",
            text = ~hover,
            hoverinfo = 'text') %>%
  layout(geo = graph_properties,
         title = "Minimum Wage in the US\n1968 - 2017",
         font = list(family = "DM Sans")) %>%
  config(displayModeBar = FALSE) %>%
  style(hoverlabel = label) %>%
  colorbar(tickprefix = '$')

minwage_graph


# Example 2
ufos <- read_csv("ufos.csv") %>%
  select(longitude, latitude, datetime, shape)

geo_properties <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  showsubunits = FALSE,
  landcolor = toRGB("gray10"),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

ufo.fig = plot_geo(ufos, lat = ~latitude, lon = ~longitude, 
                   marker = list(size = 2, color = "#ffffcc", opacity = 0.25)) %>%
  add_markers(hoverinfo = "none") %>%
  config(displayModeBar = FALSE) %>%
  layout(geo = geo_properties)

ufo.fig