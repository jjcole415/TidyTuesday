library(tidyverse)
library(countrycode)
library(ggthemes)
library(extrafont)
library(ggrepel)
loadfonts()
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
                                     
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

glimpse(coast_vs_waste)
glimpse(mismanaged_vs_gdp)
glimpse(waste_vs_gdp)

coast_vs_waste_tidy <- coast_vs_waste %>%
  filter(!is.na(`Mismanaged plastic waste (tonnes)`)) %>%
  mutate(`Coastal population` = `Coastal population`/1000000, 
         Region = countrycode(sourcevar = Entity, origin = "country.name.en", destination = "continent")) 

ggplot(coast_vs_waste_tidy, aes(y = `Mismanaged plastic waste (tonnes)`, x = `Coastal population`)) + 
  geom_point(alpha = 0.5, aes(color = Region)) + 
  scale_y_continuous(labels = scales::comma_format(), breaks = seq(from = 0, to = 4000000, by = 500000), limits = c(0, 4000000)) +
  scale_x_continuous(name = "Coastal Population (Millions)", labels = scales::comma_format(), breaks = seq(from = 0, to = 200, by = 20),limits = c(0, 200)) +
  geom_smooth(method = "lm")

mismanaged_vs_gdp_tidy <- mismanaged_vs_gdp %>%
  group_by(Entity) %>%
  mutate(`Change in GDP per Capita` = `GDP per capita, PPP (constant 2011 international $) (Rate)`/lag(`GDP per capita, PPP (constant 2011 international $) (Rate)`, order_by = Year)-1) %>%
  filter(!is.na(`Per capita mismanaged plastic waste (kilograms per person per day)`)) %>%
  ungroup() %>%
  mutate(`Total population (Gapminder)` = `Total population (Gapminder)`/1000000,
         Region = countrycode(sourcevar = Entity, origin = "country.name.en", destination = "continent"),
         Avg = mean(`Per capita mismanaged plastic waste (kilograms per person per day)`),
         Diff = `Per capita mismanaged plastic waste (kilograms per person per day)`- Avg) %>%
  filter(!is.na(Region))

world_avg <- mismanaged_vs_gdp_tidy$Avg %>% unique()

ggplot(mismanaged_vs_gdp_tidy, aes(x = `GDP per capita, PPP (constant 2011 international $) (Rate)`,
                                   y = `Per capita mismanaged plastic waste (kilograms per person per day)`)) + 
  geom_point(alpha = 0.7, aes(color = Region, size = `Total population (Gapminder)`)) + 
  scale_x_continuous(name = "GDP Per Capital (Log scale)", trans = "log", breaks = c(1, 100, 1000, 10000, 100000), labels = scales::comma_format()) +
  scale_color_tableau() +
  geom_smooth(method = "loess", color = "grey50", alpha = 0.5, size = 0.5, se = F) +
  geom_hline(yintercept = world_avg, color = "goldenrod") + 
  geom_text_repel(data = mismanaged_vs_gdp_tidy %>% top_n(n = 20, wt = Diff), 
            mapping = aes(label = Entity, color = Region), nudge_y = 0, nudge_x = 0, show.legend = F,
            family = "Georgia", size = 3, face = "bold") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#333333"), 
        text = element_text(color = "#C8C8C6", family = "Georgia"), 
        axis.text = element_text(color = "#B3B3B3")) +
  annotate("text", x = 68000, y = .035, family = "Georgia", size = 2.5, color = "goldenrod",
           label = glue::glue("Worldwide average:\n{round(world_avg, 3)} kg per person per day")) +
  geom_curve(aes(x = 50000, y = .038, xend = 37000, yend = 0.045), arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
             color = "goldenrod", curvature = -0.2) + 
  scale_size(name = "Population")

ggsave("Plastics Tidy Tuesday.png")

