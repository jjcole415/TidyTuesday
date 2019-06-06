library(tidyverse)
library(ggthemes)
library(gridExtra)

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

tidy_ramen <- ramen_ratings %>%
  filter(!is.na(style)) %>%
  group_by(style) %>%
  mutate(avg_rating_style = mean(stars, na.rm = T), n_reviews = n()) %>%
  ungroup() %>%
  filter(n_reviews > 10) %>%
  mutate(avg_rating = mean(stars, na.rm = T), style = fct_reorder(style, n_reviews))

p1 <- ggplot(data = tidy_ramen, aes(x = style, y = stars, color = style)) +
  geom_jitter(alpha = 0.25, height = .075, show.legend = F) +
  scale_color_hc() +
  theme_minimal() +
  geom_hline(yintercept = tidy_ramen$avg_rating %>% unique(), color = "gray30") +
  annotate(geom = "text", x = 1.4, y = 3.0, label = "Avg. Rating", color = "gray30", fill = "transparent") + 
  geom_curve(inherit.aes = F, x = 1.3, y = 3.0, curvature = 0.3, angle = 90, 
             show.legend = F, xend = 1.0, yend = tidy_ramen$avg_rating %>% unique(), color = "gray30") +
  scale_y_continuous(minor_breaks = seq(from = 0, to = 5, by = 0.25)) +
  coord_flip() +
  labs(x = "", y = "Rating") 

p2 <- ggplot(tidy_ramen, aes(x = stars)) +
  geom_histogram(bins = 21, fill = "#d5e4eb", color = "gray80") +
  scale_x_continuous(minor_breaks = seq(from = 0, to = 5, by = 0.25)) +
  theme_minimal() +
  labs(y = "Number of Reviews", x = "Rating", title = "Distribution of Reviews") +
  theme(plot.margin = margin(0,1,0,1, unit = "cm"), plot.title = element_text(size = 10, hjust = 0.5), axis.title = element_text(size = 8))

top_brands <- tidy_ramen %>%
  filter(stars == 5) %>%
  count(brand, sort = T, name = "count") %>%
  head(5) %>%
  mutate(brand = fct_reorder(brand, count, desc = T))

p3 <- ggplot(data = top_brands, aes(x = brand, y = count, fill = brand)) +
  geom_col(show.legend = F, alpha = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(y = "Number of Products and Flavors",  x = "",
       title = "Brands with most 5-star ratings") + 
  scale_fill_tableau() +
  theme(plot.margin = margin(0,1,0,0, unit = "cm"), plot.title = element_text(size = 10, hjust = 0.5), axis.title = element_text(size = 8))


lay <- rbind(c(1,1,2),
             c(1,1,3))
p4 <- grid.arrange(grobs = list(p1,p2,p3), layout_matrix = lay, align = "v", 
             top = grid::textGrob(label = "Packaged Ramen Ratings and Top Brands\n", hjust = 0, x = 0, gp = gpar(fontsize = 18)))
ggsave(plot = p4, "Ramen Tidy Tuesday.png", width = 8)
