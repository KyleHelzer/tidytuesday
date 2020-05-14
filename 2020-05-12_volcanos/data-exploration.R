# Author: Kyle Helzer
# TidyTuesday 2020-05-12
# Volcanic Eruptions

library(tidyverse)
library(lubridate)
library(maps)
library(ggrepel)

# get the data
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

world_map <- map_data("world")
head(world_map)

glimpse(eruptions)

eruptions %>% 
    ggplot(aes(longitude, latitude)) +
    borders("world") + 
    geom_point() + 
    coord_quickmap()

volcano %>% 
    ggplot() +
    geom_polygon(data = world_map, aes(long, lat))

ggsave("lmao.png", dpi = 400)                 


# plot of most eruptions from the year 1000



most_eruptions <- eruptions %>% 
    filter(start_year >= 1000) %>%
    group_by(volcano_name, latitude, longitude) %>% 
    count(sort = TRUE) %>% 
    head(10)

eruptions %>% 
    semi_join(most_eruptions, by = "volcano_name") %>% 
    mutate(
        start_date = make_date(start_year, start_month, start_day),
        volcano_name = ifelse(volcano_name == "Fournaise, Piton de la", "Piton de la Fournaise", volcano_name),
        n = 1
    ) %>% 
    ggplot(aes(x = start_date, y = n, fill = vei, color = vei)) +
    geom_hline(yintercept = 1, color = "black") +
    geom_point(size = 3, shape = 21) +
    # geom_dotplot(binwidth = 1, dotsize = 10000) +
    facet_wrap( ~ volcano_name, ncol = 1, strip.position = "left") +
    
    scale_x_date(limits = c(as.Date("1000-01-01"), as.Date("2020-01-01")), date_labels = "%Y") + 
    scale_y_continuous(limits = c(0, 2)) + 
    scale_color_gradient(limits = c(0, 8), low = "green", high = "red") +
    scale_fill_gradient(limits = c(0, 8), low = "green", high = "red") +
    
    labs(
        title = "Eruptions of the top 10 most active volcanos since year 1000",
        x = "Year",
        y = "Volcano"
    ) +
    
    theme(
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.y = element_rect(fill = "white"),
        strip.text.y.left = element_text(face = "bold", angle = 0)
    )
 
ggsave("colorfillvei.png", dpi = 400)

# world map of top 10
most_eruptions %>% 
    ggplot(aes(longitude, latitude)) + 
    borders("world", colour = "black", fill = "grey") + 
    geom_point(shape = 24, fill = "red") + 
    coord_quickmap() + 
    geom_label_repel(aes(label = volcano_name), size = 3, vjust = 1.5)
    
ggsave("map2.png", dpi = 400)
    
