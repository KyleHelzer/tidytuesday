library(tidyverse)


# Get the data
marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

glimpse(marbles)

marbles %>% 
    group_by(marble_name) %>% 
    summarise(
        time = ave(time_s)
    ) %>% 
    arrange(time)

marbles %>%
    