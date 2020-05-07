library(tidyverse)


# Get the Data

critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

problems(items) # Two of the oberservations in the "customizable" column are "Yes" instead of TRUE
slice(items, 4472:4473) # num_id is 7283. Same item, but two rows for the two ingredients needed to craft (softwood, iron nuggets)

# change these observations to TRUE
fixed_items <- items %>% 
    mutate(
        customizable = ifelse(num_id == 7283, TRUE, customizable)
    )

fixed_items %>%
    filter(!is.na(sell_value)) %>% 
    group_by(name) %>% 
    arrange(sell_value) %>%
    ggplot(aes(reorder(name, sell_value), sell_value)) + 
    geom_point() +
    
    theme(
        axis.ticks.x = element_blank()
    )

?reorder()
