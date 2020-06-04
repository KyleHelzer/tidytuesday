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
    filter(time_s < 100) %>% 
    group_by(site) %>% 
    ggplot(aes(x = site, y = time_s)) + 
    geom_boxplot() +
    geom_point(aes(x = site, y = time_s, color = marble_name))

marbles %>%
    filter(time_s > 100) %>% 
    group_by(site) %>% 
    ggplot(aes(x = site, y = time_s)) + 
    geom_boxplot() +
    geom_point(aes(x = site, y = time_s, color = marble_name))

marbles %>% 
    filter(time_s > 100) %>% 
    group_by(site) %>% 
    mutate(
        mean_site_time = mean(time_s),
        st_dev_by_site = sd(time_s)
    ) %>% 
    ungroup() %>% 
    mutate(
        diff_from_mean = time_s - mean_site_time,
        z_score = diff_from_mean / st_dev_by_site
    ) %>% 
    left_join(team_colors, by = "team_name") %>% 
    group_by(marble_name) %>% 
    ggplot(aes(x = reorder(marble_name, z_score, mean), y = z_score)) +
    geom_boxplot() +
    geom_point(aes(x = reorder(marble_name, z_score, mean), y = z_score)) +  
    coord_flip() + 
    
    #scale_fill_manual(values = c(rep("#FF0000", 16), rep("#00FF00", 16)))


                      
team_colors <- tribble(
    ~team_name, ~team_color,
    "Balls of Chaos", "gold", #FFD700
    "Green Ducks", "olivedrab", #6B8E23
    "Hazers", "grey", #808080
    "Hornets", "#999900", #Dark yellow
    "Limers", "lime", #00FF00
    "Mellow Yellow", "yellow", #FFFF00
    "Midnight Wisps", "darkslategray", #2F4F4F
    "O'rangers", "orange", #FFA500
    "Raspberry Racers", "#E30B5D", #raspberry
    "Rojo Rollers", "red", #FF0000
    "Savage Speeders", "maroon", #800000
    "Snowballs", "white", #FFFFFF
    "Team Galatic", "lightgrey", #D3D3D3
    "Team Momo", "yellowgreen", #9ACD32
    "Team Primary", "blue", #0000FF
    "Thunderbolts", "dodgerblue", #1E90FF
)



team_colors
