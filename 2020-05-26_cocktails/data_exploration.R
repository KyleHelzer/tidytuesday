# Author: Kyle Helzer
# TidyTuesday 2020-05-26
# Cocktails

library(tidyverse)

# get the data

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

glimpse(cocktails)

# sort by number of incredients
cocktails %>% 
    group_by(drink) %>% 
    summarise(
        n = n()
    ) %>% 
    arrange(desc(n)) # top is Angelica Liqueur with 12 ingredients

# find cocktails
cocktails %>% 
    group_by(drink) %>% 
    summarise(
        n = n()
    ) %>% 
    filter(n == 1)

cocktails %>% 
    filter(drink == "410 Gone") %>% 
    select(drink, ingredient_number, ingredient, measure) # this is missing Coca Cola

cocktails %>% 
    filter(drink == "Screwdriver") %>% 
    select(drink, ingredient_number, ingredient, measure) # this is missing Orange Juice

cocktails %>% 
    filter(drink == "Spiking coffee") %>% 
    select(drink, ingredient_number, ingredient, measure) # this is missing alcohol

cocktails %>% 
    filter(drink == "Tequila Sunrise") %>% 
    select(drink, ingredient_number, ingredient, measure) # this is missing Orange Juice and Grenadine


