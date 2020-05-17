# Author: Kyle Helzer
# TidyTuesday 2020-05-19
# Beach Volleyball

library(tidyverse)

# Get the Data

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

glimpse(vb_matches)


# who adds the most to win% when added to person

# country means location of tournament, not nationality of player
vb_matches %>% 
    group_by(country) %>% 
    count(sort = TRUE)

#TODO combine wins/loses. could be players who are "player1" and "player2" in different matches
vb_matches %>% 
    group_by(w_player1) %>% 
    count(sort = TRUE)

vb_matches %>% 
    group_by(w_player2) %>% 
    count(sort = TRUE)

#most wins by each duo
wins <- vb_matches %>% 
    group_by(w_player1, w_player2) %>% 
    count()

#most losses by each duo
losses <- vb_matches %>% 
    group_by(l_player1, l_player2) %>% 
    count()

# combine into win-loss record for each duo
wl_record <- full_join(wins, losses, by = c("w_player1" = "l_player1", "w_player2" = "l_player2")) %>% 
    rename(player1 = w_player1,
           player2 = w_player2,
           wins = n.x,
           losses = n.y,
           ) %>% 
    mutate(
        wins = ifelse(is.na(wins), 0, wins),
        losses = ifelse(is.na(losses), 0, losses),
        total_matches = wins + losses,
        win_record = wins / total_matches
    )

wl_record %>% 
    mutate(
        total_matches = wins + losses,
        win_record = wins / total_matches
    ) %>% 
    ggplot(aes(total_matches, win_record)) + 
    geom_point()
    
# best win record
wl_record %>% 
    arrange(desc(win_record), desc(total_matches)) %>% 
    filter(wins > 10)

# worst win record
wl_record %>% 
    arrange(win_record, desc(total_matches))

vb_matches %>% 
    filter(l_player1 == "Elodie Li Yuk Lo") %>% 
    select(-circuit, -tournament, -year, -date) %>% 
    print(width = Inf)


wl_record %>% 
    filter(player1 == "Kerri Walsh Jennings" | player2 == "Kerri Walsh Jennings")

wl_record %>% 
    filter(player1 == "Misty May-Treanor" | player2 == "Misty May-Treanor")
