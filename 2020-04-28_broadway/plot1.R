# colot palletts https://htmlcolorcodes.com/

library(tidyverse)
library(extrafont) # imports more fonts
font_import() # takes a bit to run. Only need to do once.
y
loadfonts(device = "pdf")
windowsFonts() # shows list of available fonts

# Get the Data
grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

cpi_normal <- cpi %>% 
    mutate(
        cpi_normalized = cpi / 267
    )

cpi_normal

grosses %>% 
    select(week_ending)

print(grosses, width = Inf)

#average seats sold vs. average weekly gross
grosses %>% 
    group_by(show) %>% 
    summarise(
        ave_weekly_gross = mean(weekly_gross),
        ave_seats_sold = mean(seats_sold)
    ) %>% 
    ggplot(aes(x = ave_seats_sold, y = ave_weekly_gross)) +
    geom_point() + 
    geom_smooth(se = FALSE, color = "red") + 
    
    scale_x_continuous(limits = c(0,NA)) + 
    scale_y_continuous(limits = c(0,NA)) +
    # xlim(0,21000) + 
    # ylim(0,2700000) + 
    labs(
        title = "TidyTuesday Broadway",
        x = "Average Seats Sold",
        y = "Average Weekly Gross"
    ) +
    theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(color = "#000000", face = "bold", family = "sans"),
        axis.title.y = element_text(color = "#000000", face = "bold", family = "sans"),
        
        panel.background = element_blank(),
        axis.line.x = element_line(color = "#000000", size = 1),
        axis.line.y = element_line(color = "#000000", size = 1),
        axis.ticks = element_line(color = "#000000", size = 1),
        panel.grid.major = element_line(color = "#B0B0B0", linetype = "dashed"),
        panel.grid.minor = element_line(color = "#F0F0F0", linetype = "dashed"),
        axis.text = element_text(face = "bold", family = "sans", color = "#000000" )
    ) +
    # arrow and text annotations
    # Hamilton
    annotate("text", x = 8000, y = 2590000, label = "Hamilton", family = "sans", fontface = "bold", color = "#FF5733") + 
    annotate("curve", x = 9300, y = 2580000, xend = 10500, yend = 2650000, curvature = 0.2, arrow = arrow(length = unit(0.1, "inches")), color = "#FF5733") +
    # Springsteen on Broadway
    annotate("text", x = 3000, y = 1900000, label = "Springsteen On Broadway", family = "sans", fontface = "bold", color = "#33FF57") +
    annotate("curve", x = 3000, y = 1950000, xend = 4050, yend = 2150000, curvature = -0.2, arrow = arrow(length = unit(0.1, "inches")), color = "#33FF57") +
    # David Copperfield
    annotate("text", x = 17500, y = 750000, label = "David Copperfield:", family = "sans", fontface = "bold", color = "#1713FF") +
    annotate("text", x = 17500, y = 660000, label = "Dreams and Nightmares", family = "sans", fontface = "bold", color = "#1713FF") + 
    annotate("curve", x = 17500, y = 800000, xend = 19800, yend = 1200000, curvature = -0.3, arrow = arrow(length = unit(0.1, "inches")), color = "#1713FF")

ggsave("test14.pdf")

#viewing top grossing musicals
grosses %>% 
    group_by(show) %>% 
    summarise(
        ave_weekly_gross = mean(weekly_gross),
        ave_seats_sold = mean(seats_sold)
    ) %>% 
    arrange(desc(ave_weekly_gross))

#viewing top ave seats sold
grosses %>% 
    group_by(show) %>% 
    summarise(
        ave_seats_sold = mean(seats_sold)
    ) %>% 
    arrange(desc(ave_seats_sold))

    
glimpse(grosses)
    




# comparing average seat price by theatre
avg_seats_by_theatre <- grosses %>% 
    separate(week_ending, c("year", "month", "day"), sep = "-") %>% 
    group_by(theatre, year) %>%
    summarise(
        n = n(),
        ave = mean(avg_ticket_price)
    )

foursix_st_theatre <- avg_seats_by_theatre %>% 
    filter(theatre == "46th Street Theatre")

hilton_theatre <- avg_seats_by_theatre %>% 
    filter(theatre == "Hilton Theatre")

lyric_theatre <- avg_seats_by_theatre %>% 
    filter(theatre == "Lyric Theatre")

foxwoods <- avg_seats_by_theatre %>% 
    filter(theatre == "Foxwoods Theatre")

ford <-  avg_seats_by_theatre %>% 
    filter(theatre == "Ford Center for the Performing Arts")

walter_kerr <- avg_seats_by_theatre %>% 
    filter(theatre == "Walter Kerr Theatre")

rr <- avg_seats_by_theatre %>% 
    filter(theatre == "Richard Rodgers Theatre")

avg_seats_by_theatre %>% 
    ggplot(aes(year, ave, group = theatre)) +
    geom_line(show.legend = FALSE, color = "#B0B0B0") +
    # geom_line(data = foursix_st_theatre, aes(year, ave), color = "red", size = 1.5) + 
    geom_line(data = hilton_theatre, aes(year, ave), color = "green", size = 1.5) +
    geom_line(data = lyric_theatre, aes(year, ave), color = "orange", size = 1.5) + 
    geom_line(data = foxwoods, aes(year, ave), color = "purple", size = 1.5) + 
    geom_line(data = ford, aes(year, ave), color = "blue", size = 1.5) +
    # geom_line(data = walter_kerr, aes(year, ave), color = "pink", size = 1.5) +
    geom_line(data = rr, aes(year, ave), color = "brown", size = 1.5) +
    
    scale_x_discrete(breaks = seq(1985, 2020, 5)) + 
    
    annotate("text", x = 13, y = 120, label = "Ford Center", color = "blue", fontface = "bold") +
    annotate("text", x = 13, y = 100, label = "for the Performing Arts", color = "blue", fontface = "bold") +
    annotate("text", x = 21, y = 0, label = "Hilton Theatre", color = "green", fontface = "bold") +
    annotate("text", x = 25, y = 150, label = "Foxwoods", color = "purple", fontface = "bold") +
    annotate("text", x = 33, y = 25, label = "Lyric Theatre", color = "orange", fontface = "bold") +
    annotate("text", x = 28.5, y = 275, label = "Richard Rodgers Theatre", color = "brown", fontface = "bold") +
    
    theme(
        panel.background = element_blank(),
        axis.line.x = element_line(color = "#000000", size = 0.75),
        axis.line.y = element_line(color = "#000000", size = 0.75),
        axis.ticks = element_line(color = "#000000", size = 0.5)
    )
    
ggsave("theatre1.pdf")

#average ticket price by theatre view
grosses %>% 
    separate(week_ending, c("year", "month", "day"), sep = "-") %>% 
    group_by(theatre, year) %>%
    summarise(
        n = n(),
        ave = mean(avg_ticket_price)
    ) %>% 
    arrange(desc(ave))

theatres <- grosses %>% 
    group_by(theatre) %>% 
    summarise(
        n = n()
    )
print(theatres, n = 58)
