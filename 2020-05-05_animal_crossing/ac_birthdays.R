# Author: Kyle Helzer
# TidyTuesday 2020-05-05
# Animal Crossing Birthdays

library(tidyverse)
library(lubridate)
library(cowplot)
library(magick)
library(extrafont)

# Get the Data
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

# format birthdays to date (Is there a simpler way to do this?)
villagers <- villagers %>% 
    mutate(
        birthday_date = as.POSIXct(birthday, format ="%m-%d"),
        birthday_date = as.Date(birthday_date)
)

# find dates which have no birthdays
all_dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "days")
villager_birthdays <- villagers$birthday_date
birthday_exists <- all_dates %in% villager_birthdays
bday_tbl <- tibble(
    day = all_dates,
    has_birthday = birthday_exists
)
no_birthdays <- bday_tbl %>% 
    filter(has_birthday == FALSE) %>% 
    mutate(
        has_birthday = 0
    )

# function to calculate midpoint between two dates
date_midpoint <- function(date1, date2){
    date1 <- as.Date(date1)
    date2 <- as.Date(date2)
    i <- interval(date1, date2)
    mid <- as.Date(i@start + as.duration(i)/2)
    return(mid)
}

# build zodiac sign tribble
zodiac <- tribble(
    ~sign,            ~begin,   ~end, # end is exclusive with the exeption of Dec 31
    #----------------/---------/------# for cleaner graphing purposes
    "Capricorn", "01-01", "01-20",
    "Aquarius", "01-20", "02-19",
    "Pisces", "02-19", "03-21",
    "Aries", "03-21", "04-20",
    "Taurus", "04-20", "05-21",
    "Gemini", "05-21", "06-21",
    "Cancer", "06-21", "07-23",
    "Leo", "07-23", "08-23",
    "Virgo", "08-23", "09-23",
    "Libra", "09-23", "10-23",
    "Scorpio", "10-23", "11-22",
    "Sagittarius", "11-22", "12-22",
    "Capricorn", "12-22", "12-31"
)

# format dates as.Date()
zodiac <- zodiac %>% 
    mutate(
        begin = as.Date(begin, format = "%m-%d"),
        end = as.Date(end, format = "%m-%d")
)

# filter out lions
lions <- villagers %>% 
    filter(species == "lion") %>% 
    group_by(birthday_date) %>% 
    summarise(
        n = n()
    )

# #filter out bulls
# bulls <- villagers %>% 
#     filter(species == "bull") %>% 
#     group_by(birthday_date) %>% 
#     summarise(
#         n = n()
#     )

# build plot
theme_set(theme_cowplot())
plot <- villagers %>% 
    group_by(birthday_date) %>% 
    summarise(
        n = n()
    ) %>% 
    
    ggplot() +
    geom_rect(data = zodiac, aes(xmin = begin, xmax = end, ymin = 0, ymax = 3, fill = sign), alpha = 0.3, show.legend = FALSE) +
    geom_jitter(aes(birthday_date, n), shape = 21, height = 0.05, color = "#000000", fill = "#999999", size = 3) + 
    geom_point(data = no_birthdays, aes(day, has_birthday), shape = 21, color = "#000000", fill = "#999999", size = 3) +
    geom_jitter(data = lions, aes(birthday_date, n), shape = 21, height = 0.05, color = "#000000", fill = "#FF9100", size = 3) +
    # geom_jitter(data = bulls, aes(birthday_date, n), height = 0.05, color = "purple", size = 2) +
    
    scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0,0)) +
    scale_y_continuous(limits = c(0,3), breaks = (seq(0,3,1))) +
    
    # labels
    labs(
        title = "Distribution of Animal Crossing Villager Birthdays",
        x = "Date",
        y = "Number of Birthdays"
    ) +
    
    # annotates all zodiac sign text
    annotate("text", x = date_midpoint(zodiac$begin, zodiac$end), y = 2.5, label = zodiac$sign, angle = 90, color = "#BBBBBB", family = "Bahnschrift", fontface = "bold") +
    # Mott text and arrow
    annotate("label", x = as.Date("2020-08-10"), y = 1.3, label = "All of the lion\nvillagers are Leos...", color = "#FF9100", fill = "#111111", family = "Bahnschrift", fontface = "bold", size = 4) +
    annotate("label", x = as.Date("2020-08-10"), y = 0.6, label = "...except for Mott\nwho is a Cancer", color = "#FF9100", fill = "#111111", family = "Bahnschrift", fontface = "bold", size = 4) +
    annotate("curve", x = as.Date("2020-07-19"), y = 0.70, xend = as.Date("2020-07-11"), yend = 0.9, curvature = -0.3, color = "#FF9100", arrow = arrow(length = unit(0.1, "inches"))) +
    # day with 3 or more text and arrows
    annotate("label", x = as.Date("2020-07-01"), y = 2.9, label = "There are no days with 3 or more birthdays", color = "#FFFFFF", fill = "#111111", family = "Bahnschrift", fontface = "bold", size = 6) +
    annotate("curve", x = as.Date("2020-04-10"), y = 2.9, xend = as.Date("2020-04-01"), yend = 2.95, curvature = -0.5, color = "#FFFFFF", arrow = arrow(length = unit(0.1, "inches"))) +
    annotate("curve", x = as.Date("2020-09-21"), y = 2.9, xend = as.Date("2020-09-30"), yend = 2.95, curvature = 0.5, color = "#FFFFFF", arrow = arrow(length = unit(0.1, "inches"))) +
    # no birthday text and arrows
    annotate("label", x = as.Date("2020-03-15"), y = 0.5, label = "There are 5 days with no birthdays", color = "#FFFFFF", fill = "#111111", family = "Bahnschrift", fontface = "bold", size = 5) +
    annotate("text", x = no_birthdays$day - 6, y = 0.15, label = format(no_birthdays$day, "%b %d"), color = "#FFFFFF", angle = -45, family = "Bahnschrift") +
    # author
    annotate("text", x = as.Date("2020-12-01"), y = 2.9, label = "Graphic: @kylehelzer", color = "#AAAAAA", family = "Bahnschrift") +
    
    theme(
        plot.background = element_rect(fill = "#111111"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "#444444"),
        text = element_text(family = "Bahnschrift", color = "#AAAAAA", face = "bold"),
        plot.title = element_text(size = 26, color = "#FFFFFF", hjust = 0.5, margin = margin(b = 20)),
        axis.title.x = element_text(color = "#FFFFFF", margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(color = "#FFFFFF", margin = margin(r = 20, l = 10)),
        axis.text = element_text(family = "Bahnschrift", color = "#CCCCCC", face = "bold"),
        axis.line = element_blank(),
        axis.ticks = element_line(color = "#444444")
    )
    
# add image of Mott
ggdraw() + 
    draw_plot(plot) +
    draw_image(villagers$url[villagers$name == "Mott"], x = 0.474, y = 0.15, width = 0.12, height = 0.12)

ggsave("bday1000.png", dpi = 400)


# villagers %>% 
#     filter(name == "Mott")

#birthday problem with triplet paper:
#https://www.math.ucdavis.edu/~tracy/courses/math135A/UsefullCourseMaterial/birthday.pdf