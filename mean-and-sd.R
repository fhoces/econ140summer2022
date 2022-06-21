# we will be using functions from these libraries
library(tidyverse)
library(reshape2)
library(cowplot)
library(magrittr)

read_data <- function(file){
    read_csv(file) %>%
        rename_all(funs(replace(., values = c("product_code", "station_number", "year", "month", "mean_max", "quality")))) %>%
        mutate(month = factor(month.name[as.integer(month)], levels = month.name)) %>%
        filter(quality == "Y")
}

# load file directly from my web server
bris_reg_max <- read_data(file = "https://davetang.org/weather/IDCJAC0002_040214_Data1.csv")
bris_reg_min <- read_data(file = "https://davetang.org/weather/IDCJAC0004_040214_Data1.csv")
bris_max <- read_data(file = "https://davetang.org/weather/IDCJAC0002_040913_Data1.csv")
bris_min <- read_data(file = "https://davetang.org/weather/IDCJAC0004_040913_Data1.csv")


theme_set(theme_bw())

get_colour <- function(df){
    colfunc <- colorRampPalette(c("blue", "red"))
    my_colour <- colfunc(12)
    
    df %>%
        group_by(month) %>%
        summarise(month_mean = mean(mean_max)) %>%
        arrange(month_mean) %>%
        pull(month) %>%
        as.integer() -> my_order
    
    my_colour[match(1:12, my_order)]
}



my_colour <- get_colour(bris_reg_max)
a <- ggplot(bris_reg_max, aes(year, mean_max, colour = month)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "loess") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.title.x = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = my_colour) +
    labs(title = "Monthly mean maximum temperature", subtitle = "Brisbane Regional Office: January 1910 - March 1986", y = "Degrees Celsius") +
    facet_wrap(~month) +
    NULL

my_colour <- get_colour(bris_reg_min)
b <- ggplot(bris_reg_min, aes(year, mean_max, colour = month)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "loess") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = my_colour) +
    labs(title = "Monthly mean minimum temperature", subtitle = "Brisbane Regional Office: January 1910 - March 1986") +
    facet_wrap(~month) +
    NULL

plot_grid(a, b)



syd_max <- read_data(file = "https://davetang.org/weather/IDCJAC0002_066062_Data1.csv")
syd_min <- read_data(file = "https://davetang.org/weather/IDCJAC0004_066062_Data1.csv")
syd_max %<>% filter(year >= 1910)
syd_min %<>% filter(year >= 1910)


my_colour <- get_colour(syd_max)
a <- ggplot(syd_max, aes(year, mean_max, colour = month)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "loess") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.title.x = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = my_colour) +
    labs(title = "Monthly mean maximum temperature", subtitle = "Sydney: January 1910 - November 2019", y = "Degrees Celsius") +
    facet_wrap(~month) +
    NULL

my_colour <- get_colour(syd_min)
b <- ggplot(syd_min, aes(year, mean_max, colour = month)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "loess") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_color_manual(values = my_colour) +
    labs(title = "Monthly mean minimum temperature", subtitle = "Sydney: January 1910 - November 2019") +
    facet_wrap(~month) +
    NULL

plot_grid(a, b)

a <- syd_max %>%
    filter(year != 2019) %>% 
    mutate(five_years = cut(year, breaks = seq(1910, 2019, by = 10)), 
           year_var = five_years) %>% 
    mutate(vardate = as.Date(paste0(month, year, "01"), 
                             format="%b%Y%d")) %>% 
    group_by(year_var) %>% 
    summarise(year_mean = mean(mean_max)) %>%
    ggplot(., aes(x = year_var, y = year_mean)) +
    geom_point() +
    theme(axis.title.x = element_blank(),
          legend.position = "none") +
    labs(title = "Annual mean maximum temperature", subtitle = "Sydney: 1910 - 2018", y = "Degrees Celsius") +
        geom_hline(yintercept=mean(syd_max$mean_max), linetype="dashed", color = "red")
    #scale_x_date(date_labels = "%Y")
a

mean(syd_max$mean_max)

b <- syd_min %>%
    filter(year != 2019) %>%
    mutate(date = format(
        as.Date(paste0(month, year, "01"), 
                format="%b%Y%d"), "%m-%Y"
        )) %>% 
    group_by(year) %>%
    summarise(year_mean = mean(mean_max)) %>%
    ggplot(., aes(year, year_mean)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    labs(title = "Annual mean minimum temperature", subtitle = "Sydney: 1910 - 2018") +
    NULL

plot_grid(a, NULL, b, nrow = 1, rel_widths = c(1, 0.05, 1))