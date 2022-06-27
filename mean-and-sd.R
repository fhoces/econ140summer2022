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




sapply(1:10 , 1)
sapply(1:10 , function(x) 1)
sapply(1:10 , function(x) sample(1))
sapply(1:10 , function(x) sample(1:2,size = 1))
sapply(1:10 , function(x) sample(1:2,size = 4))
sapply(1:10 , function(x) sample(1:2,size = 4, replace = TRUE))
draws <- sapply(1:10 , function(x) sample(18,size = 4))
draws
draws <- sapply(1:10 , function(x) sample(18,size = 25))
draws <- sapply(1:10 , function(x) sample(18,size = 25)
                draws <- sapply(1:10 , function(x) sample(18,size = 25, replace = TRUE)
                )
                draws
                draws <- sapply(1:10 , function(x) sample(0:1,size = 25, replace = TRUE, prob = 2)
cheats  <- sapply(1:10 , function(x) sample(0:1,size = 25, replace = TRUE, prob = 2))
cheats  <- sapply(1:10 , function(x) sample(0:1,size = 25, replace = TRUE, prob = .2))
cheats  <- sapply(1:10 , function(x) sample(c(0,1),size = 25, replace = TRUE, prob = .2))
sample(c(0,1) ,size = 25, replace = TRUE, prob = .2)
sample(c(0,1) ,size = 25, replace = TRUE)
sample(c(0,1) ,size = 25, replace = TRUE, prob = c(0.2,0.8))
sample(c(0,1) ,size = 25, replace = TRUE, prob = c(.2, 0.8)
)
cheats  <- sapply(1:10 , function(x) sample(c(0,1) ,size = 25, replace = TRUE, prob = c(.2, 0.8)))
cheats
cheats  <- sapply(1:10 , function(x) sample(c(0,3) ,size = 25, replace = TRUE, prob = c(.2, 0.8)))
cheats
draws + cheats
asd <- draws + cheats
asd[asd>18] <- 18
draws1 <- sapply(1:10 , function(x) sample(1:6,size = 25, replace = TRUE)
)
draws1 <- sapply(1:10 , function(x) sample(1:6,size = 25, replace = TRUE))
draws2 <- sapply(1:10 , function(x) sample(1:6,size = 25, replace = TRUE))
draws3 <- sapply(1:10 , function(x) sample(1:6,size = 25, replace = TRUE))
total = draws1 + draws2
total = draws1 + draws2 + draws3 + cheats
cheats[cheats>18] <- 18
cheats
apply(total, 2, mean)
apply(draws1, 2, mean)
total_nocheat = draws1 + draws2 + draws3
apply(total, 2, mean)  - apply(total_nocheat, 2, mean)
hist(total)
hist(total, breaks = 20)
hist(total, breaks = 60)
hist(total, breaks = 40)
hist(total, breaks = 30)
?hist
hist(total, density = 1)
hist(total, density = 10)
plot(total, density = 10)
plot(total)
total
Vectorize(total)
as.array(total)
as.vector(total)
plot(as.vector(total))
density(as.vector(total))
plot(density(as.vector(total)))
plot(density(as.vector(total), bw = 1))
plot(density(as.vector(total), bw = .1))
plot(density(as.vector(total), bw = .61))
plot(density(as.vector(total), bw = .6))
plot(density(as.vector(total), bw = .4))
mean(apply(total_nocheat, 2, mean))
abline(v = mean(apply(total_nocheat, 2, mean)))





---
  # Population *vs.* Sample
  
  ```{R, gen dataset, include = F, cache = T}
# Set population and sample sizes
n_p <- 100
n_s <- 10
# Set the seed
set.seed(142857)
# Generate data
pop_df <- tibble(
  i = 3,
  x = rnorm(n_p, mean = 2, sd = 20),
  row = rep(1:sqrt(n_p), times = sqrt(n_p)),
  col = rep(1:sqrt(n_p), each = sqrt(n_p)),
  s1 = sample(x = c(rep(T, n_s), rep(F, n_p - n_s))),
  s2 = sample(x = c(rep(T, n_s), rep(F, n_p - n_s))),
  s3 = sample(x = c(rep(T, n_s), rep(F, n_p - n_s)))
)
# Means
m0 <- mean(pop_df$x)
m1 <- mean(subset(pop_df$x, pop_df$s1 == T))
m2 <- mean(subset(pop_df$x, pop_df$s2 == T))
m3 <- mean(subset(pop_df$x, pop_df$s3 == T))
# Simulation
set.seed(12468)
sim_df <- mclapply(mc.cores = 1, X = 1:1e3, FUN = function(x, size = n_s) {
  pop_df %>% 
    sample_n(size = size) %>% 
    summarize(mu_hat = mean(x))
}) %>% do.call(rbind, .) %>% as_tibble()
```

.pull-left[
  
  ```{R, pop1, echo = F, fig.fullwidth = T, dev = "svg"}
  ggplot(data = pop_df, aes(x = row, y = col)) +
    geom_point(color = "darkslategray", size = 10) +
    theme_empty
  ```
  
  .center[**Population**]
  
]

--
  
  .pull-right[
    
    ```{R, mean1, echo = F, fig.height = 5, dev = "svg"}
    ggplot() +
      geom_histogram(data = pop_df, aes(x), fill = "darkslategray", alpha = 0.50) +
      geom_vline(xintercept = m0, size = 2, color = "darkslategray") +
      theme_empty
    ```
    
    .center[**Population relationship**]
    <br>
      $\mu = `r round(m0, 2)`$
      
  ]

---
  # Population *vs.* Sample
  
  .pull-left[
    
    ```{R, sample1, echo = F, fig.fullwidth = T, dev = "svg"}
    ggplot(data = pop_df, aes(x = row, y = col, shape = s1)) +
      geom_point(color = "darkslategray", size = 10) +
      scale_shape_manual(values = c(1, 19)) +
      theme_empty
    ```
    
    .center[**Sample 1:** 10 random individuals]
    
  ]

--
  
  .pull-right[
    
    ```{R, sample1 mean, echo = F, fig.height = 5, dev = "svg"}
    ggplot() +
      geom_histogram(data = pop_df, aes(x), fill = "darkslategray", alpha = 0.50) +
      geom_vline(xintercept = m0, size = 2, color = "darkslategray") +
      geom_histogram(data = subset(pop_df, s1 == T), aes(x), fill = red_pink, alpha = 0.50) +
      geom_vline(xintercept = m1, size = 2, color = red_pink) +
      theme_empty
    ```
    
    .center[
      
      **Population relationship**
        <br>
        $\mu = `r round(m0, 2)`$
        
        **Sample relationship**
        <br>
        $\hat{\mu} = `r round(m1, 2)`$
        
    ]
    
  ]

---
  # Population *vs.* Sample
  
  .pull-left[
    
    ```{R, sample2, echo = F, fig.fullwidth = T, dev = "svg"}
    ggplot(data = pop_df, aes(x = row, y = col, shape = s2)) +
      geom_point(color = "darkslategray", size = 10) +
      scale_shape_manual(values = c(1, 19)) +
      theme_empty
    ```
    
    .center[**Sample 2:** 10 random individuals]
    
  ]

--
  
  .pull-right[
    
    ```{R, sample2 mean, echo = F, fig.height = 5, dev = "svg"}
    ggplot() +
      geom_histogram(data = pop_df, aes(x), fill = "darkslategray", alpha = 0.50) +
      geom_vline(xintercept = m0, size = 2, color = "darkslategray") +
      geom_histogram(data = subset(pop_df, s2 == T), aes(x), fill = red_pink, alpha = 0.50) +
      geom_vline(xintercept = m2, size = 2, color = red_pink) +
      theme_empty
    ```
    
    .center[
      
      **Population relationship**
        <br>
        $\mu = `r round(m0, 2)`$
        
        **Sample relationship**
        <br>
        $\hat{\mu} = `r round(m2, 2)`$
        
    ]
    
  ]

---
  # Population *vs.* Sample
  
  .pull-left[
    
    ```{R, sample3, echo = F, fig.fullheight = T, dev = "svg"}
    ggplot(data = pop_df, aes(x = row, y = col, shape = s3)) +
      geom_point(color = "darkslategray", size = 10) +
      scale_shape_manual(values = c(1, 19)) +
      theme_empty
    ```
    
    .center[**Sample 3:** 10 random individuals]
    
  ]

--
  
  .pull-right[
    
    ```{R, sample3 mean, echo = F, fig.height = 5, dev = "svg"}
    ggplot() +
      geom_histogram(data = pop_df, aes(x), fill = "darkslategray", alpha = 0.50) +
      geom_vline(xintercept = m0, size = 2, color = "darkslategray") +
      geom_histogram(data = subset(pop_df, s3 == T), aes(x), fill = red_pink, alpha = 0.50) +
      geom_vline(xintercept = m3, size = 2, color = red_pink) +
      theme_empty
    ```
    
    .center[
      
      **Population relationship**
        <br>
        $\mu = `r round(m0, 2)`$
        
        **Sample relationship**
        <br>
        $\hat{\mu} = `r round(m3, 2)`$
        
    ]
    
  ]

---
  class: clear-slide, middle

Let's repeat this **10,000 times** and then plot the estimates.

(This exercise is called a Monte Carlo simulation.)

---
class: clear-slide, middle

```{R, simulation, echo = F, dev = "svg", fig.height =5}
ggplot() +
  geom_histogram(data = sim_df, aes(mu_hat), fill = red_pink, alpha = 0.75) +
  geom_vline(xintercept = m0, size = 2, color = "darkslategray") +
  scale_x_continuous(breaks = c(m0), labels = TeX("$\\mu$")) +
  xlab(TeX("$\\hat{\\mu}$")) +
  theme(axis.text.x = element_text(size = 20),
      axis.text.y = element_blank(),
      rect = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 20, hjust = 1, color = met_slate),
      line = element_blank())
```

---
# Population *vs.* Sample

**Question:** Why do we care about *population vs. sample*?

.pull-left[
```{R, simulation2, echo = F, dev = "svg"}
ggplot() +
  geom_histogram(data = sim_df, aes(mu_hat), fill = red_pink, alpha = 0.75) +
  geom_vline(xintercept = m0, size = 2, color = "darkslategray") +
  scale_x_continuous(breaks = m0, labels = TeX("$\\mu$")) +
  xlab(TeX("$\\hat{\\mu}$")) +
  theme(axis.text.x = element_text(size = 20),
      axis.text.y = element_blank(),
      rect = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 20, hjust = 1, color = met_slate),
      line = element_blank())
```
]

.pull-right[

- On average, the mean of the samples are close to the population mean.

- But...some individual samples can miss the mark.

- The difference between individual samples and the population creates __uncertainty__. 

]

---
# Population *vs.* Sample

**Question:** Why do we care about *population vs. sample*?

**Answer:** Uncertainty matters.

- $\hat{\mu}$ is a random variable that depends on the sample.

- In practice, we don't know whether our sample is similar to the population or not. 

- Individual samples may have means that differ greatly from the population.

- We will have to keep track of this uncertainty.
