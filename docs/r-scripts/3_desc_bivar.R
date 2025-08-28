##### Bi-variate statistics #######

# clear de "Global Environment"
rm(list=ls()) 

# set working directory
setwd("/Volumes/francijb/Documents/FRAN/Teaching/QM_2024/session") 

# upload basic packages
library(tidyverse)
library(readxl)
library(ggplot2)

# import data
data <- read_excel("data/paisley_data.xlsx")

data2 <- read_rds("paisley_v2.rds")


### Qualitative variables

# Cross-tabulation / contingency table
view(data)

data |>
  count(employed)

data |>  
  count(sex, employed)

data |>  
  count(sex, employed) |>
  pivot_wider(names_from = employed, values_from = n)


data |>  
  count(sex, employed) |>
  pivot_wider(names_from = employed, values_from = n) |>
  mutate(total = employed + unemployed,
         emp_perc = 100*employed/total,
         unemp_perc = 100*unemployed/total) |>
  mutate_if(is.numeric, round, 1) |>
  relocate(sex, employed, emp_perc, unemployed, unemp_perc, total)


library(modelsummary) # ready-made solution
data |>
  filter(!is.na(employed)) |
  datasummary_crosstab(sex ~ employed, data = _)


# Visualisation

data |> 
  filter(countryb == "scotland" & age>=16) |>  
  ggplot(aes(x = lit_rank)) + 
    geom_bar() +
    coord_flip() +
    facet_wrap(~ sex, nrow = 1)

data |> 
  filter(countryb == "scotland" & age>=16) |>  
  ggplot(mapping = aes(x = lit_rank, y = after_stat(prop), group = sex)) + 
    geom_bar() +
    coord_flip() +
    facet_wrap(~ sex, nrow = 1)

data |> 
  filter(countryb == "scotland" & age>=16 & !is.na(lit_rank)) |>
  ggplot(aes(x = lit_rank, y = after_stat(prop), 
             fill = sex, group = sex)) + 
  geom_bar(position = "dodge2") +
  coord_flip()

?ggplot

# 3 variables simultaneously

data |> 
  filter(countryb == "scotland" & age>=16 & !is.na(lit_rank)) |>
  ggplot(aes(x = lit_rank, y = after_stat(prop), 
             fill = sex, group = sex)) + 
  geom_bar(position = "dodge2") +
  coord_flip() +
  facet_wrap(~ countryb, nrow = 1)


### Comparing numerical variables by group

# Comaparing means (average)
data

data |>
  ggplot(aes(x = height)) + 
  geom_histogram()



data |> 
  filter(countryb!="overseas") |>
  group_by(countryb) |>
  summarize(
    obs = sum(!is.na(height)),
    mean_height = mean(height, na.rm = TRUE)) |>
  mutate(mean_height = round(mean_height, 1))

# Visually (not very useful)

data |> 
  filter(countryb!="overseas") |>
  group_by(countryb) |>
  summarize(mean_height = mean(height, na.rm = TRUE)) |>
  ggplot(aes(x = countryb, y = mean_height)) + 
  geom_point()

# Comparing distributions

data |>   
  filter(countryb!="overseas") |>
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~ countryb, nrow = 1)
  # but very different sample sizes

data |>   
  filter(countryb!="overseas") |>
  ggplot(aes(x = height, y = after_stat(density))) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ countryb, nrow = 1)
  # difficult to eyeball differences between graphs

# Overlapping histograms (kernel density graphs)
data |>   
  ggplot(aes(x = height, y = after_stat(density))) +
  geom_histogram(binwidth = 5) +
  geom_density(kernel = "gaussian", color = "red", size = 1.5)

data |>   
  filter(countryb!="overseas") |>  
  ggplot(aes(x = height, colour = countryb)) +
  geom_density()

# Boxplots
  # another way of comparing distributions

data |>   
  filter(countryb!="overseas") |>  
  ggplot(aes(x = countryb, y = height)) +
  geom_boxplot()

# the solid line depicts the median value (50th percentile)
# the box contains 50 per cent of the observations 
# (those contained between the percentiles 25th and 75th (IQR).
# the vertical lines that extend below and above the box 
# reflect observations falling within 1.5 the interquartile range
# the black dots refer to the extreme values that all outside that range 

data |>   
  filter(countryb!="overseas") |>  
  ggplot(aes(x = countryb, y = height)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2, width=0.10, height=0.05)


## Compositional effects

data |> 
  filter(countryb!="overseas") |>
  filter(sex=="male" & age>=18 & age<=50) |>
  group_by(countryb) |>
  summarize(
    obs = sum(!is.na(height)),
    mean_height = mean(height, na.rm = TRUE)) |>
  mutate(mean_height = round(mean_height, 1))


## With qualitative variables (able to write)

data |> 
  mutate(write = ifelse(lit_adj=="write", 1, 0)) |>
  filter(countryb!="overseas" & sex=="male" & age>18) |>
  group_by(sex, countryb) |>
  summarize(
    obs = sum(!is.na(write)), 
    write = round(mean(write, na.rm = TRUE), 3))


## Comparing multiple groups

data |> 
  group_by(sex, countryb) |>
  summarize(
    obs = sum(!is.na(height)), 
    height = mean(height, na.rm = TRUE)) |> 
  mutate_if(is.numeric, round, 2) |>
  pivot_wider(names_from = sex, values_from = c("obs", "height")) |>
  relocate(countryb, obs_male, height_male, obs_female, height_female)


# Visually

data |> 
  group_by(sex, countryb) |>
  summarize(
    obs = sum(!is.na(height)), 
    height = mean(height, na.rm = TRUE)) |> 
  ggplot(aes(x = countryb, y = height, color = sex)) +
  geom_point()

### Numerical variables

# Tables (class intervals)

data <- data |>
  mutate(weight_kg = 0.453592*weight)

data |> 
  mutate(age_class = cut(age, breaks = seq(9, 89, 10))) |>  
  filter(age>=20) |>
  group_by(age_class) |>
  summarise(obs = sum(!is.na(weight_kg)),
            mean_weight = mean(weight_kg, na.rm = TRUE))

# Line graph
data |> 
  filter(age>=20) |>
  mutate(age_class = cut(age, breaks = seq(20, 90, 10))) |>
  group_by(age_class) |>
  summarise(obs = sum(!is.na(weight)),
            mean_weight = mean(weight, na.rm = TRUE)) |>
  ggplot(aes(x = age_class, y = mean_weight, group = 1)) +
  geom_line()

## Do it by age (instead of age_class)

# Scatter plot
data |>
  filter(age>=20) |>
  ggplot(aes(x = age, y = weight)) +
  geom_point()

data |>
  filter(age>=20) |>
  ggplot(aes(x = age, y = weight_kg)) +
  geom_point() +
  stat_summary(geom = "line", fun = "mean", color = "red")   

# smoothing
data |>
  filter(age>=20) |>
  ggplot(aes(x = age, y = weight_kg)) +
  geom_point() +
  stat_summary(geom = "line", fun = "mean", color = "red") +  
  geom_smooth(se = FALSE)

# distinguishing by a third dimension
data |>
  filter(age>=20) |>
  ggplot(aes(x = age, y = weight_kg, color = sex)) +
  geom_point() +
  geom_smooth(se = FALSE)

### Evolution over time (history!)

# a table
height_year <- data |>
  mutate(year_birth = year - age) |>
  filter(sex=="male" & age>=20 & age<=50) |>
  group_by(year_birth) |>
  summarise(obs = sum(!is.na(height)),   
            av_height = round(mean(height, na.rm = TRUE), 1))
height_year

# visually
height_year |>
  ggplot(aes(x = year_birth, y = av_height)) +
  geom_point(col = "red") +
  geom_line()

## how to reduce the year-to-year variation (noise)? 
### bins, moving average, smoothing

# creating bins
data |> 
  mutate(year_birth = year - age) |>
  mutate(year_birth_5 = cut(year_birth, breaks =  seq(1795, 1885, 5))) |>
  filter(sex=="male" & age>=20 & age<=50) |>
  group_by(year_birth_5) |>
  summarise(
    av_height = mean(height, na.rm = TRUE)) |>
  ggplot(aes(x = year_birth_5, y = av_height)) +
  geom_point() + geom_line(group = 1)  +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# Given that the variable capturing the 5-year cohorts is a factor, 
# we need `group = 1` to let `ggplot` know that all the observations 
# should be treated as part of the same group, so the line can connect the points.

# Moving average (lags and leads)

height_year <- height_year |>
  arrange(year_birth) |>
  mutate(
    lag1 = lag(av_height, 1),
    lag2 = lag(av_height, 2),
    lead1 = lead(av_height, 1),
    lead2 = lead(av_height, 2))
height_year

height_year |>
  mutate(
    ma3 = (lag1 + av_height + lead1) / 3,
    ma5 = (lag2 + lag1 + av_height + lead1 + lead2) / 5) |>
  ggplot(aes(x = year_birth)) +
  geom_point(aes(y = av_height)) +
  geom_line(aes(y = av_height)) +
  geom_line(aes(y = ma3), color = "red") +
  geom_line(aes(y = ma5), color = "blue")


# Smoothing the trend (polynomial)
height_year |>
  ggplot(aes(x = year_birth, y = av_height)) +
    geom_point() +
    geom_line() +
    geom_smooth(se = FALSE)

# Comparing groups
data |> 
  mutate(year_birth = year - age) |>
  filter(countryb=="scotland" & age>=20 & age<=50) |>
  group_by(year_birth, sex) |>
  summarise(
    av_height = mean(height, na.rm = TRUE)) |>
  ggplot(aes(x = year_birth, y = av_height, color = sex)) +
  geom_point() +
  geom_smooth(se = FALSE)

  # try adjusting the smoothing parameter: span (0.25, 1.40)

data |> 
  mutate(year_birth = year - age) |>
  filter(countryb=="scotland" & age>=20 & age<=50) |>
  group_by(year_birth, sex) |>
  summarise(
    av_height = mean(height, na.rm = TRUE)) |>
  ggplot(aes(x = year_birth, y = av_height, color = sex)) +
  geom_point() +
  geom_smooth(span = 0.25, se = FALSE)

## showing all the raw data (but careful with the scale)
data |> 
  mutate(year_birth = year - age) |>
  filter(countryb=="scotland" & sex=="male" & age>=20 & age<=50) |>
  ggplot(aes(x = year_birth, y = height)) +
  geom_point() +
  stat_summary(geom = "line", fun = "mean") +
  geom_smooth(se = FALSE)

# Other statistics

data |>  
  mutate(year_birth = year - age) |>
  filter(age >= 18 & sex=="male" & year_birth>=1800) |>
  group_by(year_birth) |>
  summarise(sd = sd(height, na.rm = TRUE),
            mean = mean(height, na.rm = TRUE)) |> 
  mutate(cv = sd/mean) |>
  ggplot(aes(x = year_birth, y = cv)) +
  geom_point() +
  geom_line() + 
  geom_smooth(se = FALSE)

# qualitative variables: dummy variables

data |>
  mutate(write = ifelse(lit_adj=="write", 1, 0)) |>
  mutate(year_birth = year - age) |>  
  filter(age>=18 & age<=50) |>
  group_by(year_birth, sex) |>
  summarise(
    mean = mean(write, na.rm = TRUE)) |>
  ggplot(aes(x = year_birth, y = mean, color = sex)) +
  geom_smooth(se = FALSE) +
  geom_point()
