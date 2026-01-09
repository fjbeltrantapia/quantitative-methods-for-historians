rm(list=ls()) # Clear de "Global Environment"
setwd("~/Library/CloudStorage/OneDrive-NTNU/course-websites/quantitative-methods-for-historians")

# Create the Soho map
library(tidyverse)
library(tidytext)
library(SnowballC)
library(textstem)

tail(data)

data <- read_csv("data/state-of-the-union-texts.csv")
data_token <- data |>
  unnest_tokens(output = words, input = Text) |>
  anti_join(stop_words, by = c("words" = "word")) |>
  mutate(lemmas = lemmatize_words(words)) |>
  mutate(women = if_else(lemmas=="wom", 1, 0)) |>
  group_by(Year) |>
  summarise(women = mean(women, na.rm = TRUE)) |>
  ggplot(aes(x = Year, y = women)) +
  geom_line()

plot <- data |>
  mutate(wc = str_count(Text, "[:alpha:]+"),
         Women = 1000*str_count(Text, " [Ww]om[ae]n ")/wc,
         Men = 1000*str_count(Text, " [Mm][ae]n ")/wc) |>
  pivot_longer(c("Women", "Men"), names_to = "word", values_to = "counts") |>  
  group_by(Year, word) |> # there are 2 speeches in 1790
  summarize(counts = mean(counts, na.rm = TRUE)) |>
  ggplot(aes(x = Year, y = counts, color = word)) +
  geom_line() +
  scale_color_manual(name = "",
                     values = c("blue", "red")) +
  scale_x_continuous("", breaks = seq(1800, 2025, 25)) +
  scale_y_continuous("Frequency (per 1,000 words") +
  theme_minimal() +
  theme(legend.position = c(0.9, 1),
        legend.justification = c("right", "top"),
        legend.text  = element_text(size = 11))


ggsave(plot, filename = "images/plot-speeches.tiff",
          width    = 7,
          height   = 5)

ggsave(plot, filename = "images/plot-speeches.png",
       width    = 7,
       height   = 5)