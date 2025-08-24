# clear de "Global Environment"
rm(list=ls()) 

# set working directory
setwd("/Volumes/francijb/Documents/FRAN/Teaching/QM-2024/session") 

# Install packages
# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("tokenizers")

# Open packages that we will be using
library(tidyverse)
library(tidytext)

# Importing the data
data <- read_csv("data/state_of_the_union_texts.csv")

# Having a look at the data
data
print(data)
print(data, n=20)
print(data, n=Inf)
View(data)

# In case it was not a tibble, convert to the tidyverse's format for dataframes
data <- as_tibble(data)

data %>% 
  count(President)

data %>%
  count(Year) %>% print(n = Inf)


# Reporting a particular speech (number 4 in this case)
data$Text[4] 
  # will show the fourth line of the text column of the "data" dataframe



#### n-grams (number of particular words in the texts)
data <- data %>%
  mutate(peace = str_count(Text, "[Pp]eace")) %>%
  mutate(war = str_count(Text, "[Ww]ar")) 

# summary statistics
data %>%
  summarize(obs = sum(!is.na(war)),
            sum = sum(war),
            mean = mean(war, na.rm = TRUE)) 

data %>%
  group_by(Year) %>%
  summarize(sum = sum(war), 
            mean = mean(war, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = mean)) +
  geom_line()

data %>%
  filter(Year>1790) %>%
  ggplot(aes(x = Year, y = war)) +
  geom_line()

data %>%
  filter(war>150)

# aggregate the data into periods
data %>%
  mutate(period = case_when(
    Year<1914 ~ "Pre-1914",
    Year>=1914 & Year<=1945 ~ "World Wars",
    Year>1945 ~ "Post-1945")) %>%
  mutate(period = factor(period, 
                         levels = c("Pre-1914", "World Wars", "Post-1945"),
                         ordered = TRUE)) %>%
  group_by(period) %>%
  summarize(sum = sum(war), 
            mean = mean(war, na.rm = TRUE)) 
  
    # factor ranks them (instead of alphabetically, as with strings)

# Plots
data %>%
  group_by(Year) %>%
  summarize(peace = sum(peace), war = sum(war)) %>%
  pivot_longer(c("peace", "war"), names_to = "word", values_to = "counts") %>%
  ggplot(aes(x = Year, y = counts, color = word)) +
  geom_line()

data %>%
  filter(Year>1790) %>%
  pivot_longer(c("peace", "war"), names_to = "word", values_to = "counts") %>%  
  ggplot(aes(x = Year, y = counts, color = word)) +
  geom_line()

data %>%
  filter(Year>1840 & Year<1855 & war>50)

  # do it with "[Ww]omen"
data <- data %>% mutate(women = str_count(Text, "[Ww]om[ae]n"))

data %>%
  group_by(Year) %>%
  summarize(women = sum(women)) %>%
  ggplot(aes(x = Year, y = women)) +
    geom_line()


data %>%
  filter(Year>1790) %>%
  ggplot(aes(x = Year, y = women)) +
  geom_line()
  # no need to group by year if you exclude the first year (with 2 speeches)



#### Compute "word counts"

# Counting all words or numbers that are separated by spaces on either side
data <- data %>%
  mutate(wc = str_count(Text, "[\\w]+"))
  # [\\w]+ is a regular expression (regex or regexp)
    # a sequence of characters that specifies a match pattern in text
  # more on this here: https://regexr.com
  # this other handy tool provides a quick references to regular expressions 
    # and also allows testing them: https://regex101.com

# Graph it
data %>%
  ggplot(aes(x = Year, y = wc)) + 
  geom_point() + 
  geom_line()
# just line graph
# we might add geom_point() to underscore that we have missing years

data %>% 
  filter(Year>1940 & wc>20000)

data %>% 
  mutate(war_rel = war/wc) %>%
  mutate(war_rel2 = 1000*war/wc) %>%
  filter(Year>1790) %>%
  ggplot(aes(x = Year, y = war_rel)) + 
  geom_point() + 
  geom_line()

#### "tokenizing"  
  # removes all of the punctuation, 
  # splits the text into individual words, and 
  # converts everything into lowercase characters

data_token <- data %>%
  unnest_tokens(output = words, input = Text)

data_token # almost 1.8 million entries


#### Top word frequencies (the most common words)
data_token %>%
  count(words, sort = TRUE) %>% 
  print(n = 20) # "Inf" display all rows
  # the most common words have no meaning

#### stop words 
  # words that are so common that we are not interested in them 
  # already built within the tidy environment

stop_words %>% print(n = 25)
  # note that the stop words are in the "word" column

# Exclude stop_words from the tokens
data_token_stop <- data_token %>%
  anti_join(stop_words, by = c("words" = "word"))
  # anti_join drops the words matching in both "datasets"
  # "words" & "word" are the names of the fields in each "dataset"

data_token_stop %>%
  count(words, sort = TRUE) %>% 
  print(n = 20)

# Create our own stop words (or add more to the existing list)
stop_words
my_stop_words <- c("peace", "war")
my_stop_words
custom_stop_words <- tibble(word = my_stop_words, lexicon = "my_customization")
custom_stop_words
stop_words_custom <- rbind(stop_words, custom_stop_words)
stop_words_custom
tail(stop_words_custom) # view the end of the tibble, look like our words were added correctly

data_token_stop # reduce to almost 700,000 entries

# Graphing - Top word frequencies
data_token_stop %>%
  count(words, sort=TRUE) %>%
  top_n(15) %>%                     # selecting to show only top 15 words
  mutate(words = reorder(words, n)) %>%  # highest frequency words appear first
  ggplot(aes(words, n)) +
    geom_col() +
    coord_flip()

# Focusing on a particular period (1850-1900)
data_token_stop %>%
  filter(Year >1850 & Year < 1900) %>%
  count(words, sort=TRUE) %>%
  top_n(15) %>%                     # selecting to show only top 15 words
  mutate(words = reorder(words, n)) %>%  # this will ensure that the highest frequency words appear to the left
  ggplot(aes(words, n)) +
  geom_col() +
  coord_flip()

# Comparing periods
data_token_stop <- data_token_stop %>%
  mutate(period = ifelse(Year <= 1900, "19th c.", "20th c."))

data_token_stop %>%
  group_by(period) %>%
  count(words, sort=TRUE) %>%
  mutate(proportion = n / sum(n) * 1000) %>%  # word freq- per 1000 words instead of counts
  slice_max(order_by=proportion, n = 15) %>%  # selecting to show only top 15 words
  mutate(words = reorder(words, desc(proportion))) %>%  # this will ensure that the highest frequency words appear to the left
  ggplot(aes(reorder_within(x = words, by = proportion, within = period), proportion, fill = period)) +    # reordering is a bit tricky, see                                                                                                     ?reorder_within()
    geom_col() +
    scale_x_reordered() +
    scale_fill_manual(values = c("blue", "#56B4E9")) +
    coord_flip() +
    facet_wrap(~period, ncol = 2, scales = "free") +
    xlab("Word")

?ggplot

# Focusing on particular words
data_token_stop <- data_token_stop %>%
  mutate(war = ifelse(words == "war", 1, 0))

data_token %>%
  filter(Year>=1900 & Year<2000) %>%
  mutate(women = ifelse(words == "women", 1, 0)) %>%
  count(women)

# Evolution over time
data_token_stop %>%   
  group_by(Year) %>%
  summarize(fr_war = mean(war, na.rm = TRUE)) %>%
  ggplot() +
    geom_col(aes(x = Year, y = fr_war))


# Stemming
  # looking at the common root of similar words

# install.packages(SnowballC)
library(SnowballC)

data_token_stop_stem <- data_token_stop %>%
  mutate(word_stem = wordStem(words))
data_token_stop_stem

data_token_stop_stem %>%
  count(word_stem, sort=TRUE) %>% 
  print(n = 30)

data_token_stop_stem %>%
  count(word_stem, sort=TRUE) %>%
  top_n(15) %>%                     # selecting to show only top 15 words
  mutate(word_stem = reorder(word_stem, n)) %>%  # this will ensure that the highest frequency words appear to the left
  ggplot(aes(word_stem, n)) +
  geom_col() + coord_flip()


# We could do the whole process simultaneously
data_adj <- data %>%
  unnest_tokens(output = words, input = Text) %>%       # tokenize
  anti_join(stop_words, by = c("words" = "word")) %>%   # drop stop words
  mutate(word_stem = wordStem(words))

data_adj

data_adj <- data_adj %>%
  mutate(war = ifelse(words == "war", 1, 0)) %>%
  mutate(peace = ifelse(words == "peace", 1, 0))

data_adj %>%   
  group_by(Year) %>%
  summarize(fr_war = mean(war, na.rm = TRUE),
            fr_peace = mean(peace, na.rm = TRUE)) %>%
  ggplot(aes(x = Year)) +
    geom_line(aes(y = fr_war), color = "red") +
    geom_line(aes(y = fr_peace), color = "blue")


#### n-grams: multiple-word tokens

data_token2 <- data %>%
  unnest_tokens(twogram, Text, token = "ngrams", n = 2)
data_token2

# in separate columns
data_token2 <- data_token2 %>%
  separate_wider_delim(cols = twogram, delim = " ", names = c("g1", "g2"))
data_token2

# identify words accompanying particular words
women <- data_token2 %>%
  filter(g1 == "women" | g2 == "women") %>%
  pivot_longer(g1:g2) %>% # put both columns in the same one
  select(!name) %>% # drop the variable we are not using
  rename(words = value) %>% # rename the new column we created
  filter(words!="women") %>% # drop the word women (we are interested in those around)
  anti_join(stop_words, by = c("words" = "word")) 

women %>%
  count(words, sort=TRUE) %>%
  print(n = 25)

women %>% 
  filter(words == "pregnant") %>%
  count(Year) 


# Exercise: 
  # think about a potential topic (word) 
  # explore the words around it (2 in each side)


# Further topics:
  # TF-IDF Term frequency - Inverse document frequency
  # POS (Part of Speech) - nouns, verbs, adjectives...
  # Sentiment analysis
  # Topic models
  # Co-occurrence



# check women / men

data %>%
  mutate(women = str_count(Text, "[Ww]omen")) %>%
  summarize(women = sum(women)) # 300 

data_token %>%
  mutate(women = if_else(words, "women")) %>%
  summarize(women = sum(women)) # 302 

data_token2 %>%
  mutate(women = str_count(g2, "[Ww]omen")) %>%
  summarize(women = sum(women)) # 302 in either g1 and g2 

