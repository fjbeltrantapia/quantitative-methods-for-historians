# clear de "Global Environment"
rm(list=ls()) 

# set working directory
setwd("/Volumes/francijb/Documents/FRAN/Teaching/QM_2024/session") 

# Install packages
# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("tokenizers")
# install.packages("stm")
# install.packages("SnowballC")

if (!requireNamespace("xfun")) install.packages("xfun")
xfun::pkg_attach2("tidyverse", "lubridate", "rvest", "stringr", "readtext", "tesseract", "tidytext", "SnowballC", "wordcloud", "wordcloud2", "widyr", "quanteda", "quanteda.textstats", "magrittr", "pdftools", "devtools", "tsne", "topicmodels", "readtext")
# The following two packages have Java dependencies that might give some (especially Windows) machines trouble. No worries if they don't load on your computer, I'll demonstrate running them on RStudio Cloud.
xfun::pkg_attach2("tabulizer", "openNLP")
install.packages("devtools")
devtools::install_github("bmschmidt/wordVectors")

# Open packages that we will be using
library(tidyverse)
library(tidytext)

library(SnowballC)

# Importing the data
data <- read_csv("data/state_of_the_union_texts.csv")
data

#### Tagging - Expanding on counting words (script 4) 
  # tagging the corpus into "parts of speech" and "named entities"

## Part-of-speech tagging
### identify verbes, nouns, etc.

# install.packages("udpipe")
library(udpipe)

  # first download and load the language library (English have several libraries)
pos_model <- udpipe_download_model("english-gum")
pos_model <- udpipe_load_model(file = pos_model$file_model)

  # example with the first sentence of the first speech
library(tokenizers)
sample_sentence <- tokenize_sentences(data$Text[1])[[1]][[1]]
tagged_pos <- udpipe_annotate(pos_model, x = sample_sentence)
as_tibble(tagged_pos)

### upos: universal part of speech tag
### xpos: language- (or corpus-) specific part of speech tag
| Tag         | Category                  |
  |-------------|---------------------------|
  | ADJ         | adjective                 |
  | ADP         | adposition                |
  | ADV: adverb | adverb                    |
  | AUX         | auxiliary                 |
  | CCONJ       | coordinating conjunction  |
  | DET         | determiner                |
  | INTJ        | interjection              |
  | NOUN        | noun                      |
  | NUM         | numeral                   |
  | PART        | particle                  |
  | PRON        | pronoun                   |
  | PROPN       | proper noun               |
  | PUNCT       | punctuation               |
  | SCONJ       | subordinating conjunction |
  | SYM         | symbol                    |
  | VERB        | verb                      |
  | X           | other                     |

## finding the most commonly occuring nouns
data_pos <- udpipe_annotate(pos_model,
                            x = data$Text,
                            doc_id = data$Year)    
data_pos <- as_tibble(data_pos) |> 
  mutate(Year = as.numeric(doc_id)) # more coherent with our dat
data_pos |> 
  filter(upos == "NOUN") |> 
  count(lemma, sort=TRUE) |>  
  top_n(15) |>                 
  mutate(lemma = reorder(lemma,n)) |>  
  ggplot(aes(lemma, n)) + 
  geom_col() + 
  coord_flip()
  
## another example more for language/literary analyses: 
  # chart the number of adjectives, nouns and verbs (as a percentage of total words)

data_pos |> 
  group_by(Year) |> 
  summarize(word_count = n(), 
            nouns = sum(upos == "NOUN"), 
            verbs = sum(upos == "VERB"),
            adj = sum(upos == "ADJ")) |> 
  mutate(nouns = nouns/word_count, 
         verbs = verbs/word_count, 
         adj = adj/word_count) |> 
  select(!word_count) |> 
  pivot_longer(!Year, names_to = "POS", values_to = "Count") |> 
  ggplot() +
  geom_line(aes(x = Year, y = Count, group = POS, color = POS)) +
  scale_y_continuous(labels = scales::label_percent()) + # labels as %
  scale_x_continuous(breaks = seq(1905, 2021, 10))


## Named entity recognition (NER)
install.packages("entity") # wrapped around NLP and openNLP
  # download the zip ball here: 
  # https://github.com/trinker/entity/?tab=readme-ov-file#installation
  # and run:
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load_gh("trinker/entity")

library(entity)
  
  # entity can annotate six kinds of names: 
  ## persons, locations, organizations, dates, mentions of money, and dates
  ## person_entity(), location_entity(), organization_entity(), ... 

## example with locations (what type before and after 1900?)
location_entity(data$Text[5]) 

locations <- data |> 
  mutate(locations = location_entity(Text)) 
locations |>   
  mutate(Period = ifelse(Year<1900, "XIX c.", "XX c."))  |>  
  mutate(Period = factor(Period, levels = c("XIX c.", "XX c."))) |> 
  unnest(locations)|> # instead of unnest_tokens (unnesting a list now)
  group_by(Period) |>                                            
  count(locations, sort=TRUE) |> 
  mutate(proportion = 1000*n/sum(n)) |>                    
  top_n(15) |> 
  ggplot(aes(x = reorder_within(x = locations, 
                                by = proportion, 
                                within = Period), 
             y = proportion, 
             fill = Period)) +    
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~Period, ncol = 2, scales = "free") +
  labs(x = "Word", 
       y = "Frequency per 1000 words", 
       title = "Location frequencies") +  
  theme_bw() +
  guides(fill="none") 

  ### we could now map this (geocoding first to extract xy-coordinates)

#### TF-IDF Term frequency - Inverse document frequency
  # Identifying words that are unique (or show up less often) to certain documents (speeches, periods, etc.)
  # No need to "stopwords" because words that are common in all docs are unimportant here

data <- data %>%
  mutate(period = case_when(
  Year<1914 ~ "Pre-1914",
  Year>=1914 & Year<=1945 ~ "World Wars",
  Year>1945 ~ "Post-1945")) %>%
  mutate(period = factor(period, 
                         levels = c("Pre-1914", "World Wars", "Post-1945"),
                         ordered = TRUE))

data_token <- data %>%
  unnest_tokens(output = words, input = Text) %>%       # tokenize first
  count(words, period, sort = TRUE) # count them

tf_idf <- data_token %>%
  bind_tf_idf(words, period, n) # distinguish documents by year
  # the commonest words are weighted away (their tf-idf score is 0)

tf_idf
  # tf - term frequency
  # idf - inverse document frequency
  # tf_idf - identifies the most unique words in each year

# Check the highest scores
tf_idf %>% arrange(desc(tf_idf)) 

# You could now identify which are the most unique words in each speech


#### POS (Part of Speech)- Identify types of words: nouns, verbs, adjectives...

# There is an in-built list of words identifying POS
parts_of_speech

data_token <- data %>%
  unnest_tokens(output = word, input = Text) %>%  # the data we will be using (tokenised)
  inner_join(parts_of_speech, relationship = "many-to-many") %>%                 # join the two datasets using "word" as the "matching" field
  count(pos)
data_token


# There are more sophisticated POS tagging would require the context of the sentence structure
# It is though beyond what we cover here and involve other packages ("NLP", "openNLP", "tm")
# See here:
# https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html


#### Sentiment analysis
# install.packages("textdata")

library(tidytext)
library(textdata)

# Tidytext includes three dictionaries with "sentiments": afinn, bing & nrc

get_sentiments("afinn")
  # the first time you will need to say yes to download of the sentiment dictionary
tail(get_sentiments("afinn"))
get_sentiments("afinn") %>%
  summary(value) 
  # summary statistics for the value column (from -5 to 5)

get_sentiments("bing")


# Count words from the lexicons that appear in a text and add them all up

# (positives - negatives)
data_token_stop <- data %>%
  unnest_tokens(output = words, input = Text) %>%   # tokenise
  anti_join(stop_words, by = c("words" = "word"))   # remove stop words

sentiment <- data_token_stop %>%
  rename("word" = "words") %>% # we call our new column "word" which makes inner_joins easier 
  inner_join(get_sentiments("bing"), relationship = "many-to-many") %>%
  count(sentiment, year = Year) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) 

sentiment %>%
  ggplot(aes(year, sentiment)) +
    geom_line(show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = 2, alpha = .8)

sentiment %>%
  filter(year > 1950 & sentiment > 400)

# using the afinn lexicon
data_token_stop %>%
  rename("word" = "words") %>% # we call our new column "word" which makes inner_joins easier 
  inner_join(get_sentiments("afinn")) %>%
  group_by(Year) %>%
  summarize(sentiment = sum(value)) %>%
  ggplot(aes(Year, sentiment)) +
    geom_line(show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = 2, alpha = .8)


# Putting both analysis together

bing <- data_token_stop %>%
  rename("word" = "words") %>% # we call our new column "word" which makes inner_joins easier 
  inner_join(get_sentiments("bing"), relationship = "many-to-many") %>%
  count(sentiment, year = Year) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(bing = positive - negative) 

afinn <- data_token_stop %>%
  rename("word" = "words") %>% # we call our new column "word" which makes inner_joins easier 
  inner_join(get_sentiments("afinn")) %>%
  group_by(Year) %>%
  summarize(afinn = sum(value))

bing %>%
  inner_join(afinn, by = c("year" = "Year")) %>%
  pivot_longer(c("bing", "afinn"), names_to = "sentiment", values_to = "counts") %>%
  ggplot(aes(x = year, y = counts, color = sentiment)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = 2, alpha = .8)



# Topic models
  # discovering the abstract "topics" that occur in a collection of documents
  # unveils hidden semantic structures in a text body
  # it works with matrixes of words/documents
  # more details here: 
  # https://towardsdatascience.com/latent-dirichlet-allocation-lda-9d1cd064ffa2

# install.packages('topicmodels')
# install.packages('stm')

options(stringsAsFactors = FALSE)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(stm)
library(SnowballC)

# transform dataframe to DTM - document term matrix
data_dtm <- data_token_stop %>%
  mutate(word_stem = wordStem(words)) %>% # stemming
  select(President, period, word_stem) %>%
  rename(President = President, words = word_stem) %>%
  group_by(period) %>%
  count(words, sort = TRUE) %>%
  cast_dtm(period, words, n)
data_dtm

# transform to tm (adjust the parameters)
k = 10      # number of topics
alpha = 2   # how many topics may dominate each text
data_tm <- LDA(data_dtm, k = 10, alpha = 2) 
  # LDA: Latent Dirichlet Allocation (algorithm)
  # choosing the number of topics is a bit of an art

?LDA

# look at the output of the topic model
str(posterior(data_tm))

# highest words in each topic (do the topics make sense for a human?)
terms(data_tm, 15)

# transform the output back to the tidy format (so we can better work with it)
# install.packages("tidyr")
# install.packages("reshape2")
library(tidytext)
library(tidyr)

terms <- tidy(data_tm, matrix = "beta")
terms
  # the topics are given numbers
  # you can later "name" them 

words_in_topics <- terms %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)
words_in_topics

words_in_topics %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()




#### Co-occurence
  # Words that appear together (although not necessarily right next to each other)
# install.packages("widyr")
library(widyr)

data_adj <- data %>%
  unnest_tokens(output = words, input = Text) %>%   # tokenize
  anti_join(stop_words, by = c("words" = "word")) %>%   # drop stop words
  pairwise_count(words, Year, sort = TRUE)
  # Count the number of times each pair of items appear together within a group

data_adj

## Exercises:
  # Analise the evolution of the importance of "education" (and related words) in the speeches
  # What time of words show up in the context of education? Do they change over time?


Corpuscle: corpuses
https://clarino.uib.no/korpuskel/home

