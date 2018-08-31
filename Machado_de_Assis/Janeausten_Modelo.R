# install.packages(c("tidytext","gutenbergr","stm","glmnet"))
# install.packages("tidyverse")
# http://www.gutenberg.org/browse/languages/pt#a9685
# https://www.tidytextmining.com/

library("tidytext")
library("gutenbergr")
library("stm")
library("glmnet")
library("tidyverse")



library(dplyr)
library(janeaustenr)
d <- data_frame(txt = prideprejudice)
d

d %>%
  unnest_tokens(word, txt)

d %>%
  unnest_tokens(sentence, txt, token = "sentences")

d %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 2)

d %>%
  unnest_tokens(chapter, txt, token = "regex", pattern = "Chapter [\\d]")

d %>%
  unnest_tokens(shingle, txt, token = "character_shingles", n = 4)

# custom function
d %>%
  unnest_tokens(word, txt, token = stringr::str_split, pattern = " ")

# tokenize HTML
h <- data_frame(row = 1:2,
                text = c("<h1>Text <b>is</b>", "<a href='example.com'>here</a>"))

h %>%
  unnest_tokens(word, text, format = "html")
