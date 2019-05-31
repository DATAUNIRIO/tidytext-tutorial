##================================================================================================
##                                                                                              
##    Nome:The game is afoot! Topic modeling of Sherlock Holmes stories                                            
##                                                    
##    site: https://juliasilge.com/blog/sherlock-holmes-stm/                                                                                                                                                 ##
##    
##    prof. Steven Dutt-Ross                          
##    UNIRIO           
##================================================================================================
 

library(tidyverse)
library(gutenbergr)

sherlock_raw <- gutenberg_download(1661)

sherlock <- sherlock_raw %>%
    mutate(story = ifelse(str_detect(text, "ADVENTURE"),
                          text,
                          NA)) %>%
    fill(story) %>%
    filter(story != "THE ADVENTURES OF SHERLOCK HOLMES") %>%
    mutate(story = factor(story, levels = unique(story)))

sherlock

#----------------------------------------------------------------------------------------

library(tidytext)

tidy_sherlock <- sherlock %>%
    mutate(line = row_number()) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    filter(word != "holmes")

tidy_sherlock %>%
    count(word, sort = TRUE)

#----------------------------------------------------------------------------------------
library(drlib)

sherlock_tf_idf <- tidy_sherlock %>%
    count(story, word, sort = TRUE) %>%
    bind_tf_idf(word, story, n) %>%
    arrange(-tf_idf) %>%
    group_by(story) %>%
    top_n(10) %>%
    ungroup

sherlock_tf_idf %>%
    mutate(word = reorder_within(word, tf_idf, story)) %>%
    ggplot(aes(word, tf_idf, fill = story)) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ story, scales = "free", ncol = 3) +
    scale_x_reordered() +
    coord_flip() +
    theme(strip.text=element_text(size=11)) +
    labs(x = NULL, y = "tf-idf",
         title = "Highest tf-idf words in Sherlock Holmes short stories",
         subtitle = "Individual stories focus on different characters and narrative elements")
#----------------------------------------------------------------------------------------

# POSSIVEL PROBLEMA
#Trying to compute distinct() for variables not found in the data:
# This is an error, but only a warning is raised for compatibility reasons.
#The operation will return the input unchanged. 

# SOLUCAO
#Had same error message when using cast_sparse(). Fixed with devtools::install_github("juliasilge/tidytext").
#devtools::install_github("juliasilge/tidytext")

library(quanteda)
library(stm)

sherlock_dfm <- tidy_sherlock %>%
    count(story, word, sort = TRUE) %>%
    cast_dfm(story, word, n)

sherlock_sparse <- tidy_sherlock %>%
    count(story, word, sort = TRUE) %>%
    cast_sparse(story, word, n)

topic_model <- stm(sherlock_dfm, K = 6, 
                   verbose = FALSE, init.type = "Spectral")

td_beta <- tidy(topic_model)

td_beta %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    mutate(topic = paste0("Topic ", topic),
           term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    labs(x = NULL, y = expression(beta),
         title = "Highest word probabilities for each topic",
         subtitle = "Different words are associated with different topics")


td_gamma <- tidy(topic_model, matrix = "gamma",                    
                 document_names = rownames(sherlock_dfm))

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
    geom_histogram(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ topic, ncol = 3) +
    labs(title = "Distribution of document probabilities for each topic",
         subtitle = "Each topic is associated with 1-3 stories",
         y = "Number of stories", x = expression(gamma))

