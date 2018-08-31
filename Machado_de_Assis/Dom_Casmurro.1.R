# install.packages(c("tidytext","gutenbergr","stm","glmnet"))
# install.packages("tidyverse")
# http://www.gutenberg.org/browse/languages/pt#a9685
# https://www.tidytextmining.com/

library("tidytext")
library("gutenbergr")
library("stm")
library("glmnet")
library("tidyverse")

##########################################################################

# Download do Livro
DomCasmurro<-gutenberg_download(55752)
DomCasmurro

# modificacao dos caracteres latinos
encoding<-DomCasmurro   %>%
  mutate(text = iconv(text, from="latin1", to="UTF-8"))

# tansformando em tidydata (uma observacao por linha)
DomCasmurrotidy <- encoding %>%
  mutate(line=row_number()) %>%
  unnest_tokens(word,text)
#  unnest_tokens(word,text,strip_punct=FALSE)

# vendo as palavras que mais ser repetem no Dom Casmurro
DomCasmurrotidy %>%
  count(word,sort = TRUE)

# tirando as palavras comuns (o, a, que, de, um, uma, etc) - palavras banidas
pt_stop<-get_stopwords(language = "pt")

palavras_extras<- data.frame(word = c("é","á","tambem","assim","ha","ainda","outra"),lexicon=rep("customizado",7),stringsAsFactors = FALSE)

# juntando as minhas palavras com a lista de palavras banidas
palavra_onibus<-pt_stop %>% bind_rows(palavras_extras)

# lista de palavras utilizadas por Machado de Assis (lista de palavras automaticas banidas)
DomCasmurrotidy %>%
  anti_join(get_stopwords(language = "pt")) %>%
  count(word,sort = TRUE)

# lista de palavras utilizadas por Machado de Assis (lista de palavras banidas com eh e ah )
DomCasmurrotidy %>%
  anti_join(palavra_onibus) %>%
  count(word,sort = TRUE)

DomCasmurrotidy %>%
  anti_join(palavra_onibus) %>%
  count(word,sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(fct_reorder(word,n),n)) +
  geom_col()+
  coord_flip()+
  xlab("Palavra")

# Ainda nao funciona no brasil, mas eh a lista de palavras para analise de sentimentos
# afinn, bing, nrc e loughran sao metodos de escolha de palavras
#get_sentiments("afinn")
#get_sentiments("bing")
#get_sentiments("nrc")
#get_sentiments("loughran")

##########################################################################
##########################################################################
##########################################################################
#A Mao e A Luva = 53101
#Esau e Jacob = 56737
#Memorias Posthumas de Braz Cubas = 54829
#Quincas Borba =  55682
#Dom Casmurro  =55752
# José de Alencar: 5 minutos =44540

# baixando todos os livros
full_collection <- gutenberg_download(c(55752,54829,55682)) %>%
                   gutenberg_download(meta_fields = "title")

# full_collection <- gutenberg_download(55752) %>%
#                    gutenberg_download(54829) %>%
#                    gutenberg_download(55682) %>%
#                    gutenberg_download(meta_fields = "title")

full_collection

# modificacao dos caracteres latinos
encoding<-full_collection   %>%
  mutate(text = iconv(text, from="latin1", to="UTF-8"))

# tansformando em tidydata (uma observacao por linha - uma palavra por linha)
book_words <- encoding %>%
  mutate(line=row_number()) %>%
  unnest_tokens(word,text) %>%
  count(title,word,sort = TRUE)

# calculando o indice tf_idf
book_words<- book_words %>%
  bind_tf_idf(word,title,n)
book_words

# Palavras mais utilizadas em cada livro
book_words %>%
  group_by(title) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(fct_reorder(word, tf_idf), tf_idf, 
             fill = title)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~title, scales = "free")

# que palvras ficam juntas (correlacao de palavras)
tidy_ngram<- DomCasmurro %>%
      unnest_tokens(bigram,text, token= "ngrams",n=2)
  
tidy_ngram %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

## What can you do with n-grams?
#- tf-idf of n-grams
#- network analysis
#- negation

#########################################################################
#########################################################################
# Machado de Assis: Dom Casmurro  =55752
# José de Alencar: Cinco minutos =44540
#########################################################################
#########################################################################
library(tidyverse)
library(gutenbergr)


titles<-c("Dom Casmurro","Cinco minutos")
titles<-c(55752,44540)
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title") %>% 
  mutate(document = row_number())


titles<-c("The War of the Worlds","Pride and Prejudice")
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title") %>% 
                       mutate(document = row_number())

# palavras que repetiram 50 vezes pelo menos                            
tidybooks <-books %>%
  unnest_tokens(word,text) %>%
  group_by(word) %>%
  filter(n()>50) %>%
  ungroup()

tidybooks

## Cast to a sparse matrix
#And build a dataframe with a response variable
tidy_books <- tidybooks
sparse_words <- tidy_books %>%
  count(document, word, sort = TRUE) %>%
  cast_sparse(document, word, n)

books_joined <- data_frame(document = as.integer(rownames(sparse_words))) %>%
  left_join(books %>%
              select(document, title))

## Train a glmnet model
library(glmnet)
is_jane <- books_joined$title == "Pride and Prejudice"

model <- cv.glmnet(sparse_words, is_jane, family = "binomial", 
                   parallel = TRUE, keep = TRUE)


## Tidying our model


library(broom)

coefs <- model$glmnet.fit %>%
  tidy() %>%
  tbl_df() %>%     ## filter to choose some lambda from glmnet output
  filter(lambda >= 1.2 * model$lambda.1se) %>% 
  filter(lambda == min(lambda))

Intercept <- coefs %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)

## Tidying our model

classifications <- tidy_books %>%
  inner_join(coefs, by = c("word" = "term")) %>%
  group_by(document) %>%
  summarize(Score = sum(estimate)) %>%
  mutate(Probability = plogis(Intercept + Score))

classifications

## Understanding our model

coefs %>%
  group_by(estimate > 0) %>%
  top_n(15, abs(estimate)) %>%
  ungroup %>%
  ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip()

coefs %>%
  group_by(estimate > 0) %>%
  top_n(15, abs(estimate)) %>%
  ungroup %>%
  ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = NULL,
       title = "Coefficients that increase/decrease probability of classification",
       subtitle = "A document mentioning Martians is unlikely to be written by Jane Austen")


comment_classes <- classifications %>%
  left_join(books %>%
              select(title, document), by = "document") %>%
  mutate(Correct = case_when(title == "Pride and Prejudice" ~ TRUE,
                             TRUE ~ FALSE))

roc <- comment_classes %>%
  arrange(desc(Probability)) %>%
  mutate(TPR = cumsum(Correct) / sum(Correct),
         FPR = cumsum(!Correct) / sum(!Correct),
         FDR = cummean(!Correct))

## ROC
roc %>%
  arrange(Probability)


roc %>%
  arrange(Probability) %>%
  ggplot(aes(FPR, TPR)) +
  geom_abline(lty = 2, alpha = 0.5, 
              color = "gray50",
              size = 1.5) + 
  geom_line(color = "midnightblue",
            size = 1.8) +
  labs(title = "ROC for text classification")

roc %>%
  summarise(AUC = sum(diff(FPR) * na.omit(lead(TPR) + TPR)) / 2)



## Misclassifications
# Let's talk about misclassifications. Which documents here were incorrectly predicted to be written by Jane Austen?

roc %>%
  filter(Probability > .8, !Correct) %>%
  sample_n(10) %>%
  inner_join(books %>%
               select(document, text)) %>%
  select(Probability, text)

## Misclassifications

#Let's talk about misclassifications. Which documents here were incorrectly predicted to *not* be written by Jane Austen?

roc %>%
filter(Probability < .2, Correct) %>%
sample_n(10) %>%
inner_join(books %>%
select(document, text)) %>%
select(Probability, text)