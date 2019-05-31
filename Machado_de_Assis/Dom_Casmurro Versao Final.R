##================================================================================================
##                                                                                              
##    Nome: Shakespeare ou Machado de Assis                                           
##
##    
##    prof. Steven Dutt-Ross                          
##    UNIRIO           
##================================================================================================


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

# modificacao dos caracteres latinos v1
#DomCasmurro<-rm_accent(DomCasmurro)

# modificacao dos caracteres latinos v2
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


# que palvras ficam juntas (correlacao de palavras)
tidy_ngram<- DomCasmurro %>%
    unnest_tokens(bigram,text, token= "ngrams",n=2)

tidy_ngram %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)

tidy_ngram %>%
    count(bigram, sort = TRUE)

tidy_ngram %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)

## What can you do with n-grams?
#- tf-idf of n-grams
#- network analysis
#- negation

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
#José de Alencar: 5 minutos =44540
#Othello =28526
##########################################################################
##########################################################################
##########################################################################

# Exemplo original
# titles <- c("The War of the Worlds",
#             "Pride and Prejudice")
# 
# books <- gutenberg_works(title %in% titles) %>%
#     gutenberg_download(meta_fields = "title") %>%
#     mutate(document = row_number())
# 
# books

# baixando todos os livros
full_collection <- gutenberg_download(c(55752,28526))%>%
    mutate(document = row_number())

full_collection$title<-full_collection$gutenberg_id
full_collection$title<-gsub(55752,"Dom Casmurro",full_collection$title)
full_collection$title<-gsub(28526,"Othello",full_collection$title)


# modificacao dos caracteres latinos
encoding<-full_collection   %>%
  mutate(text = iconv(text, from="latin1", to="UTF-8"))

# tansformando em tidydata (uma observacao por linha - uma palavra por linha)
book_words <- encoding%>%
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


#########################################################################
#########################################################################
#########################################################################
#########################################################################

# palavras que repetiram 100 vezes pelo menos                            
tidybooks <-encoding %>%
  unnest_tokens(word,text) %>%
  group_by(word) %>%
  filter(n()>100) %>%
  ungroup()

tidybooks

## Cast to a sparse matrix
#And build a dataframe with a response variable

# POSSIVEL PROBLEMA
#Trying to compute distinct() for variables not found in the data:
# This is an error, but only a warning is raised for compatibility reasons.
#The operation will return the input unchanged. 

# SOLUCAO
#Had same error message when using cast_sparse(). Fixed with devtools::install_github("juliasilge/tidytext").
#devtools::install_github("juliasilge/tidytext")


tidy_books <- tidybooks
sparse_words <- tidy_books %>%
  count(document, word, sort = TRUE) %>%
  cast_sparse(document, word, n)

books_joined <- data_frame(document = as.integer(rownames(sparse_words))) %>%
  left_join(encoding %>%
              select(document, title))

## Train a glmnet model
library(glmnet)
eh_o_Machado <- books_joined$title == "Dom Casmurro"

model <- cv.glmnet(sparse_words, eh_o_Machado, family = "binomial", 
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
       title = "Coeficientes que aumentam / diminuem a probabilidade de classificação",
       subtitle = "É improvável que um documento mencionando Desdêmona seja escrito por Machado de Assis")

comment_classes <- classifications %>%
  left_join(encoding %>%
              select(title, document), by = "document") %>%
  mutate(Correct = case_when(title == "Dom Casmurro" ~ TRUE,
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
  labs(title = "ROC para classificação de texto")

roc %>%
  summarise(AUC = sum(diff(FPR) * na.omit(lead(TPR) + TPR)) / 2)


## Misclassifications
# Vamos falar sobre erros de classificação. 
# Quais documentos aqui foram incorretamente previstos para serem escritos por Machado de Assis?
# (acho que não está funcionando)

roc %>%
  filter(Probability > .8, !Correct) %>%
  sample_n(15) %>%
  inner_join(encoding %>%
               select(document, text)) %>% 
    select(Probability, text)

## Misclassifications
# Vamos falar sobre erros de classificação. 
# Quais documentos aqui foram incorretamente previstos para *não* serem escritos por Machado de Assis?

roc %>%
    filter(Probability < .2, Correct) %>%
    sample_n(15) %>%
    inner_join(full_collection %>%
               select(document, text)) %>%
    select(Probability, text)



#https://github.com/sillasgonzaga/lexiconPT