## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.

library(tidyverse) # metapackage with lots of helpful functions
knitr::opts_chunk$set(echo = TRUE)
library(tidytext)
library(stringr)
library(scales)
library(forcats)
library(wordcloud)
devtools::install_github("cpsievert/LDAvisData")
library(LDAvis)
library(jsonlite)
library(tm)
## Running code


## Reading in files

folder = "/home/partha/Desktop/Assignment/Level 2/BBC News Summary/"

txt_from_folder <- function(subfolder) {
  path = str_c(folder, subfolder)
  print(path)
  files <- list.files(path, pattern = "txt") #список файлов в папке
  
  for (i in (1:length(files))) {
    filename <- str_c(path, files[i])
    lines <- read_lines(filename,
                        locale = locale(encoding = "UTF-8"))
    lines <- lines[lines != '']
    text <- data_frame(doc_name = str_c(str_replace(files[i],".txt",""), subfolder), 
                       line = 1:length(lines), text = lines, theme = subfolder)
    
    if (i==1) {texts <- text}
    else {texts <- union_all(texts, text)}
  }
  
  texts
}

## Download to start two topics: politics and business

docs_politics <- txt_from_folder("News Articles/politics/")
docs_business <- txt_from_folder("News Articles/business/")
head(docs_politics)
full_df <- union_all(docs_business, docs_politics)

articles <- full_df %>%
  unnest_tokens(word, text)


articles %>%
  count(word, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  ungroup()

### So, in the first ten of the most common words - only general purpose words, all kinds of articles.
#### Delete them using the stop_word list from the tidytext package:

tidy_articles <- articles %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  anti_join(stop_words) %>%
  filter(str_detect(word, "[a-z']"))

tidy_articles %>%
  group_by(theme) %>%
  count(word, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = theme)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Most frequently words in theme",
       x = "Words", y = "Amount of words in theme",
       fill = "Papers")
### In addition, we desperately need to build a cloud of fun colored tags:
tidy_articles %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors=brewer.pal(8, "Dark2")))

### The calculated the correlation of topics here and made some conclusion from this:
frequency <- tidy_articles %>%
  count(theme, word) %>%
  group_by(theme) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(theme, proportion, fill = 0)
head(frequency)

cor.test(data = frequency, 
         ~ `News Articles/business/` + `News Articles/politics/`)
### We build a frequency graph for topics before and after deleting stop words:
article_words <- full_df %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  filter(!is.na(word))

word_freq <- article_words%>%
  count(theme, word) %>%
  group_by(theme) %>%
  mutate(freq = n / sum(n)) 

word_freq2 <- tidy_articles %>%
  mutate(theme = str_c(theme,  " without stop words")) %>%
  count(theme, word) %>%
  group_by(theme) %>%
  mutate(freq = n / sum(n))



union_all(word_freq, word_freq2) %>% 
  ggplot(aes(freq, fill = theme)) +
  geom_histogram(show.legend = FALSE, bins = 40) +
  #xlim(NA, 0.006) +
  facet_wrap(~theme, ncol = 2) +
  labs(title = "Word frequency distribution",
       y = "Amount of words", x = "Frequency")

### Now we will consider the tf-idf metric, which is a measure of how unique the word is for its topic.
#### So we find out which words are topic markers

word_tf_idf <- word_freq %>%
  bind_tf_idf(word, theme, n) %>%
  select(-freq) 

head(word_tf_idf)


word_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(word, tf_idf), tf_idf, fill = theme)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf", fill = "Papers", title = "The most important words in themes") +
  coord_flip()

### Well, it's time to put data into our document body.
### Add the remaining three topics:

new_full_df <- rbind(full_df
                     ,txt_from_folder("News Articles/tech/")
                     ,txt_from_folder("News Articles/entertainment/")
                     ,txt_from_folder("News Articles/sport/"))

### Earlier we built the frequency graph of Word frequency distribution, and it is unsatisfactory, ### anyway the frequency distribution is very slanted.
#### An excellent solution: you need to create your own custom list of stop words !: it is considered (as in the training) that the words that are present in all topics, and with high frequency, get there ####

words_tf_idf <- new_full_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  filter(!is.na(word)) %>%
  count(theme, word) %>%
  bind_tf_idf(word, theme, n) %>%
  group_by(theme) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  arrange(desc(tf_idf))

custom_stop_words <- words_tf_idf %>% 
  filter(idf==0) %>%
  group_by(word) %>%
  summarise(frequency = sum(n)/sum(total)) %>%
  filter(frequency > 0.00025) %>%
  arrange(desc(frequency))

print(custom_stop_words)

tidy_articles <- new_full_df %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% custom_stop_words$word) %>%
  filter(!word %in% stop_words$word) %>%
  filter(str_detect(word,"[a-z]"))

## Now a complicated performance: building a map of feature space
### We use the visual serVis library to build topics purely in vocabulary, without specifying ### headers.
#### It is necessary to make a series of transformations, first - to form a dictionary of all-all words, without repetition. 
#### These will be dimensions for our attribute space
# compute the table of terms:
term.table <- table(unlist(tidy_articles$word))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

### Now each word of all documents is encoded in accordance with the dictionary - we sign odnёrki
# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x$word, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(split.data.frame(tidy_articles, tidy_articles$doc_name), get.terms)
# Calculate some statistics
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]
print(c(D, W, N))

### Building the LDA model - Latent Dirichlet deployment.
### This is the so-called generative model - it as it suggests the reasons for the similarity of some parts of the data. Its main parameters are the body of documents in the vector space, and the number of topics that we wish.
### Use it right out of the box. It calculates the data for a while, so I created it and it’s stored in our file, so as not to recount it again
### I want to check whether lda can directly determine the source topics from the text, if I order 5 topics to her just like we originally had

K <- 5
G <- 1000
alpha <- 0.02
eta <- 0.02

## Fit the model:======================================================================================
library(lda)
set.seed(357)

if (!file.exists("../working/son.json")) {
  print("GO")
  t1 <- Sys.time()
  fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                     num.iterations = G, alpha = alpha, 
                                     eta = eta, initial = NULL, burnin = 0,
                                     compute.log.likelihood = TRUE)
  
  
  t2 <- Sys.time()
  t2 - t1  # about 24 minutes on laptop
  
  theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
  phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
  
  lda_data <- list(phi = phi,
                   theta = theta,
                   doc.length = doc.length,
                   vocab = vocab,
                   term.frequency = term.frequency)
  
  # create the JSON object to feed the visualization:
  json2 <- createJSON(phi = lda_data$phi, 
                      theta = lda_data$theta, 
                      doc.length = lda_data$doc.length, 
                      vocab = lda_data$vocab, 
                      term.frequency = lda_data$term.frequency)
  
  write(json2, "../working/son.json")
} else {
  print("ne GO")
  json2 <- read_file("../working/son.json")
}

## And publish the model
serVis(json2, out.dir = "./", open.browser = F)

system("mv index.html results.html")

### In topics such as business and politics, it is generally not clear what the words mean. Therefore, it will be very useful to break into bigrams

doc_bigrams <- new_full_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) 

head(doc_bigrams)

# Cleanse
bigrams_separated <- doc_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(str_detect(word1,"[a-z]") | str_detect(word2,"[a-z]"))

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

head(bigram_counts)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  count(theme, bigram) %>%
  bind_tf_idf(bigram, theme, n) %>%
  arrange(desc(tf_idf))

head(bigram_tf_idf)

bigram_tf_idf %>%
  group_by(theme) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(bigram, tf_idf), y = tf_idf, fill = theme)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~theme, scales = "free", ncol = 2) +
  labs(x = NULL, y = "tf-idf", title = "The most importatnt bigrams in papers") +
  coord_flip()

###It should be noted that now it looks much, much more clearly than with individual words
## Write CSV file
write.csv(new_full_df, file = 'data.csv')
write.csv(bigram_tf_idf, file = 'bigram_tf_idf.csv')
getwd()
