library(rvest)
library(textclean) 
library(textstem)
library(hunspell) 
library(stopwords)
library(tm)
library(topicmodels)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

url <- "https://www.thedailystar.net"
extention <- "/news/world/"
webpage <- read_html(paste(c(url,extention),collapse = ""))
web_data <- webpage %>% html_nodes(".card-content.columns")

raw_data <- data.frame(Title = character(), Anchor_tag = character(), Raw_article = character(), Cleaned_article = character())
cleaned_data <- data.frame(cleaned_text = character(), stringsAsFactors = FALSE)

for (title in web_data){
  title_text <- title %>% html_node("a") %>% html_text(trim = TRUE)
  print(title_text)
  anchor_tag <- title %>% html_node("a")
  href_value <- html_attr(anchor_tag, "href")
  tmp <- paste(c(url,href_value),collapse = "")
  
  extwebpage <- read_html(tmp)
  description <- extwebpage %>% html_nodes("article p") %>% html_text() %>% paste(collapse = " ")
  print(description)
  
  text <- tolower(description)                               
  text <- replace_contraction(text)                      
  text <- replace_emoji(text)
  text <- replace_emoticon(text)                        
  text <- gsub("[^a-z\\s]", " ", text)                     
  text <- gsub("\\s+", " ", text)
  
  tokens <- unlist(strsplit(text, " "))
  tokens <- tokens[!(tokens %in% c(stopwords("en"), ""))]
  tokens <- tokens[nchar(tokens) > 1] 
  
  lemmatized <- lemmatize_words(tokens)
  
  correct <- hunspell_check(lemmatized)
  tokens <- tokens[unlist(correct)]
  
  
  
  cleaned_text <- paste(tokens, collapse = " ")
  cleaned_data <- rbind(cleaned_data, data.frame(cleaned_text = cleaned_text, stringsAsFactors = FALSE))
  print(head(cleaned_data))
  
  raw_data <-rbind(raw_data, data.frame(
    Title = title_text,
    Anchor_tag = href_value,
    Raw_article = description
  ))
}

write.csv(raw_data, file = "D:/10th semester/Data Science/final_project/Raw_Article.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(cleaned_data, file = "D:/10th semester/Data Science/final_project/Cleaned_Aricle.csv", row.names = FALSE, fileEncoding = "UTF-8")



corpus_data <- read.csv("D:/10th semester/Data Science/final_project/Cleaned_Aricle.csv", stringsAsFactors = FALSE)
texts <- corpus_data$cleaned_text

corpus_all <- VCorpus(VectorSource(texts))
dtm_all <- DocumentTermMatrix(corpus_all, control = list(wordLengths = c(3, Inf)))
dtm_all_sparse <- removeSparseTerms(dtm_all, 0.99)  # Remove very sparse terms

k <- 5
set.seed(1234)
lda_model <- LDA(dtm_all_sparse, k = k, control = list(seed = 1234))

topics <- tidy(lda_model, matrix = "beta")
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
print(top_terms)

top_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(5, beta) %>% 
  arrange(topic, -beta) %>%
  summarise(topic_label = paste(term, collapse = "_"))  # join top words with underscore

gamma_df <- tidy(lda_model, matrix = "gamma")
gamma_df$document <- as.integer(gamma_df$document)

dominant_topics <- gamma_df %>%
  group_by(document) %>%
  slice_max(order_by = gamma, n = 1, with_ties = FALSE) %>%
  ungroup()

dominant_topics <- dominant_topics %>%
  left_join(top_terms, by = "topic")

gamma_wide <- gamma_df %>%
  pivot_wider(names_from = topic, values_from = gamma, names_prefix = "Topic_") %>%
  arrange(document)

gamma_wide <- gamma_wide %>%
  left_join(
    dominant_topics %>% select(document, dominant_topic_label = topic_label),
    by = c("document")
  )
print(gamma_wide)

ggplot(gamma_df, aes(x = factor(topic), y = factor(document), fill = gamma)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Topic Proportions (Gamma) per Paragraph",
       x = "Topic",
       y = "Paragraph (Document)",
       fill = "Proportion") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

for (i in seq_along(texts)) {
  cat("Generating wordcloud for paragraph", i, "\n")
  
  corpus_i <- VCorpus(VectorSource(texts[i]))
  dtm_i <- DocumentTermMatrix(corpus_i, control = list(wordLengths = c(3, Inf)))
  
  word_freq_i <- colSums(as.matrix(dtm_i))
  word_freq_i <- sort(word_freq_i, decreasing = TRUE)
  
  wordcloud(words = names(word_freq_i), freq = word_freq_i, min.freq = 2, max.words = 30, random.order = FALSE, rot.per = 0.3, colors = brewer.pal(8, "Dark2"))
}

