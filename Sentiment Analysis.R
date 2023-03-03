library(tidyverse)
library(ggplot2)
library(tidytext)
library(textdata)
library(rvest)


##Web scraping 
##Chicago PD Sentiments
url1 <- "https://www.chicagotribune.com/news/breaking/ct-cpd-independent-monitor-report-5-consent-decree-20220417-eedyh5mctnbjlmkfxw7swv7pi4-story.html"

article <- read_html(url1)

paragraphs <- html_nodes(article, "p")
text_list <- html_text(paragraphs)
text_cpd <- paste(text_list, collapse = "")
text_cpd <- tibble(text = text_cpd)
text_wt_cpd <- unnest_tokens(text_cpd, word_tokens,  text, token = "words")

text_wt_cpd <- anti_join(text_wt_cpd, stop_words, by = c("word_tokens" = "word"))
sentiment_nrc <- get_sentiments("nrc") %>%
  rename(nrc = sentiment)

text_wtns_cpd <- text_wt_cpd %>%
  left_join(sentiment_nrc, by = c("word_tokens" = "word"))

ggplot(data = filter(text_wtns_cpd, !is.na(nrc))) +
  geom_histogram(aes(nrc), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Chicago Police Dep. Sentiments") +
  labs(y = "Word Relation Count", x = "Sentiment", subtitle = "Article 1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

###article 2
url2 <- "https://www.chicagotribune.com/news/breaking/ct-chicago-police-use-of-force-study-20221003-irihtyyoabaqfolkdaummtybou-story.html"

article2 <- read_html(url2)

paragraphs2 <- html_nodes(article2, "p")
text_list2 <- html_text(paragraphs2)
text_cpd2 <- paste(text_list2, collapse = "")
text_cpd2 <- tibble(text = text_cpd2)
text_wt_cpd2 <- unnest_tokens(text_cpd2, word_tokens,  text, token = "words")

text_wt_cpd2 <- anti_join(text_wt_cpd2, stop_words, by = c("word_tokens" = "word"))

text_wtns_cpd2 <- text_wt_cpd2 %>%
  left_join(sentiment_nrc, by = c("word_tokens" = "word"))

ggplot(data = filter(text_wtns_cpd2, !is.na(nrc))) +
  geom_histogram(aes(nrc), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Chicago Police Dep. Sentiments") +
  labs(y = "Word Relation Count", x = "Sentiment", subtitle = "Article 2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

##article 3
url3 <- "https://www.wbez.org/stories/new-federal-judge-takes-over-cpd-court-ordered-reforms/33774cad-850f-445e-b5ff-71af5e89ae63"
article3 <- read_html(url3)

paragraphs3 <- html_nodes(article3, "p")
text_list3 <- html_text(paragraphs3)
text_cpd3 <- paste(text_list3, collapse = "")
text_cpd3 <- tibble(text = text_cpd3)
text_wt_cpd3 <- unnest_tokens(text_cpd3, word_tokens,  text, token = "words")

text_wt_cpd3 <- anti_join(text_wt_cpd3, stop_words, by = c("word_tokens" = "word"))

text_wtns_cpd3 <- text_wt_cpd3 %>%
  left_join(sentiment_nrc, by = c("word_tokens" = "word"))

ggplot(data = filter(text_wtns_cpd3, !is.na(nrc))) +
  geom_histogram(aes(nrc), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Chicago Police Dep. Sentiments") +
  labs(y = "Word Relation Count", x = "Sentiment", subtitle = "Article 3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

##cbs Article 4
url4 <- "https://www.cbsnews.com/chicago/news/cpd-makes-change-after-scathing-report-on-cancellation-of-officers-off-days/"
article4 <- read_html(url4)

paragraphs4 <- html_nodes(article4, "p")
text_list4 <- html_text(paragraphs4)
text_cpd4 <- paste(text_list4, collapse = "")
text_cpd4 <- tibble(text = text_cpd4)
text_wt_cpd4 <- unnest_tokens(text_cpd4, word_tokens,  text, token = "words")

text_wt_cpd4 <- anti_join(text_wt_cpd4, stop_words, by = c("word_tokens" = "word"))

text_wtns_cpd4 <- text_wt_cpd4 %>%
  left_join(sentiment_nrc, by = c("word_tokens" = "word"))

ggplot(data = filter(text_wtns_cpd4, !is.na(nrc))) +
  geom_histogram(aes(nrc), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Chicago Police Dep. Sentiments") +
  labs(y = "Word Relation Count", x = "Sentiment", subtitle = "Article 4") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))
