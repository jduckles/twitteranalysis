library(rtweet)
library(tidytext) 
library(ggplot2)
library(dplyr) 

frequent_words <- function(query, n=500, more_than=5, lang=NULL) { 

  tweets <- search_tweets(query, n=500, lang=lang) 
  
  other_stops = c('is', 'https', 't.co') # removing https and t.co from tweets
  stops <- rbind(stop_words,data.frame(word=other_stops,lexicon="custom")) %>% transmute(text=word)

  text_tokens <- tweets %>% select(text) %>% unnest_tokens(text, text) %>% anti_join(stops) 

  frequent_words <- text_tokens %>%
    count(text, sort = TRUE) %>%         
    filter(n > 5) %>%                    
    mutate(text = reorder(text, n)) 
  frequent_words
}

frequent_words("🍯")