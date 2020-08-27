library(rtweet)
library(ggplot2)
library(dplyr) 


honey <- search_tweets("🍯")
manuka_tweets <- search_tweets("bioactive", n=500) 

frequent_words <- function(query, n=500, more_than=5, lang=NULL) { 

  tweets <- search_tweets(query, n=500, lang=lang) 
  
  other_stops = c('is', 'https', 't.co') # removing al from et al.
  stops <- rbind(stop_words,data.frame(word=other_stops,lexicon="custom")) %>% transmute(text=word)

  text_tokens <- tweets %>% select(text) %>% unnest_tokens(text, text) %>% anti_join(stops) 

  frequent_words <- text_tokens %>%
    count(text, sort = TRUE) %>%         
    filter(n > 5) %>%                    
    mutate(text = reorder(text, n)) 
  frequent_words
}

