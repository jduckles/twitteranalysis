library(rtweet)
library(tidytext) 
library(ggplot2)
library(dplyr) 
library(lubridate) 

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

# When active 
activity_plot <- function(faves) { 
  
  
  faves %>% select(created_at) %>% mutate(wday = wday(created_at), 
                                           hour = hour(created_at), 
                                           week=week(created_at)) %>% 
    ggplot(aes(x=hour)) + 
    geom_density() #+ 
    #facet_wrap(~ week)
  
}

tweetsc <- get_favorites('jduckles', n=2000)
             
frequent_words("🍯")