# twitter
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(scales)
library(tidytext)
library(rmarkdown) 

set.seed(101, sample.kind="Rounding")
#-------------------------------------------
# Get raw data from trumptwitterarchive.com

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
#-------------------------------------------
# Use data from 2015 to 2016

tw1 <- map(2015:2016, ~sprintf(url, .x))  %>% map_df(jsonlite::fromJSON, simplifyDataFrame = T) 
#-------------------------------------------
# Remove retweets
tw2 <-tw1 %>% 
  filter(!is_retweet & !str_detect(text, '^"')) %>% 
  mutate(created_at = parse_date_time(created_at, orders="a b! d! H!:M!:S! z!* Y!", tz="EST")) %>% 
  select(source, id_str, text, created_at)

#-------------------------------------------
# Find tweets that are sent only from Android or iPhone. 
# Other devices/platform are ignored. 
# 
tw2 <- tw2 %>% arrange(created_at) %>% 
  extract(source, 'source', 'Twitter for (.*)') 

#-------------------------------------------
# Include these tweets only that are posted during the election campaign. 

tw2 <- tw2 %>%  filter(source %in% c('Android', 'iPhone') & created_at >= ymd('2015-06-17') & created_at < ymd('2016-11-08')) 
#-------------------------------------------
# Group data by hour of a day to see if there is any trend in which devices for posting at each hour. 
# Calculate how much fractions of whole tweets are posted that hour of a day.

tw3 <- tw2 %>%
  mutate(hour = hour(with_tz(created_at,'EST'))) %>%
  count(source, hour)  %>% 
  group_by(source)  %>%
  mutate(percent=n/sum(n)) # %>% ungroup() %>%

# Plot it
tw3 %>% ggplot(aes(x=hour, y=percent, fill=source) )+ 
      geom_bar(aes(fill=source), stat="identity",position="identity") + 
      scale_fill_manual(values = alpha(c("coral1","cyan4"), 0.75)) +
      xlab('Hour of day (EST)')+ 
      scale_y_continuous(labels=percent_format()) 

# There is an obvious peak between 9 am and 5 pm posted from Android. 
# Apparently there are two teams -- 
#     1) who use Androlid during morning
#     2) who use iPhone in the afternoon 
# 1) is likely Trump himself, and 2) is staff
#===================================
# Now we will investigate if there is any difference in sentiment 
# in the tweets posted by two teams above. 

# Remove twitter web site URL
tw4 <- tw2 %>% mutate(text=str_replace_all(text, 'http://t.co/[A-Za-z\\d]+|&amp;', '')) %>% 
  unnest_tokens(word, text, token='tweets')  %>%
  filter(!word %in% stop_words$word & !str_detect(word, '^\\d+$')) %>%
  mutate(word = str_replace(word, "^'", "")) 

#-----------------------------------
# These are kind of words that most frequenly appear in tweets
tw4 %>% count(word) %>% arrange(desc(n)) %>% head

#-----------------------------------
# Now group by device, Android or iPhone
# remove those words where the samples are less than 32.
aoi <- tw4 %>% count(word, source) %>% 
  spread(source, n, fill=0)  %>%
  filter(Android + iPhone >= 32) %>% arrange(desc(Android))

head(aoi)
#-----------------------------------
# Selection of dictionaries that sentiment labels are set.
# We will use nrc sentiment. 
nrc <- get_sentiments('nrc') # %>% select(word, sentiment)
afi <- get_sentiments('afinn') # %>% select(word, sentiment)
bing <- get_sentiments("bing")
loug <- get_sentiments("loughran") %>% count(sentiment)

#-----------------------------------
# Assign sentiment 
snt1 <- tw4 %>% left_join(nrc, by='word') %>% 
  count(source, sentiment)%>% 
  spread(source, n) %>% 
  filter(!is.na(sentiment)) %>%
  mutate(Android = Android / sum(Android) ) %>% 
  mutate(iPhone = iPhone / sum(iPhone) )  %>% 
  mutate(sentiment = fct_reorder(sentiment, desc(Android - iPhone))) 

snt1
#----------------------
# Plot sentiment in the order Android is more prone to
snt1 %>% mutate(sentiment = fct_reorder(sentiment, desc(Android - iPhone))) %>%
    gather("source", "f", 2:3) %>%
    ggplot(aes(sentiment, f, fill=source)) + 
    geom_bar(aes(fill=source), stat='identity', position='identity') +
    scale_fill_manual(values=alpha(c('coral1', 'cyan4'), .7)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

#----------------------
# Conclusion: sentiment that were sent from Android during morning hours are more negative
# than those from iPhone in the afternoon. The sentiments that are more often seen in the former are
# "negative", "anger", "disgust", "sadness", and "fear". 
