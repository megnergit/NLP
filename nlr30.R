# twitter
install.packages('tidytext')


library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
library(tidytext)
library(dslabs)
set.seed(101)

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
tw1 <- map(2009:2017, ~sprintf(url, .x))  %>% map_df(jsonlite::fromJSON, simplifyDataFrame = T) 
tw2 <-  tw1 %>% filter(!is_retweet & !str_detect(text, '^"')) %>% mutate(created_at = parse_date_time(created_at, orders="a b! d! H!:M!:S! z!* Y!", tz="EST"))


# library(dslabs)
# data("trump_tweets")
# tt1 <- trump_tweets
# head(tt1)
# names(tt1)
# tt1 %>% select(text) %>% head
# tt1 %>% count(source)  
# tt1 %>% extract(source, 'source', regex='Twitter for (.*)')  %>% count(source) %>% arrange(desc(n))

# map(2009:2020, ~sprintf(url, .x)) # %>%
# trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%

# library(dslabs)
# data("trump_tweets")
ct1 <- tw2
ct1 <- ct1 %>% filter(!is_retweet) %>% arrange(created_at) %>% extract(source, 'source', 'Twitter for (.*)')
ct1 <- ct1 %>% filter(source %in% c('Android', 'iPhone') & created_at >= ymd('2015-06-17') & created_at < ymd('2016-11-08'))

ds_theme_set()
ct2 <- ct1 %>% mutate(hour = hour(with_tz(created_at,'EST'))) %>% count(source, hour) %>% group_by(source)  %>% mutate(percent=n/sum(n)) # %>% ungroup
ct2 %>% ggplot(aes(hour, percent, color=source)) + geom_line() + geom_point() + scale_y_continuous(labels=percent_format()) + labs(x="Hour of day (EST)", y='% of tweets', color='')

# example <- data_frame(line = c(1, 2, 3, 4),                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
# example
# example %>% unnest_tokens(word, text)

i <- 3008
ct1$text[i]
patt <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# ct1[i,] %>% mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>% unnest_tokens(word, text, token='regex', pattern=patt) %>% select(word)
tw1 <- ct1 %>% mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>% unnest_tokens(word, text, token='regex', pattern=patt)#  %>% select(word)
tw1 <- tw1 %>% filter((!word %in% stop_words$word) & (!str_detect(word, "^\\d+$"))) %>%  mutate(word = str_replace(word, "^'", ""))
# tw1 <- tw1 %>% count(word) %>% top_n(10, n) %>% mutate(word=reorder(word, n)) %>% arrange(desc(n))

aoi <- tw1 %>% count(word, source) %>% spread(source, n, fill=0) %>% mutate(a = (Android + 0.5) / (iPhone + 0.5) /(sum(Android) - Android + 0.5) * (sum(iPhone) - iPhone + 0.5))
aoi %>% filter(Android + iPhone >=100) %>% arrange(desc(a))
aoi %>% filter(Android + iPhone >=100) %>% arrange(a)

nrc <- get_sentiments('nrc') # %>% select(word, sentiment)
afi <- get_sentiments('afinn') # %>% select(word, sentiment)

# snt1 <- snt1 %>% mutate(sentiment = repalce_na(sentiment, replace='none')) %>% arrange(desc(n))
snt1 <- tw1 %>% left_join(nrc, by='word') %>% count(source, sentiment)%>% spread(source, n)
snt1 <- snt1 %>% mutate(Android = Android / (sum(Android) - Android), iPhone = iPhone / (sum(iPhone) - iPhone), a = Android / iPhone ) %>% arrange(desc(a))

#----------------------
library(broom)
log_or <- snt1 %>%  mutate(log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>% arrange(desc(log_or))

log_or

log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 

aoi %>% inner_join(nrc) %>% filter(sentiment == "disgust" & Android + iPhone > 10) %>%  arrange(desc(a))

aoi %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(a)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



