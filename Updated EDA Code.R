library(tidyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(wordcloud)


text <- SP %>% select(Season,Character,Line)
text$ID <- seq.int(nrow(text))
text <- text[,c(4,1,2,3)]
View(text)


View(text %>% count(Character,sort = TRUE)) # gives line count by character

tidy_lines <- text %>% unnest_tokens(word, Line)
View(tidy_lines %>% count(word, sort = TRUE)) # commonly used words


## these include stop words

by_char <- group_by(tidy_lines, Character) # breaks lines into words at Character level
View(del1 <- summarise(by_char,count = n())) # number of words spoken by character
View(filter(del1,Character == "Stan")) # we can filter word count for each character using this syntax


tidy_lines2 <- text %>% unnest_tokens(word, Line) %>% anti_join(stop_words) ## important dataset


tidy_lines2 %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100)) ### wordcloud


## You can do same words spoken analysis by removing stop words
by_char2 <- group_by(tidy_lines2, Character) # breaks lines into words at Character level
View(del2 <- summarise(by_char2,count = n())) # number of words spoken by character
View(filter(del2,Character == "Stan")) # we can filter word count for each character using this syntax


View(abc <- tidy_lines2 %>% count(Character,sort = TRUE)) ## gives word count of characters after stop words have been removed
top_words <- head(abc,20) ## top 20 characters based on word count


View(res1 <- inner_join(top_words,tidy_lines2)) ## we can subset all the words for top 20 speakers
res2 <- res1 %>% select(Character,Season,word)
ggplot(res2) + aes(Character,..count..) + geom_bar() ## GGPLOT
res2 %>%ggplot(aes(Character, group = Season)) + geom_bar() ## another way to make same graph. Character vs word count


res2 %>% ggplot(aes(Season, group = Character)) + geom_bar() ## graph giving word count by season
View(res2 %>% count(Season,sort = TRUE)) ## word count by season
View(res2 %>% count(Season,Character,sort = TRUE)) ## word count of character by season


## gives a graph with word count across seasons for top 20 characters
b2 <- (res2 %>% count(Character,Season)) 
p <- ggplot(b2,aes(Season,n)) + geom_point()
p + facet_grid(.~Character) 

## use facet wrap better visualiztion


by_char_nest <- tidy_lines2 %>% 
  group_by(Character, Season) %>% 
  nest()

tidy_SP_count <- tidy_lines2 %>% count(word, sort = TRUE) ## has all words and the counts

tidy_SP_count %>%
  with(wordcloud(word, n, max.words = 100)) ### this wordcloud does not give top word for some reason


SP_net_sentiment_bing <- tidy_SP_count %>%
  inner_join(get_sentiments("bing")) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

View(SP_net_sentiment_bing)

sum(SP_net_sentiment_bing$sentiment) ## total summation


# do for different lexicons
# 10 distinct sentiments in nrc. can get top words for each sentiment
## nrc will basically help me get top words for joy, sadness etc


sentiment_index_top20_bing <- inner_join(res1,SP_net_sentiment_bing,by = "word")
View(sentiment_index_top20_bing) ### maps id and words for top 20 speakers for bing lexicon
## can get plot of sentiment vs time across characters and seasons for top 20 characters

sent_all <- (sentiment_index_top20_bing %>% group_by(Season)) ## sentiment and season in one table. bing lexicon


sentiment_season <- sentiment_index_top20_bing %>%
  group_by(Season) %>%
  summarize(mean_sum = sum(sentiment, na.rm = TRUE)) ## this gives group by of sentiments at season level


ggplot(sentiment_season,aes(Season,mean_sum)) + geom_col(alpha = .4) ## Sentiments / Season level. Season 17 quite positive, then 7, then 9



u <- ggplot(sentiment_index_top20_bing,aes(Season,sentiment,color = Character)) + geom_point(alpha = .4)
u + facet_grid(.~Character) ## this graph should give sentiment plot for characters across seasons
## but using this graph i cant say which season was less or more negative for characters


u <- ggplot(sentiment_index_top20_bing,aes(Season,sentiment,color = Character)) + geom_point(alpha = .4)
u + facet_wrap(~Character) ## better visualiztion


#afinn lexicon

afinn44 <- tidy_SP_count %>%
  inner_join(get_sentiments("afinn")) ## just this much could work for afinn.check

sentiment_index_top20_bing_afinn <- inner_join(res1,afinn44,by = "word") ## top 20 characters. afinn lexicon ranking
## what graphs can we make for afinn table?




##-----------


## add sentiments and word cloud code to this
## nested info
# fit regression models .nested data
## put word cloud and put online
