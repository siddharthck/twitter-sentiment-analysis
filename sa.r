install.packages("twitteR", dependencies=TRUE)
install.packages("RCurl")
install.packages('bitops')
install.packages('base64enc')
install.packages('httpuv')
install.packages('tm')
install.packages('wordcloud')
install.packages("stringr")
libs = c("twitteR", "RCurl", "tm", "stringr", "wordcloud","bitops","base64enc","httpuv")
lapply(libs, require, character.only=TRUE)

options(stringsAsFactors = FALSE)

path="C:/Users/crocodile/Desktop"
filename="twitterOauth.txt"
doOAuth = function("C:/Users/crocodile/Desktop", "twitterOauth.txt"){
  
  file = paste("C:/Users/crocodile/Desktop","twitterOauth.txt",sep='/')
  oauthCreds = read.table(file,header=T)
  setup_twitter_oauth(oauthCreds$consumer_key,
                      oauthCreds$consumer_secret,
                      oauthCreds$access_token,
                      oauthCreds$access_secret) 
  
}
searchterms=c("Asia+Cup+Final+2018")
getTweets = ("Asia+Cup+Final+2018", 300){
  
  tweets_list = searchTwitter("Asia+Cup+Final+2018",lang="en",n=300,resultType="recent")
  length(tweets_list)
  class(tweets_list)
 
  
}
View(tweets_list)

tweets_text = sapply(tweets_list, function(x) x$getText())
View(tweets_text)
#corpus is collection of the data!
tweets_corpus = Corpus(VectorSource(tweets_text))

View(tweets_corpus)
tweets_corpus



#preprocessing of the corpus
tweets_corpus_clean = tm_map(tweets_corpus, removePunctuation)
tweets_corpus_clean = tm_map(tweets_corpus_clean, stripWhitespace)
tweets_corpus_clean = tm_map(tweets_corpus_clean, removeNumbers)
tweets_corpus_clean = tm_map(tweets_corpus_clean, removeWords, stopwords("english"))
tweets_corpus_clean = tm_map(tweets_corpus_clean, content_transformer(tolower))
toSpace = content_transformer(function(x, pattern) gsub(pattern,"",x))
tweets_corpus_clean = tm_map(tweets_corpus_clean, toSpace,"https*|youtu*")
tweets_corpus_clean = tm_map(tweets_corpus_clean, stemDocument)
install.packages("SnowballC")
library(SnowballC)
tweets_tdm = TermDocumentMatrix(tweets_corpus_clean)
str(tweets_tdm)

tweets_tdm = as.matrix(tweets_tdm)
str(tweets_tdm)

tdm_term_freq_sort = sort(rowSums(tweets_tdm), decreasing=TRUE)
tdm_term_freq_sort_inc = sort(rowSums(tweets_tdm), decreasing=FALSE)


tdm_term_freq_df = data.frame(word = names(tdm_term_freq_sort),
                              freq = tdm_term_freq_sort)
str(tdm_term_freq_df)
head(tdm_term_freq_df,10)
#set rownames to number
#return( list(name = searchTerms, tdm = tweets_term_freq_df))

}


wordcloud(words =tdm_term_freq_df$word,
          freq= tdm_term_freq_df$freq,
          min.freq=5,
          max.words=300,
          random.order=FALSE,
          rot.per=0.35,
          colors=brewer.pal(8,'Dark2'),
          scale=c(3,0.5))
#####################################################################
#SENTIMENT ANALYSIS
pos = scan('C:/Users/crocodile/Documents/words2/words/positive-words.txt',
           what='character',
           comment.char=';')

neg = scan('C:/Users/crocodile/Documents/words2/words/negative-words.txt',
           what='character', comment.char=';')

path="C:/Users/crocodile/Desktop"
filename="twitterOauth.txt"
doOAuth = function("C:/Users/crocodile/Desktop", "twitterOauth.txt"){
  
  file = paste("C:/Users/crocodile/Desktop","twitterOauth1.txt",sep='/')
  oauthCreds = read.table(file,header=T)
  setup_twitter_oauth(oauthCreds$consumer_key,
                      oauthCreds$consumer_secret,
                      oauthCreds$access_token,
                      oauthCreds$access_secret) 
  
}
tweets_list = searchTwitter("google+pixel+3+xl",lang="en",n=300,resultType="recent")
#length(tweets_list)
class(tweets_list)
return(tweets_list)


# clean tweets and split in to words

tweets_words = lapply(tweets_text, tweet_words)
# returns a list of 600 items - each corresponding to one tweet
# each item is character vector or words in each tweet
class(tweets_words)
#[1] "list"
str(tweets_words)
# List of 600
# $ : chr [1:13] "see" "the" "detailed" "look" ...
# $ : chr [1:17] "rt" "testervnr" "although" "the" ...
# $ : chr [1:15] "samsung" "supplying" "oled" "panel" ...
# $ : chr [1:11] "rt" "cnet" "how" "apple" ...
# $ : chr [1:10] "razer" "mocks" "apple" "macbook" ...
# $ : chr [1:13] "apple" "<U+2033>""| __truncated__ "macbook" "pro" ...
# $ : chr [1:11] "rt" "cnet" "how" "apple" ...
# and so on ...

#now get the score for each
# input list 'l' , output array 'a', use laply

require(plyr)
scores = laply(tweets_words,get_score_for_tweet,pos,neg)
class(scores)
#[1] "integer"
str(scores)
#int [1:600] 0 0 0 0 -1 0 0 -1 1 0 ...

# create score frequency table

analysis = data.frame(score=scores, tweet=tweets_text)
class(analysis)

table(analysis$score)
# -2    -1     0     1     2    3
# 88  139 279 78   14  2
###########################################
getTweets_text = function(tweets_list){
  
  tweets_text = sapply(tweets_list, function(x) x$getText())
  #str(tweets_text)
  #class(tweets_text)
  return (tweets_text)
  
}
get_tweet_words = function(tweet_text){
  
  tweet_text_clean = gsub('[[:punct:]]','', tweets_text)
  tweet_text_clean = gsub('[[:cntrl:]]','', tweet_text_clean)
  tweet_text_clean = gsub('\\d+','', tweet_text_clean)
  tweet_text_clean = tolower(tweet_text_clean)
  tweet_words_list = str_split(tweet_text_clean, '\\s+')
  
  # sometimes a list() is one level of hierarchy too much
  tweet_words = unlist(tweet_words_list)
  return(tweet_words)
  
}

get_score_for_tweet = function(tweets_words, pos, neg){
  
  pos.matches = match(tweet_words, pos)
  neg.matches = match(tweet_words, neg)
  #match() returns the position of the matched term or NA
  #we just want not na, so for all values with position it will return true
  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  score = sum(pos.matches)-sum(neg.matches)
  return(score)
  
}
sum(pos.matches)
View(neg.matches)
str(tweets_text)
str(tweet_words)
help(lapply)
tweet_words = lapply(tweets_text, tweet_words)
tweets_words = lapply(tweets_text, get_tweet_words)

tweets_text = getTweets_text(tweets_list)

analysis = data.frame(score=scores, tweet=tweets_text)
class(analysis)

table(analysis$score)
scores = laply(tweet_words,get_score_for_tweet,pos,neg)
class(scores)
str(scores)
attach(analysis)
summary(analysis$score)
bins = seq(min(score), max(score), 1)
hist(analysis$score, breaks=bins,
         main='sentiment analysis of tweets',
         ylab='frequency of scores',
         xlab='scores',
         col='grey')

##################################################################################################### pahilyapasun
getTweets = function(searchTerms, numberOfTweets){
  
  tweets_list = searchTwitter("google+pixel+3xl",lang="en",n=300,resultType="recent")
  #length(tweets_list)
  class(tweets_list)
  return(tweets_list)
  
}

getTweets_text = function(tweets_list){
  
  tweets_text = sapply(tweets_list, function(x) x$getText())
  #str(tweets_text)
  #class(tweets_text)
  return (tweets_text)
  
}
