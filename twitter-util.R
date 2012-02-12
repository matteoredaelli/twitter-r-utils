## Create and clean a Corpus from tweets for analysis.
##
## By Matteo DOT Redaelli AT gmail DOT com
## http://www.redaelli.org/matteo/
## 2011-08-01
##
## Ideas adapted from
##    http://heuristically.wordpress.com/2011/04/08/text-data-mining-twitter-r/
## and
##   Earl F Glynn, Franklin Center for Government & Public Integrity

library(tm)
library(methods)
library(wordcloud)
library(RColorBrewer)
library(methods)

require(plyr)
require(stringr)
     
RemoveDots <- function(tweet) {
  gsub("[\\.\\,\\;]+", " ", tweet)
}

RemoveLinks <- function(tweet) {
  gsub("http:[^ $]+", "", tweet)
}

RemoveAtPeople <- function(tweet) {
  gsub("@\\w+", "", tweet)
}

CleanTweet <- function(tweet) {
  s1 <- RemoveLinks(tweet)
  s2 <- RemoveAtPeople(s1)
  s3 <- RemoveDots(s2)
  s3
}


WordCloud <- function(mydata.corpus, title, filename="wordcloud.png", width=600, height=600) {
  tdm <- TermDocumentMatrix(mydata.corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  pal2 <- brewer.pal(8,"Dark2")
  png(filename, width=width, height=height)
  wordcloud(d$word, d$freq, scale=c(8,.2),min.freq=3,
  max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
  dev.off()
}

vector2normalizedcorpus <- function(tweets, my.stopwords=c()) {
  tweets <- unlist(tweets, as.vector)
  tweets <- as.vector(sapply(tweets, CleanTweet))

  ## build a corpus
  mydata.corpus <- Corpus(VectorSource(tweets))
 
  ## make each letter lowercase
  mydata.corpus <- tm_map(mydata.corpus, tolower)
 
  ## remove punctuation
  mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
 
  ## remove generic and custom stopwords
  stopwords_1 <- c(stopwords('english'), stopwords('italian'))
  mydata.corpus <- tm_map(mydata.corpus, removeWords, stopwords_1)

  stopwords_2 <- c(stopwords('spanish'), stopwords('portuguese'))
  mydata.corpus <- tm_map(mydata.corpus, removeWords, stopwords_2)

  mydata.corpus <- tm_map(mydata.corpus, removeWords, my.stopwords)

  mydata.corpus
}

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array ("a") of scores back, so we use
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
         
        # clean up sentences with R's regex-driven global substitute, gsub():
        sentence = gsub('[[:punct:]]', '', sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence)
        # and convert to lower case:
        sentence = tolower(sentence)
 
        # split into words. str_split is in the stringr package
        word.list = str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too much
        words = unlist(word.list)
 
        # compare our words to the dictionaries of positive & negative terms
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
     
        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
 
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score = sum(pos.matches) - sum(neg.matches)
 
        return(score)
    }, pos.words, neg.words, .progress=.progress )
 
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
}

