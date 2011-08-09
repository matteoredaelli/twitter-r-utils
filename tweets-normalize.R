##    This program is free software: you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation, either version 3 of the License, or
##    any later version.
##
##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.
##
##    You should have received a copy of the GNU General Public License
##    along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

args <- commandArgs()
q <- ifelse(is.na(args[6]), "twitter",args[6]) 

load("fetch.Rdata")

tweets <- as.vector(sapply(tweets, CleanTweet))

## build a corpus
mydata.corpus <- Corpus(VectorSource(tweets))
 
## make each letter lowercase
mydata.corpus <- tm_map(mydata.corpus, tolower)
 
## remove punctuation
mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
 
## remove generic and custom stopwords


stopwords_1 <- c(stopwords('english'),
                 stopwords('italian'),
                 q
                )
mydata.corpus <- tm_map(mydata.corpus, removeWords, stopwords_1)

stopwords_2 <- c(stopwords('spanish'),
                 stopwords('portuguese')
                )
mydata.corpus <- tm_map(mydata.corpus, removeWords, stopwords_2)

save(mydata.corpus, file="normalize.Rdata")
