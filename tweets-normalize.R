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

##                   GNU GENERAL PUBLIC LICENSE
##                       Version 3, 29 June 2007
##
## Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
## Everyone is permitted to copy and distribute verbatim copies
## of this license document, but changing it is not allowed.
##                            Preamble
##
##  The GNU General Public License is a free, copyleft license for
##software and other kinds of works.
##
##  The licenses for most software and other practical works are designed
##to take away your freedom to share and change the works.  By contrast,
##the GNU General Public License is intended to guarantee your freedom to
##share and change all versions of a program--to make sure it remains free
##software for all its users.  We, the Free Software Foundation, use the
##GNU General Public License for most of our software; it applies also to
##any other work released this way by its authors.  You can apply it to
##your programs, too.
##
## see http://www.gnu.org/licenses/gpl-3.0.txt

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
