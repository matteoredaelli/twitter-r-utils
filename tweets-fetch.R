## Capture Twitter search for analysis.
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

library(XML)   # htmlTreeParse

args <- commandArgs()

FetchTweets <- function(q) {
  ## initialize a storage variable for Twitter tweets
  mydata.vectors <- character(0)
 
  ## paginate to get more tweets
  for (page in c(1:15)) {
    ## search parameter
    twitter_q <- URLencode(q)
    ## construct a URL
    twitter_url = paste('http://search.twitter.com/search.atom?q=',twitter_q,'&rpp=100&result_type=recent&page=', page, sep='')
    ## fetch remote URL and parse
    mydata.xml <- xmlParseDoc(twitter_url, asText=F)
    ## extract the titles
    mydata.vector <- xpathSApply(mydata.xml, '//s:entry/s:title', xmlValue, namespaces =c('s'='http://www.w3.org/2005/Atom'))
    ## aggregate new tweets with previous tweets
    mydata.vectors <- c(mydata.vector, mydata.vectors)
  }
  mydata.vectors
}


if (is.na(args[6])) {
  print("Missing search string. Bye")
  exit(1)
}

q <- args[6]

tweets <- unlist(FetchTweets(q), as.vector)
save(tweets, file="fetch.Rdata")
