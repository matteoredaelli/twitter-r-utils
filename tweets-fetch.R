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
