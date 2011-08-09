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

## Wordcloud of tweets
##
## By Matteo DOT Redaelli AT gmail DOT com
## http://www.redaelli.org/matteo/
## 2011-08-01
##
## Ideas adapted from
##    http://onertipaday.blogspot.com/2011/07/word-cloud-in-r.html

library(tm)
library(methods)
library(wordcloud)
library(RColorBrewer)


WordCloud <- function(mydata.corpus, title, filename="wordcloud.png", width=1000, height=800) {
  tdm <- TermDocumentMatrix(mydata.corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  pal2 <- brewer.pal(8,"Dark2")
  png(filename, width=600,height=600)
  wordcloud(d$word, d$freq, scale=c(8,.2),min.freq=3,
  max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
  dev.off()
}

args <- commandArgs()

title  <- ifelse(is.na(args[6]), "twitter", args[6])

load("normalize.Rdata")
WordCloud(mydata.corpus, title=title) 


