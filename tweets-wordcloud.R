## Wordcloud of tweets
##
## By Matteo DOT Redaelli AT gmail DOT com
## http://www.redaelli.org/matteo/
## 2011-08-01
##
## Ideas adapted from
##    http://onertipaday.blogspot.com/2011/07/word-cloud-in-r.html

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


