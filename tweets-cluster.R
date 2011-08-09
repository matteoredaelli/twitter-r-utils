## Hcluster of tweets
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

ClusterCorpus <- function(mydata.corpus, title, filename="hcluster.png", k=7, sparse=0.96, border="red", width=1000, height=800) {
  ## build a term-document matrix
  mydata.dtm <- TermDocumentMatrix(mydata.corpus)
 
  ## inspect most popular words
  ## findFreqTerms(mydata.dtm, lowfreq=30)

  mydata.dtm2 <- removeSparseTerms(mydata.dtm, sparse=sparse)
  ## convert the sparse term-document matrix to a standard data frame
  mydata.df <- as.data.frame(inspect(mydata.dtm2))

  mydata.df.scale <- scale(mydata.df)
  d <- dist(mydata.df.scale, method = "euclidean") # distance matrix
  fit <- hclust(d, method="ward")
  png(file=filename, width=width, height=height)
  plot(fit, main=title) # display dendogram?
  groups <- cutree(fit, k=k) # cut tree into 5 clusters
  ## draw dendogram with red borders around the 5 clusters
  rect.hclust(fit, k=k, border=border)
  dev.off()
}


args <- commandArgs()

title  <- ifelse(is.na(args[6]), "twitter",      args[6])
sparse <- ifelse(is.na(args[7]), 0.95, as.double(args[7]))
k      <- ifelse(is.na(args[8]), 5,   as.integer(args[8]))

load("normalize.Rdata")
ClusterCorpus(mydata.corpus, k=k, sparse=sparse, title=title, border="red") 


