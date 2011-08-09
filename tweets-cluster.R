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


