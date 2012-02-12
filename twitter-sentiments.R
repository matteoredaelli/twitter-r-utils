#!/usr/bin/env Rscript
## This program is fre esoftware: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Authors: M. Redaelli
## http://www.redaelli.org/matteo/

## Code and suggestions from http://www.inside-r.org/howto/mining-twitter-airline-consumer-sentiment

library(twitteR)
library(plyr)
library(stringr)
library(ggplot2)

width=600
height=600

##setwd("/home/r/twitter-sentiments")

args <- commandArgs()
#########data di inizio e di fine delle analisi
folder <- args[6]
words <-  args[7:length(args)]
##words <- c("continental", "dunlop", "pirelli", "michelin", "goodyear", "bridgestone")

print( length(words))
if( length(words) == 0) {
  warning("Missing twitter words")
  exit(1)
}


source("twitter-util.R")
## http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html

hu.liu.pos = scan('positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('negative-words.txt', what='character', comment.char=';')


pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical')

dir.create(folder, showWarnings = FALSE)

all.scores <- data.frame()

for (word in words) {
  print(paste("Retreiving tweets for", word, "..."))
  tweets = searchTwitter(word, n=1500)
  twitter.text = laply(tweets, function(t) iconv(t$getText(), to="UTF8"))
  scores = score.sentiment(twitter.text, pos.words, neg.words, .progress='text')
  scores$name = word
  all.scores = rbind(scores, all.scores)

  ## generating wordcloud
  ##print(paste("Generating wordcloud for", word))
  ##mydata.corpus  <- vector2normalizedcorpus(unlist(twitter.text, as.vector), my.stopwords=word)
  ##WordCloud(mydata.corpus, title=word, filename=file.path(folder, paste(word, ".png", sep="")), width=width, height=height)
}

all.scores$very.pos = as.numeric( all.scores$score >= 2 )
all.scores$very.neg = as.numeric( all.scores$score <= -2 )

twitter.df = ddply(all.scores, c('name', 'name'), summarise,
  pos.count = sum( very.pos ), neg.count = sum( very.neg ) )

twitter.df$all.count = twitter.df$pos.count + twitter.df$neg.count

twitter.df$score = round( 100 * twitter.df$pos.count / twitter.df$all.count )

##hist(delta.scores$score)
##qplot(delta.scores$score)


png(file.path(folder, "score.png"), width = width, height = height, units = "px")
ggplot(data=twitter.df) + # ggplot works on data.frames, always
  geom_bar(mapping=aes(x=score, fill=name), binwidth=1) +
  facet_grid(name~.) + # make a separate plot for each airline
  theme_bw() + scale_fill_brewer() # plain display, nicer colors
dev.off()

png(file.path(folder, "scatter.png"), width = width, height = height, units = "px")
twitter.df$neg.count = 0 - twitter.df$neg.count
g = ggplot(twitter.df, aes(x=pos.count, y=neg.count)) + geom_point( aes(color=name),  size=5 ) + 
       theme_bw() + opts( legend.position=c(0.5, 0.85) )
g = g + geom_smooth(aes(group=1), se=F, method="lm")
print(g)
dev.off()

