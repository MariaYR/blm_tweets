#ASW full analysis 

## remove any previously saved objects in your R environment
rm(list=ls())

library(streamR)
library (tidyverse)
library(stm)

setwd ("~/Dropbox/BLMTweets/paper_analysis/blm_tweets_repo")
load("~/Dropbox/BLMTweets/paper_analysis/blm_tweets_repo/stm_analysis.RData")

##############
#STM 
##############

####################
#start with #UniteTheRight
###################

unite_right_tweets <- parseTweets("UniteTheRight.json", simplify = TRUE)
names(unite_right_tweets)

#remove emoji characters
unite_right_tweets$text <- sapply(unite_right_tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#remove na's  
na_count <-sapply(unite_right_tweets, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#remove columns with na's
unite_analysis <- subset(unite_right_tweets, select = -c(in_reply_to_screen_name,in_reply_to_status_id_str, 
                                                         in_reply_to_user_id_str, user_url, time_zone))
#now removes rows with na's, should be  much less
unite_analysis <- na.omit(unite_analysis)
names(unite_analysis)

#begin STM 
unite_processed <- textProcessor(unite_analysis$text, metadata = unite_analysis)

#prepare
unite_out <- prepDocuments(unite_processed$documents, unite_processed$vocab, unite_processed$meta)
#Removing 9233 of 15173 terms (9233 of 287258 tokens) due to frequency 
#Removing 173 Documents with No Words 
#Your corpus now has 29398 documents, 5940 terms and 278025 tokens.

plotRemoved(unite_processed$documents, lower.thresh = seq(1,10, by =5))

#unite out 
unite_docs<- unite_out$documents
unite_vocab<- unite_out$vocab
unite_meta <- unite_out$meta


#inspect to make sure preprocessing went ok 
#unite
head(unite_docs) #this tells you how many words are in what position 
head(unite_vocab)
head(unite_meta)
names(unite_meta)

#Estimate 
library (Rtsne)
library(rsvd)
library(geometry)

unite_fit <- stm(documents = unite_out$documents, vocab = unite_out$vocab, K=0, 
                 data = unite_out$meta, init.type = "Spectral")
#algorithm = 80

#Evaluate
#searchk runs selectmodel for researcher selected # of k and computes diagnostic properties for
#the returned model. 

#unite
unite_storage <- searchK(unite_out$documents, unite_out$vocab, K = c(10,20,30,40,50,60,70,80,90,100), data = meta) 
plot(unite_storage)

#lowest held out likelihood = 100 topics
#lowest residual = 80 topics
#highest semantic coherence = 60 topics
#highest lower bound = 100 topics

#Understand 
#STM lets us do a couple of things: 
#1: display words associated with topics 

#labelTopics 
labelTopics (unite_fit)
labelTopics (unite_fit2)

#convert to dataframe and export to csv 
unite_k90 <- make.dt(unite_fit)
write.csv(unite_k90, "unite_k90.csv")

#2. Estimate the relationship between metadata and topics/topical content 
estimateEffect()

#3. Calculate topic correlations 
unite_corr <- topicCorr(unite_fit, method = "simple", cutoff = 0.01, verbose = TRUE)

#must install igraph before graphing correlations 
library(igraph)

#enlarge margins to get most out of image 
par(mar=c(0,0,1,0))

plot (unite_corr, cex = .05, main = "#UniteTheRight Topic Correlations") 

#Visualize 
#reset par mfrow
par(mfrow=c(1,1))    


plot.STM(unite_fit,type="summary", xlim=c(0, .3))
#just top 20 
plot.STM(unite_fit,type="summary", xlim=c(0, .3), ylim=c(60,80))

#show the topics that proportionally make up the most of the corpus
#cville first 
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))
unite_thoughts44 <- findThoughts(unite_fit, texts=unite_analysis$text, topics=44, n=2)
unite_thoughts45 <- findThoughts(unite_fit, texts=unite_right_tweets$text, topics=45, n=2)
unite_thoughts40 <- findThoughts(unite_fit, texts=unite_right_tweets$text, topics=40, n=2)
unite_thoughts26 <- findThoughts(unite_fit, texts=unite_right_tweets$text, topics=26, n=2)
unite_thoughts33 <- findThoughts(unite_fit, texts=unite_right_tweets$text, topics=33, n=2)
plotQuote(unite_thoughts44$docs[[1]], main = "#UniteTheRight Topic 19")
plotQuote(unite_thoughts45$docs[[1]], main = "#UniteTheRight Topic 31")
plotQuote(unite_thoughts40$docs[[1]], main = "#UniteTheRight Topic 57")
plotQuote(unite_thoughts26$docs[[1]], main = "#UniteTheRight Topic 40")
plotQuote(unite_thoughts33$docs[[1]], main = "#UniteTheRight Topic 9")

#plot word clouds with sample tweets
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))

plotQuote(model3_thoughts66$docs[[1]], main = "Topic 66")
cloud(cville_fit2, topic = 66, scale = c(2, .25))

plotQuote(model3_thoughts3$docs[[1]], main = "Topic 3")
cloud(cville_fit2, topic = 3, scale = c(2, .25))

plotQuote(model3_thoughts9$docs[[1]], main = "Topic 9")
cloud(cville_fit2, topic = 9, scale = c(2, .25))

plotQuote(model3_thoughts62$docs[[1]], main = "Topic 62")
cloud(cville_fit2, topic = 62, scale = c(2, .25))

plotQuote(model3_thoughts20$docs[[1]], main = "Topic 20")
cloud(cville_fit2, topic = 20, scale = c(2, .25))

plotQuote(model3_thoughts55$docs[[1]], main = "Topic 55")
cloud(cville_fit2, topic = 55, scale = c(2, .25))

#####################
#then #charlottesville 
#####################

cville_tweets <- parseTweets("Charlottesville.json", simplify = TRUE)
names(cville_tweets)

#remove emoji characters
cville_tweets$text <- sapply(cville_tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

head(cville_tweets)
head(unite_right_tweets)

#begin stm
cville_processed <- textProcessor(cville_tweets$text, metadata = cville_tweets)

#Prepare
cville_out <- prepDocuments(cville_processed$documents, cville_processed$vocab, cville_processed$meta)
#Removing 9576 of 16357 terms (9576 of 237588 tokens) due to frequency 
#Removing 157 Documents with No Words 
#Your corpus now has 23792 documents, 6781 terms and 228012 tokens.

plotRemoved(cville_processed$documents, lower.thresh = seq(1,10, by =5))

#this plot indicates the number of docs removed at a cetain threshold. Our threshold is 1

#cville out
cville_docs<- cville_out$documents
cville_vocab<- cville_out$vocab
cville_meta <- cville_out$meta

#inspect to make sure preprocessing went ok 
#cville
head(cville_docs) #this tells you how many words are in what position 
head(cville_vocab)
head(cville_meta)
names(cville_meta)

#Estimate 
library (Rtsne)
library(rsvd)
library(geometry)

#no metadata and specifying k
blm_Fit<- stm(documents = out$documents, vocab = out$vocab, K= 10, 
              data = out$meta, init.type = "Spectral")
#this is all blm tweets together, seperating into cville and unite right only 

cville_fit <- stm(documents = cville_out$documents, vocab = cville_out$vocab, K=0, 
                  data = cville_out$meta, init.type = "Spectral")
#algorithm = 81 topics 

#note: spectral initialization solves the issue of the estimation answers being sensitive to 
# the values of the parameters (the distribution over words for a particular topic)
#it is an initialization absed on the method of moments, which is 
#deterministic and globally consistent. 

#Evaluate
#searchk runs selectmodel for researcher selected # of k and computes diagnostic properties for
#the returned model. 

#cville
cville_storage <- searchK(cville_out$documents, cville_out$vocab, K = c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150), data = meta) 
plot(cville_storage)

#lowest held out likelihood = XX topics
#lowest residual = XX topics
#highest semantic coherence = XX topics
#highest lower bound = XX topics

#final model fit based on best parameters 
#algorithm estimate seems best given diagnostics 

#Understand 
#STM lets us do a couple of things: 
#1: display words associated with topics 

#labelTopics 
labelTopics (cville_fit)

#convert to dataframe and export to csv 
cville_k81 <- make.dt(cville_fit)
write.csv(cville_k81, "cville_k81.csv")

#2. Estimate the relationship between metadata and topics/topical content 
estimateEffect()

#3. Calculate topic correlations 
cville_corr <- topicCorr(cville_fit, method = "simple", cutoff = 0.01, verbose = TRUE)

#must install igraph before graphing correlations 
library(igraph)

#enlarge margins to get most out of image 
par(mar=c(0,0,1,0))

plot (cville_corr, cex = .05, 
      main = "#Charlottesville Topic Correlations")

#Visualize 
#reset par mfrow
par(mfrow=c(1,1))

plot.STM(cville_fit,type="summary", xlim=c(0, .3))
#just top 20 
plot.STM(cville_fit,type="summary", xlim=c(0, .3), ylim=c(61,81))

#show the topics that proportionally make up the most of the corpus
#cville first 
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))
cville_thoughts19 <- findThoughts(unite_fit, texts=cville_tweets$text, topics=19, n=2)
cville_thoughts31 <- findThoughts(cville_fit, texts=cville_k81$text, topics=31, n=2)
cville_thoughts57 <- findThoughts(cville_fit, texts=cville_k81$text, topics=57, n=2)
cville_thoughts40 <- findThoughts(cville_fit, texts=cville_k81$text, topics=40, n=2)
cville_thoughts9 <- findThoughts(cville_fit, texts=cville_k81$text, topics=9, n=2)
plotQuote(cville_thoughts19$docs[[1]], main = "#Charlottesville Topic 19")
plotQuote(cville_thoughts31$docs[[1]], main = "Charlottesville Topic 31")
plotQuote(cville_thoughts57$docs[[1]], main = "Charlottesville Topic 57")
plotQuote(cville_thoughts40$docs[[1]], main = "Charlottesville Topic 40")
plotQuote(cville_thoughts9$docs[[1]], main = "Charlottesville Topic 9")

#plot word clouds with sample tweets
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))

plotQuote(model3_thoughts66$docs[[1]], main = "Topic 66")
cloud(cville_fit2, topic = 66, scale = c(2, .25))

plotQuote(model3_thoughts3$docs[[1]], main = "Topic 3")
cloud(cville_fit2, topic = 3, scale = c(2, .25))

plotQuote(model3_thoughts9$docs[[1]], main = "Topic 9")
cloud(cville_fit2, topic = 9, scale = c(2, .25))

plotQuote(model3_thoughts62$docs[[1]], main = "Topic 62")
cloud(cville_fit2, topic = 62, scale = c(2, .25))

plotQuote(model3_thoughts20$docs[[1]], main = "Topic 20")
cloud(cville_fit2, topic = 20, scale = c(2, .25))

plotQuote(model3_thoughts55$docs[[1]], main = "Topic 55")
cloud(cville_fit2, topic = 55, scale = c(2, .25))
