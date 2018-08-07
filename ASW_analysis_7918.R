#ASW full analysis 

## remove any previously saved objects in your R environment
rm(list=ls())

setwd ("~/Dropbox/BLMTweets/paper_analysis/blm_tweets_repo")
load("~/Dropbox/BLMTweets/paper_analysis/blm_tweets_repo/stm_analysis_7.11.18.RData")
library(streamR)
library (tidyverse)
library(stm)
library (Rtsne)
library(rsvd)
library(geometry)
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
                                                         in_reply_to_user_id_str, user_url))
names(unite_analysis)

#clean tweets to remove urls and amp
unite_analysis$stripped_text <- gsub("http.*","",  unite_analysis$text)
unite_analysis$stripped_text <- gsub("https.*","", unite_analysis$stripped_text)
unite_analysis$stripped_text <- gsub("amp.*","", unite_analysis$stripped_text)
unite_analysis$stripped_text <- gsub("^[[:space:]]*","",unite_analysis$stripped_text) ## Remove leading whitespaces
unite_analysis$stripped_text <- gsub("[[:space:]]*$","",unite_analysis$stripped_text) ## Remove trailing whitespaces
unite_analysis$stripped_text <- gsub(' +',' ',unite_analysis$stripped_text) ## Remove extra whitespaces
unite_analysis$stripped_text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", unite_analysis$stripped_text)#remove retweets
#now removes rows with na's, should be  much less
#unite_analysis_test <- na.omit(unite_right_tweets)
#removing na's results in only 232 observations. lets keep them and remove retweets instead 

#begin STM 
unite_processed <- textProcessor(unite_analysis$stripped_text, metadata = unite_analysis)

#prepare
unite_out <- prepDocuments(unite_processed$documents, unite_processed$vocab, unite_processed$meta)
#Removing 5901 of 12473 terms (5901 of 641344 tokens) due to frequency  
#Removing 201 Documents with No Words 
#Your corpus now has 77480 documents, 6572 terms and 635443 tokens.

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
#picked up on line 142

#Estimate 
unite_fit <- stm(documents = unite_out$documents, vocab = unite_out$vocab, K=0, 
                 data = unite_out$meta, init.type = "Spectral")


#k=0 results in 74 topics 


#what if we add time?
names(unite_analysis)
head(unite_analysis$created_at)

#convert time to something usable in r 
#below is one at a time? 
#Sys.setlocale("LC_TIME", "English")
#as.POSIXct("Tue Jun 07 23:27:12 +0000 2016", format="%a %b %d %H:%M:%S +0000 %Y", tz="GMT")

#create vector of dated tweets converting created_at to POSIX, the date/time format R understands 
#dated_tweets <- as.POSIXct(unite_analysis$created_at, format = "%a %b %d %H:%M:%S +0000 %Y")
#hist(dated_tweets, breaks ="min", freq = TRUE, 
#     ylab = "Minutes", xlab = "Unite The Right Tweets")
#now execute the conversion with the required dataframe
unite_analysis$created_at <- as.POSIXct(unite_analysis$created_at, format = "%a %b %d %H:%M:%S +0000 %Y")
unite_meta$created_at <- as.POSIXct(unite_meta$created_at, format = "%a %b %d %H:%M:%S +0000 %Y")

#unite_out$meta$created_at - as.POSIXct(unite_out$meta$created_at, format = "%a %b %d %H:%M:%S +0000 %Y")
#Error in `-.POSIXt`(unite_out$meta$created_at, as.POSIXct(unite_out$meta$created_at,  : 
#                                                            can only subtract from "POSIXt" objects

#just a check 
hist(unite_meta$created_at, breaks ="min", freq = TRUE, 
     ylab = "Number of Tweets", xlab = "Time", 
     main = "Frequency of #UniteTheRight Tweets")

#unite_fit2 will use k=74 from above, but addes whether user is verified and time
#as covariates 
head(unite_meta$created_at)
head(unite_meta$verified)
#convert verified to false = 0, true = 1
unite_meta$verified [unite_meta$verified == "TRUE"] <- 1
unite_meta$verified [unite_meta$verified == "FALSE"] <- 0
unite_meta$verified <- as.integer(unite_meta$verified)
head(unite_meta$verified)
table(unite_meta$verified)

#git
#git config --global user.email "you@example.com"
#git config --global user.name "Your Name"

#cool, run new stm with time as prevalence factor 
#prevalence = covariates affecting frequency with which topics are discussed 
#here time. Prevalance also includes a topical content covariate, 
#here whether the account is verified
#unite_fit2 will use k=74 from above, but addes whether user is verified and time
#as covariates 
unite_fit2 <- stm(documents = unite_out$documents, vocab = unite_out$vocab, K=74, 
                  prevalence =~unite_meta$verified + unite_meta$created_at, max.em.its = 75, 
                  gamma.prior="L1", data = unite_out$meta, init.type = "Spectral")

labelTopics (unite_fit2)
unite_meta$verified <- as.factor(unite_meta$verified)
#use largest proportional topic
#first, find it 
plot.STM(unite_fit2,type="summary", xlim=c(0, .3))
#just top 20 
plot.STM(unite_fit2,type="summary", xlim=c(0, .3), ylim=c(54,74))

#effect of verified users
#number of simulations default = 25 
prep <- estimateEffect(c(54) ~ verified, unite_fit2, meta = unite_meta, uncertainty = "Global")
summary(prep)
plot(prep, "verified", model=unite_fit2, method="pointestimate")

#effect of time of tweet 
#prep2 <- estimateEffect(c(54) ~ created_at, unite_fit2, meta = unite_meta, uncertainty = "None")
#Error in qr.default(xmat) : too large a matrix for LINPACK

prep2 <- estimateEffect(1:74 ~ verified, unite_fit2, meta = unite_meta, uncertainty = "Global")
summary(prep2, topics=54)
summary(prep2, topics=43)
summary(prep2, topics=55)
#the marginal topic proportion for each of the levels

plot(prep, "verified", model=unite_fit2, method="pointestimate")

plot(prep2,covariate ="verified", topics = c(54,43,55), 
     model=unite_fit2, method="pointestimate", 
     xlab = "Verified Twitter User....Not Verified", 
     main = "Effect of Verified Twitter Users", 
     xlim = c(-.1, .1))

#from Github page - how to use estimate effect
#Just one topic (note we need c() to indicate it is a vector)
#prep <- estimateEffect(c(1) ~ treatment, gadarianFit, gadarian)
#summary(prep)
#plot(prep, "treatment", model=gadarianFit, method="pointestimate")

#three topics at once
#prep <- estimateEffect(1:3 ~ treatment, gadarianFit, gadarian)
#summary(prep)
#plot(prep, "treatment", model=gadarianFit, method="pointestimate")

##with interactions
#prep <- estimateEffect(1 ~ treatment*s(pid_rep), gadarianFit, gadarian)
#summary(prep)

#when stuck at "completed E step" change gamma.prior to "L1", and get unstuck. 
#run 3rd model with k=0 per k=0 fit1

unite_fit3 <- stm(documents = unite_out$documents, vocab = unite_out$vocab, K=0, 
                  prevalence =~unite_meta$verified + unite_meta$created_at, 
                  gamma.prior="L1", data = unite_out$meta, init.type = "Spectral")

labelTopics (unite_fit3)

# i need to isolate the time of tweets from unite_meta
#then convert to a continuous variable 
#use seperate function from tidyr 
library(tidyverse)
test<- separate(unite_meta, created_at, c("date","time"), sep = " ")
unite_meta <-separate(unite_meta, created_at, c("date","time"), sep = " ")
#now have unite_meta$date and unite_meta$time 

#convert time to continuous variable 
unite_meta$time <- as.POSIXct(unite_meta$time, format = "%H:%M:%S")

#here's the example 
#str(Sys.time())
# POSIXct[1:1], format: "2018-08-07 15:14:21"
#unclass(Sys.time())
# [1] 1533654871]

#so...
unclass(unite_meta$time)
#converts to seconds since the beginning of January 1, 1970, also known as seconds since epoch

#now run unite_fit3 again 
#run 3rd model with k=0 per k=0 fit1

unite_fit3 <- stm(documents = unite_out$documents, vocab = unite_out$vocab, K=0, 
                  prevalence =~unite_meta$verified + unite_meta$time, 
                  gamma.prior="L1", data = unite_out$meta, init.type = "Spectral")

save.image("~/blm_tweets/asw_analysis_8718.RData")
#if it doesn't make snese, use s(unite_meta$time) to 
labelTopics (unite_fit3)

#effect of time of tweet 
#prep2 <- estimateEffect(c(54) ~ created_at, unite_fit2, meta = unite_meta, uncertainty = "None")
#Error in qr.default(xmat) : too large a matrix for LINPACK

prep3 <- estimateEffect(1:60 ~ verified + time, unite_fit3, meta = unite_meta, uncertainty = "Global")
summary(prep3)

#largest proportional topics??
plot.STM(unite_fit3,type="summary", xlim=c(0, .3))
#break up visual 
plot.STM(unite_fit3,type="summary", xlim=c(0, .3), ylim=c(40,60))
plot.STM(unite_fit3,type="summary", xlim=c(0, .3), ylim=c(20,40))
plot.STM(unite_fit3,type="summary", xlim=c(0, .3), ylim=c(1,20))

#effect of verified users
#number of simulations default = 25 
plot(prep3, "verified", model=unite_fit3, method="pointestimate")

plot(prep3, covariate = "verified", topics = c(40, 44, 31),
     model = unite_fit3, method = "difference",
     cov.value1 = "Not Verified", cov.value2 = "Verified",
     xlab = "Not Verified ... Verified",
     main = "Effect of Verified Users",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('Topic 40','Topic 44', 'Topic 31'))
     
summary(prep3, topics=40)
#Call:
#  estimateEffect(formula = 1:60 ~ verified + time, stmobj = unite_fit3, 
#                 metadata = unite_meta, uncertainty = "Global")


#Topic 40:
  
#  Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) -1.579e+01  1.871e+01  -0.844    0.399
#verified    -1.127e-03  4.243e-03  -0.266    0.791
#time         1.030e-08  1.220e-08   0.845    0.398

summary(prep3, topics=44)
#Call:
#  estimateEffect(formula = 1:60 ~ verified + time, stmobj = unite_fit3, 
#                 metadata = unite_meta, uncertainty = "Global")


#Topic 44:
  
#  Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) -1.582e+01  1.858e+01  -0.851    0.395
#verified    -1.135e-03  4.272e-03  -0.266    0.790
#time         1.032e-08  1.212e-08   0.852    0.394

summary(prep3, topics=31)
#Call:
#  estimateEffect(formula = 1:60 ~ verified + time, stmobj = unite_fit3, 
#                 metadata = unite_meta, uncertainty = "Global")


#Topic 31:
  
#  Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) -1.582e+01  1.867e+01  -0.847    0.397
#verified    -1.136e-03  4.244e-03  -0.268    0.789
#time         1.032e-08  1.218e-08   0.848    0.397
#verified user and time not significant 

#the marginal topic proportion for each of the levels


#Evaluate
#searchk runs selectmodel for researcher selected # of k and computes diagnostic properties for
#the returned model. 

#unite
unite_storage <- searchK(unite_out$documents, unite_out$vocab, K = c(10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100), data = unite_meta) 
plot(unite_storage)

#lowest held out likelihood = 100 topics
#lowest residual = 80 topics
#highest semantic coherence = 60 topics
#highest lower bound = 100 topics

#algorithm chose 74 topics, 74 seems reasonable 

#Understand 
#STM lets us do a couple of things: 
#1: display words associated with topics 

#labelTopics 
labelTopics (unite_fit)

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


##########
#Understand 
#########
#STM lets us do a couple of things: 
#1: display words associated with topics 

#labelTopics 
labelTopics (unite_fit2)

#plot topic model 
plot.STM(unite_fit2,type="summary", xlim=c(0, .3))
#For paper, just top 10 
plot.STM(unite_fit2,type="summary", xlim=c(0, .3), ylim=c(40,50))

#convert to dataframe and export to csv 
k50_unite_right <- make.dt(unite_fit2)
write.csv(k50_unite_right, "topicModelresults_unite_right_K50_7_9_18.csv")

#show the topics that proportionally make up the most of the corpus
#cville first 
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))
model2_unite_thoughts37 <- findThoughts(unite_fit2, texts=unite_meta$text, topics=37, n=3)
model2_unite_thoughts54 <- findThoughts(unite_fit2, texts=unite_meta$text, topics=54, n=2)
model2_unite_thoughts30 <- findThoughts(unite_fit2, texts=unite_meta$text, topics=30, n=2)
plotQuote(model2_unite_thoughts37$docs[[1]], main = "#UniteTheRight Topic 37")
plotQuote(model2_unite_thoughts54$docs[[1]], main = "#UniteTheRight Topic 54")
plotQuote(model2_unite_thoughts30$docs[[1]], main = "#UniteTheRight Topic 30")

#plot word clouds with sample tweets
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))

plotQuote(model2_unite_thoughts37$docs[[1]], main = "Topic 37")
cloud(unite_fit2, topic = 37, scale = c(2, .25))

plotQuote(model2_unite_thoughts54$docs[[1]], main = "Topic 54")
cloud(unite_fit2, topic = 54, scale = c(2, .25))

plotQuote(model2_unite_thoughts30$docs[[1]], main = "Topic 30")
cloud(unite_fit2, topic = 30, scale = c(2, .25))

#label topics 

#plot number of topic
unite_storage <- searchK(unite_out$documents, unite_out$vocab, K = c(10,20,30,40,50,60,70,80,90,100), data = meta) 
plot(unite_storage)

#lowest held out likelihood = 100 topics
#lowest residual = 80 topics
#highest semantic coherence = 60 topics
#highest lower bound = 100 topics

out$meta$rating <- as.factor(out$meta$rating)
prep <- estimateEffect(1:20 ~ rating + s(day), poliblogPrevFit,
                       + meta = out$meta, uncertainty = "Global")
summary(prep, topics=1)


#####################
#word network 
#
#####################

# library(devtools)
#install_github("dgrtwo/widyr")
library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
climate_tweets_paired_words <- climate_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

climate_tweets_paired_words %>%
  count(paired_words, sort = TRUE)
ibrary(tidyr)
climate_tweets_separated_words <- climate_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

climate_tweets_filtered <- climate_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
climate_words_counts <- climate_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(climate_words_counts)

library(igraph)
library(ggraph)

# plot climate change word network
climate_words_counts %>%
  filter(n >= 24) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - Climate Change",
       subtitle = "Text mining twitter data ",
       x = "", y = "")

#####################
#
#then #charlottesville
#
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

#note for github
