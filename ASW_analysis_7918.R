#ASW full analysis 

## remove any previously saved objects in your R environment
rm(list=ls())

setwd ("~/Dropbox/blm_tweets/")
load("~/Dropbox/blm_tweets/asw_analysis.RData")
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

names(unite_analysis)

#plot tweeter locations
unite_analysis %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(20) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "#UniteTheRight Tweeters - unique locations ")

#plot most active tweeters
d <- as.data.frame(table(unite_analysis$screen_name))
d <- d[order(d$Freq, decreasing=T), ] #descending order of tweeters according to frequency of tweets
names(d) <- c("User","Tweets")
head(d)

# Plot the table above for the top 20
barplot(head(d$Tweets, 20), names=head(d$User, 20), horiz=T, las=1, main="Top 20 #UniteTheRight Tweeters: Tweets per User", col=1)

#begin STM 
unite_processed <- textProcessor(unite_analysis$stripped_text, metadata = unite_analysis)

#prepare
unite_out <- prepDocuments(unite_processed$documents, unite_processed$vocab, unite_processed$meta)

out <- unite_out
#Removing 5729 of 12102 terms (5729 of 578582 tokens) due to frequency  
#Removing 202 Documents with No Words 
#Your corpus now has 76016 documents, 6373 terms and 572853 tokens.

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
install.packages("Rtsne")
install.packages("rsvd")
install.packages("geometry")
library (Rtsne)
library(rsvd)
library(geometry)

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

#number of unique users
unique(unite_meta$screen_name)


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

#cool, run new stm with time as prevalence factor 
#prevalence = covariates affecting frequency with which topics are discussed 
#here time. Prevalance also includes a topical content covariate, 
#here whether the account is verified
#unite_fit2 will use k=74 from above, but addes whether user is verified and time
#as covariates 

head(unite_meta$created_at)
head(unite_meta$verified)


unite_fit2 <- stm(documents = unite_out$documents, vocab = unite_out$vocab, K=5, 
                  prevalence =~ unite_meta$created_at + unite_meta$verified, max.em.its = 10,  
                  data = unite_out$meta, init.type = "Spectral")

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

#rest
plot.STM(unite_fit2,type="summary", xlim=c(0, .3), ylim=c(44,53))
plot.STM(unite_fit2,type="summary", xlim=c(0, .3), ylim=c(24,44))
plot.STM(unite_fit2,type="summary", xlim=c(0, .3), ylim=c(1,23))


#effect of verified users
#number of simulations default = 25 
#try devtools::install_github('methodds/stminsights')
#run_stminsights()
prep_all <- estimateEffect(1:74 ~ verified + created_at, unite_fit2, meta = unite_meta, uncertainty = "Global")
prep <- estimateEffect(c(54, 55, 43) ~ verified + created_at, unite_fit2, meta = unite_meta, uncertainty = "Global")
summary(prep)
#in below plot, time created at is kept  at the median 
summary(unite_meta$created_at)
#"2017-08-13 23:28:53"

plot(prep, "verified", model=unite_fit2, method="pointestimate",
     width = 5, main ="Estimated effect of verified users & time of tweet")

prep2 <- estimateEffect(c(35, 20, 73) ~ verified + created, unite_fit2, meta = unite_meta, uncertainty = "Global")
summary(prep2)
plot(prep2, "verified", model=unite_fit2, method="pointestimate",
     width = 10, main ="Estimated effect of verified users & time of tweet")

prep3 <- estimateEffect(c(54, 55, 43) ~ verified, unite_fit2, meta = unite_meta, uncertainty = "Global")
summary(prep3)
plot(prep3, "verified", model=unite_fit2, method="pointestimate",
     width = 10, main ="Estimated effect of verified users & time of tweet")

prep4 <- estimateEffect(c(53, 59, 61, 56) ~ verified, unite_fit2, meta = unite_meta, uncertainty = "Global")
summary(prep4)
plot(prep4, "verified", model=unite_fit2, method="pointestimate",
     width = 10, main ="Estimated effect of verified users & time of tweet")

#just plot top 10 topics
#54, 55, 43, 35, 20, 73, 53, 59, 61, 56
prep_top <- estimateEffect(c(54, 55, 43, 35, 20, 73, 53, 59, 61, 56) ~ verified + created_at, unite_fit2, meta = unite_meta, uncertainty = "Global")
summary(prep_top)
####################
#> summary(prep_top)


#Call:
#  estimateEffect(formula = c(54, 55, 43, 35, 20, 73, 53, 59, 61, 
#                             56) ~ verified + created_at, stmobj = unite_fit2, metadata = unite_meta, 
#                 uncertainty = "Global")


#Topic 54:
  
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  7.093e+02  3.729e+01  19.020  < 2e-16 ***
#verified    -2.722e-02  6.690e-03  -4.069 4.72e-05 ***
#created_at  -4.720e-07  2.482e-08 -19.019  < 2e-16 ***
  ---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#interpretation - verified users use this topic more than non-verified users
#time tweeted is not a strong predictor of this topic ??
  
#Topic 55:
  
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  5.187e+02  3.542e+01  14.645  < 2e-16 ***
#verified    -2.741e-02  6.434e-03  -4.261 2.04e-05 ***
#created_at  -3.452e-07  2.357e-08 -14.644  < 2e-16 ***
  ---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#interpretation - unverified users use this topic more than verified users
#time tweeted is not a strong predictor of this topic 
  

#Topic 43:
  
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  4.621e+02  4.191e+01  11.024  < 2e-16 ***
#verified    -1.864e-02  7.203e-03  -2.587  0.00967 ** 
#created_at  -3.075e-07  2.789e-08 -11.023  < 2e-16 ***
  ---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#interpretation - nonverified users use this topic more than verified users
#time tweeted is not strong predictor of this topic OR is it about how early/late
  

#Topic 35:
  
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  4.069e+02  3.542e+01  11.487  < 2e-16 ***
#verified     5.084e-02  9.373e-03   5.425 5.82e-08 ***
#created_at  -2.708e-07  2.357e-08 -11.486  < 2e-16 ***
  ---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#interpretation - verified users use this topic more than non-verified users
  

#Topic 20:
  
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  9.291e+02  3.425e+01  27.125  < 2e-16 ***
#verified    -2.220e-02  6.488e-03  -3.421 0.000624 ***
#created_at  -6.183e-07  2.280e-08 -27.124  < 2e-16 ***
  ---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#interpretation - nonverified users use this topic more than verified users
  
#Topic 73:
  
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.394e+02  3.571e+01   3.904 9.45e-05 ***
#verified    -1.997e-02  6.167e-03  -3.239   0.0012 ** 
#created_at  -9.275e-08  2.376e-08  -3.904 9.48e-05 ***
  ---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#interpretation - nonverified users use this topic more than verified users
  

#Topic 53:
  
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.049e+03  3.166e+01 -33.130   <2e-16 ***
#verified     7.393e-03  7.573e-03   0.976    0.329    
#created_at   6.981e-07  2.107e-08  33.130   <2e-16 ***
  ---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#interpretation - verified users use this topic more than non-verified users
#but not statistically significant 
#time tweeted is strong predictor of this topic 
  

#Topic 59:
  
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.519e+02  2.647e+01   5.740 9.52e-09 ***
#verified     1.898e-02  7.580e-03   2.504   0.0123 *  
#created_at  -1.011e-07  1.761e-08  -5.739 9.56e-09 ***
  ---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#interpretation - verified users use this topic more than non-verified users
  

#Topic 61:
  
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -2.318e+02  2.928e+01  -7.917 2.47e-15 ***
#verified     1.434e-03  6.024e-03   0.238    0.812    
#created_at   1.543e-07  1.949e-08   7.917 2.46e-15 ***
  ---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#interpretation - verified users use this topic more than non-verified users
#but not statistically signifcant 
#time tweet created stronger predictor 
  
  

#Topic 56:
  
#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)  
#(Intercept) -5.645e+01  3.080e+01  -1.833   0.0668 .
#verified     1.479e-02  7.201e-03   2.054   0.0399 *
#created_at   3.758e-08  2.050e-08   1.834   0.0667 .
---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#interpretation - verified users use this topic more than non-verified users
  
####################

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


#convert time to continuous variable 
#unite_meta$time <- as.POSIXct(unite_meta$time, format = "%H:%M:%S")

#here's the example 
#str(Sys.time())
# POSIXct[1:1], format: "2018-08-07 15:14:21"
#unclass(Sys.time())
# [1] 1533654871]

#so...
#unclass(unite_meta$time)
#converts to seconds since the beginning of January 1, 1970, also known as seconds since epoch

#Calculate topic correlations 
unite_corr <- topicCorr(unite_fit2, method = "simple", cutoff = 0.01, verbose = TRUE)

#must install igraph before graphing correlations 
library(igraph)

#enlarge margins to get most out of image 
par(mar=c(0,0,1,0))

plot (unite_corr, cex = .05, main = "#UniteTheRight Topic Correlations") 

##########
#Understand 
#########

#convert to dataframe and export to csv 
k74_unite_right <- make.dt(unite_fit2)
write.csv(k74_unite_right, "topicModelresults_unite_right_K74.csv")

#show the topics that proportionally make up the most of the corpus
#unite first 
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))
model2_unite_thoughts54 <- findThoughts(unite_fit2, texts=unite_meta$text, topics=54, n=2)
model2_unite_thoughts55 <- findThoughts(unite_fit2, texts=unite_meta$text, topics=55, n=2)
model2_unite_thoughts43 <- findThoughts(unite_fit2, texts=unite_meta$text, topics=43, n=2)
model2_unite_thoughts35 <- findThoughts(unite_fit2, texts=unite_meta$text, topics=35, n=2)
model2_unite_thoughts20 <- findThoughts(unite_fit2, texts=unite_meta$text, topics=20, n=2)
model2_unite_thoughts73 <- findThoughts(unite_fit2, texts=unite_meta$text, topics=73, n=2)
model2_unite_thoughts53 <- findThoughts(unite_fit2, texts=unite_meta$text, topics=53, n=2)
model2_unite_thoughts59 <- findThoughts(unite_fit2, texts=unite_meta$text, topics=59, n=2)
model2_unite_thoughts61 <- findThoughts(unite_fit2, texts=unite_meta$text, topics=61, n=2)
model2_unite_thoughts56 <- findThoughts(unite_fit2, texts=unite_meta$text, topics=56, n=2)

plotQuote(model2_unite_thoughts54$docs[[1]], main = "#UniteTheRight Topic 54")
plotQuote(model2_unite_thoughts55$docs[[1]], main = "#UniteTheRight Topic 55")
plotQuote(model2_unite_thoughts43$docs[[1]], main = "#UniteTheRight Topic 43")
plotQuote(model2_unite_thoughts35$docs[[1]], main = "#UniteTheRight Topic 35")
plotQuote(model2_unite_thoughts20$docs[[1]], main = "#UniteTheRight Topic 20")
plotQuote(model2_unite_thoughts73$docs[[1]], main = "#UniteTheRight Topic 73")
plotQuote(model2_unite_thoughts53$docs[[1]], main = "#UniteTheRight Topic 53")
plotQuote(model2_unite_thoughts59$docs[[1]], main = "#UniteTheRight Topic 59")
plotQuote(model2_unite_thoughts61$docs[[1]], main = "#UniteTheRight Topic 61")
plotQuote(model2_unite_thoughts56$docs[[1]], main = "#UniteTheRight Topic 56")

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

#use stm insights to visualize 
library(stminsights)
run_stminsights()

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

####################
#sentiment analysis of unite_right
####################
library(sentimentr)
unite_analysis

#must use get_sentences for workflow QC
mysentences <- get_sentences(unite_analysis$text)
sentiment(mysentences)

# group_by

#convert verified to false = 0, true = 1 in unite_analysis
unite_analysis$verified [unite_analysis$verified == "TRUE"] <- 1
unite_analysis$verified [unite_analysis$verified == "FALSE"] <- 0
unite_analysis$verified <- as.integer(unite_analysis$verified)
head(unite_analysis$verified)
table(unite_analysis$verified)

#unite_analysis$created_at
head(unite_analysis$created_at)

unite_analysis$created_at <- as.POSIXct(unite_analysis$created_at, format = "%a %b %d %H:%M:%S +0000 %Y")
unite_analysis$hour <- as.POSIXlt(unite_analysis$created_at)$hour
aggregate(.~hour,data=unite_analysis,sum)

names(unite_analysis)
library(lubridate)
library(magrittr)
library(tidyverse)
unite_analysis %>%
  mutate(created_at = as.POSIXct(created_at)) %>%
  group_by(lubridate::hour(created_at), text) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

(sentiment_out <- with(
  unite_analysis, 
  sentiment_by(
    get_sentences(text), 
    list(verified, created_at)
  )
))

plot(sentiment_out)

#I need to chunk time within the 24 hours, so as to make plot more legible 
library(ggplot2)
library(lubridate)
library(scales)
library(tm)
library(stringr)
library(wordcloud)
library(syuzhet)
library(reshape2)
library(dplyr)
library(twitteR)

#extract days
unite_analysis$timeonly <- as.numeric(unite_analysis$created_at - trunc(unite_analysis$created_at, "days"))
class(unite_analysis$timeonly) <- "POSIXct"

ggplot(data = unite_analysis, aes(x = timeonly)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_x_datetime(breaks = date_breaks("2 hours"), 
                   labels = date_format("%H:00")) +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")
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
