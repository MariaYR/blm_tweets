#BlackLivesMatter descriptives 
#compiling geolocated tweets
#begun 2.14.18
#fnsihed 2.15.18

library(twitteR)
install.packages("streamR")
library(streamR)
install.packages("ROAuth")
library (ROAuth)
install.packages ("tidyverse")
library (tidyverse)

setwd ("~/Dropbox/BLMTweets/Charlottesville")

#statrt with Charlottesville 
protest <- parseTweets("Charlottesville.json", simplify = TRUE)
table (protest$geo_enabled) 
#FALSE  TRUE 
#13571 10379 

#convert to tibble 
as_tibble(protest)

#filter geoenabled tweets
protest_geo <- protest %>% filter (geo_enabled == TRUE)

names(protest_geo)
table(protest_geo$location)
table(protest_geo$time_zone) #better way to think of location is timezone, more complete

#next data set from BLM 
#Charleston 

setwd ("~/Dropbox/BLMTweets/Charleston")
charleston <- parseTweets("tweets_Charleston.json", simplify = TRUE)
#hmm, these files are nowhere to be found 
#might be in the rdata set though. have to do mamually 

#Next dataset, Flint
setwd ("~/Dropbox/BLMTweets/FlintWaterCrisis")
flint <- parseTweets("tweets_Flint.json", simplify = TRUE)
as_tibble(flint)
names(flint)
flint_geo <- flint %>% filter (geo_enabled == TRUE)

#next one = BLM 

setwd ("~/Dropbox/BLMTweets/BlackLivesMatter")
BLMtweets <- parseTweets("tweets_BLM.json", simplify = TRUE)
#hmm, no tweets here either...

#next one = baltimore riots 
setwd ("~/Dropbox/BLMTweets/Baltimore")
bmore <- parseTweets("tweets_bmore.json", simplify = TRUE)
as_tibble(bmore)
names(bmore)
bmore_geo <- bmore %>% filter (geo_enabled == TRUE)

#last one = Alton Sterling 
setwd ("~/Dropbox/BLMTweets/AltonSterling")
alton <- parseTweets("tweets_Alton.json", simplify = TRUE)
as_tibble(alton)
alton_geo <- alton %>% filter (geo_enabled == TRUE)

#append all geos together 
one_geo <- rbind(protest_geo, flint_geo)
two_geo <- rbind(one_geo, bmore_geo)
all_blm_geo <- rbind(two_geo, alton_geo)

#check it went ok
names(all_blm_geo)
source('functions_by_pablobarbera.R')
getCommonHashtags(all_blm_geo$text, n=50)

#cool, export data for use in next round 
setwd ("~/Dropbox/BLMTweets/SCORE_Analysis")
write.csv(all_blm_geo, file = "all_blm_geo.csv")

save.image("~/Dropbox/BLMTweets/SCORE_Analysis/geo_parse.RData")

