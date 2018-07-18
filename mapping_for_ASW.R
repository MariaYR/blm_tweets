#ASW analysis 
#create map of tweets

#Unite the Right goes first 

#required packages
install.packages("leaflet") install.packages("maps") library(leaflet) library(maps)

#load data
read.csv("C:\Users\YourName\Documents\ApptoMap\tweets.csv", stringsAsFactors = FALSE)

#use open base map from leaflet package 
m <- leaflet(mymap) %>% addTiles()

#this creates one circle per tweet
#Let's add circles to the base map. For lng and lat, enter the name of the columns that 
#contain the latitude and longitude of your tweets followed by ~. The ~longitude and 
#~latitude refer to the name of the columns in your .csv file:
m %>% addCircles(lng = ~longitude, lat = ~latitude, popup = mymap$type, weight = 8, radius = 40, color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)

#then this: https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/map-tweet-locations-over-time-r/