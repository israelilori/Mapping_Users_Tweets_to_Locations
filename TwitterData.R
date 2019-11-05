# Open file in R

castro_tweets <- read.csv("Castro_keyword.csv", header = TRUE)
harris_tweets <- read.csv("Harris_keyword.csv", header = TRUE)
sanders_tweets <- read.csv("Sanders_keyword.csv", header = TRUE)
trump_tweets <- read.csv("Trump_keyword.csv", header = TRUE)
warren_tweets <- read.csv("Warren_keyword.csv", header = TRUE)
cortez_tweets <- read.csv("Ocasio_Cortez_keyword.csv", header = TRUE)

df <- rbind(castro_tweets, harris_tweets, sanders_tweets, trump_tweets, warren_tweets, cortez_tweets)
df

write.csv(df, file="df.csv")

#df1 <- sapply(df, function(x) x$getText())

#To extract text from the body of the tweets
df1<- df$text


#convert all text to lower case
df2<- tolower(df1)

# Replace blank space ("rt")
df3 <- gsub("rt", "", df2)

# Replace @UserName
df4 <- gsub("@\\w+", "", df3)

# Remove punctuation
df5 <- gsub("[[:punct:]]", "", df4)

# Remove links
df6 <- gsub("http\\w+", "", df5)

# Remove tabs
df7 <- gsub("[ |\t]{2,}", "", df6)

# Remove blank spaces at the beginning
df8 <- gsub("^ ", "", df7)

# Remove blank spaces at the end
df9 <- gsub(" $", "", df8)



#clean up by removing stop words
install.packages("tm")
library(tm)
df_text1 <- Corpus(VectorSource(df9))

Textprocessing <- function(x)
{gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) ## Remove URLs
  gsub('\\b+RT', '', x) ## Remove RT
  gsub('#\\S+', '', x) ## Remove Hashtags
  gsub('@\\S+', '', x) ## Remove Mentions
  gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub('[[:punct:]]', '', x) ## Remove Punctuations
  gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  gsub(' +',' ',x) ## Remove extra whitespaces
}

df_text2 <- tm_map(df_text1,Textprocessing)

mystopwords <- c(stopwords("english"),"rt","нн","get","like","just","yes","know","will","good","day","people","the", "her", "for", "that")

df_text2 <- tm_map(df_text1, removeWords, stopwords)
df_text2 <- tm_map(df_text1, removePunctuation)
df_text2 <- tm_map(df_text1, content_transformer(tolower))
df_text2 <- tm_map(df_text1, stripWhitespace)


#building the wordcloud
install.packages("wordcloud")
library(wordcloud)
pal <- brewer.pal(8, "Dark2")

wordcloud(df_text2, min.freq = 200, max.words = Inf, random.order = FALSE, colors = pal)


######
install.packages("SentimentAnalysis")
library(SentimentAnalysis)
library(ggplot2)
#library(get)
#getting emotions using in-built function
mysentiment <- get_nrc_sentiment(df9)


#calculationg total score for each sentiment
Sentimentscores <- data.frame(colSums(mysentiment[,]))

names(Sentimentscores) <- "Score"
Sentimentscores <- cbind("sentiment"=rownames(Sentimentscores),Sentimentscores)
rownames(Sentimentscores) <- NULL


#plotting the sentiments with scores
ggplot(data=Sentimentscores, aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment), stat = "identity") +
  theme(legend.position ="none") +
  xlab("Sentiments") + ylab("scores") + ggtitle("Sentiments of tweets")



names(df)
#packages ---
install.packages("tm")
install.packages("lubridate")
install.packages("dplyr")
install.packages("plotly")
install.packages("scales")
install.packages("formattable")
install.packages("RSentiment")
install.packages("stringr")
install.packages("broom")
install.packages("tidyr")
install.packages("tidytext")
install.packages("igraph")
install.packages("ggplot2")
install.packages("ggplot")

library(ggplot2)
library(ggplot)
library(tm)
library(lubridate)
library(dplyr)
library(plotly)
library(scales)
library(formattable)
library(RSentiment)
library(stringr)
library(broom)
library(tidyr)
library(tidytext)
library(igraph)


#Statistical Analysis for Castro tweets. If they are original tweets or not

temp <- castro_tweets %>% select(screen_name,is_retweet)
temp$is_retweet <- ifelse(temp$is_retweet=='FALSE','Original Tweets','Retweets')
temp<- temp %>% group_by(is_retweet,screen_name) %>% summarise(n=n())
ggplot(temp, aes(x=screen_name, y=n, fill=is_retweet)) +
  geom_bar(position="dodge",stat='identity')



#----Showing the locations of the users talking about the candidates
# install relevant packages
install.packages("twitteR")
install.packages("ROAuth")
install.packages("RJSONIO")
install.packages("data.table")
install.packages("leaflet")
library(twitteR)
library(ROAuth)
library(RJSONIO)
library(leaflet)
library(data.table)

# authenticate with Twitter
consumerKey<-	"gqOGypzh3PPHPsdYsSA8XVxw3"
consumerSecret<-"lrkmNz3Ev4zlekYpgt47lM22g23ZT3LTDHdv1eIzH6djXGu1Ze"

accessToken<-"1091711245315641344-RJ82bw7Brx3m4gnw1hrcx7qMLohi6n"
accessSecret<-"OponAQ4utL3ihKlMEe4pEQJoRUT5py2j5Vcg2CMjDyqWP"

setup_twitter_oauth (consumerKey, consumerSecret, accessToken, accessSecret)  # authenticate


#For Candidate Julian Castro

#Read in the file into R
castro <- read.csv("Castro_keyword.csv", header = TRUE)

#Get the location of the candidate via their profile
user<-getUser("JulianCastro")
user$location

#Set the users location from their profile to empty so we can save into it
castro$user_location_on_twitter_bio <- NA

#loop over the various tweets and their users in the dataframe #this was done for the first 200 tweets
for (user in castro$screen_name[1:200]){  
  print(c("finding the profile for:",user))
  Sys.sleep(3) #build in a sleeper to prevent Twitter API rate limit error. 
  try(castro[castro$screen_name==user,]$user_location_on_twitter_bio <- getUser(user)$location)
}

#Linking google earth platform locator so as to correlate with users location on the earth
castro$lat <- NA
castro$lng <- NA

source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")
geocode_apply<-function(x){
  geocode(x, source = "google", output = "all", api_key="AIzaSyDgba3BMFysxhuE8Mi8EkAMoko_fIiJZPk")
}


#create a dataframe for tweets with geo location informations 
castro_withgeo <- castro[castro$user_location_on_twitter_bio != "" & !is.na(castro$user_location_on_twitter_bio),]

#Get corresponding coordinates for each location of the first 200 tweets

for (name in castro_withgeo$user_location_on_twitter_bio[1:200]){ #get the coordinate data for the first 10 tweets via Google Map API.
  rowid <- which(castro_withgeo$user_location_on_twitter_bio == name)
  print(paste0("getting the coordinates for:", name, ", rowid is:", rowid))
  Sys.sleep(1)
  try(geodata <- geocode_apply(name))
 

if (geodata$status=="OK" & length(geodata$results)=="1") {
    print(c("the lat is:", geodata$results[[1]]$geometry$location[[1]]))
    print(c("the lng is:", geodata$results[[1]]$geometry$location[[2]]))
    castro_withgeo[rowid,]$lat <- geodata$results[[1]]$geometry$location[[1]]
    castro_withgeo[rowid,]$lng <- geodata$results[[1]]$geometry$location[[2]]
  }else {
    print ("skipping")
  }
}

#create a separate dataframe called castro_tweets_withgeo. This dataframe contains only complete coordinates. 
castro_tweets_withgeo_show <- castro_withgeo[!is.na(castro_withgeo$lat),c("lat","lng", "user_location_on_twitter_bio", "text")]


#Visualize the results using R's leaflet package
map1 <- leaflet() %>% setView(lng = -98.35, lat = 39.50, zoom = 3)
map1 <- leaflet(data = castro_tweets_withgeo_show) %>% 
  addTiles() %>%
  setView(lng = -98.35, lat = 39.50, zoom = 4) %>% 
  addCircleMarkers(lng = ~lng, lat = ~lat, popup = ~ as.character(user_location_on_twitter_bio), stroke = FALSE, fillOpacity = 0.5) %>% 
  addProviderTiles("CartoDB.Positron") 

map1


#--------
#For Candidate Kamala Harris

#Read in the file into R
harris <- read.csv("Harris_keyword.csv", header = TRUE)

#Get the location of the candidate via their profile
user<-getUser("KamalaHarris")
user$location

#Set the users location from their profile to empty so we can save into it
harris$user_location_on_twitter_bio <- NA

#loop over the various tweets and their users in the dataframe #this was done for the first 200 tweets
for (user in harris$screen_name[1:200]){  
  print(c("finding the profile for:",user))
  Sys.sleep(3) #build in a sleeper to prevent Twitter API rate limit error. 
  try(harris[harris$screen_name==user,]$user_location_on_twitter_bio <- getUser(user)$location)
}

#Linking google earth platform locator so as to correlate with users location on the earth
harris$lat <- NA
harris$lng <- NA

source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")
geocode_apply<-function(x){
  geocode(x, source = "google", output = "all", api_key="AIzaSyDgba3BMFysxhuE8Mi8EkAMoko_fIiJZPk")
}


#create a dataframe for tweets with geo location informations 
harris_withgeo <- harris[harris$user_location_on_twitter_bio != "" & !is.na(harris$user_location_on_twitter_bio),]

#Get corresponding coordinates for each location of the first 200 tweets

for (name in harris_withgeo$user_location_on_twitter_bio[1:200]){ #get the coordinate data for the first 10 tweets via Google Map API.
  rowid <- which(harris_withgeo$user_location_on_twitter_bio == name)
  print(paste0("getting the coordinates for:", name, ", rowid is:", rowid))
  Sys.sleep(1)
  try(geodata <- geocode_apply(name))
  
  
  if (geodata$status=="OK" & length(geodata$results)=="1") {
    print(c("the lat is:", geodata$results[[1]]$geometry$location[[1]]))
    print(c("the lng is:", geodata$results[[1]]$geometry$location[[2]]))
    harris_withgeo[rowid,]$lat <- geodata$results[[1]]$geometry$location[[1]]
    harris_withgeo[rowid,]$lng <- geodata$results[[1]]$geometry$location[[2]]
  }else {
    print ("skipping")
  }
}

#create a separate dataframe called castro_tweets_withgeo. This dataframe contains only complete coordinates. 
harris_tweets_withgeo_show <- harris_withgeo[!is.na(harris_withgeo$lat),c("lat","lng", "user_location_on_twitter_bio", "text")]


#Visualize the results using R's leaflet package
map2 <- leaflet() %>% setView(lng = -98.35, lat = 39.50, zoom = 3)
map2 <- leaflet(data = harris_tweets_withgeo_show) %>% 
  addTiles() %>%
  setView(lng = -98.35, lat = 39.50, zoom = 4) %>% 
  addCircleMarkers(lng = ~lng, lat = ~lat, popup = ~ as.character(user_location_on_twitter_bio), stroke = FALSE, fillOpacity = 0.5) %>% 
  addProviderTiles("CartoDB.Positron") 

map2


#--------
#For Candidate Bernie Sanders

#Read in the file into R
sanders <- read.csv("Sanders_keyword.csv", header = TRUE)

#Get the location of the candidate via their profile
user<-getUser("BernieSanders")
user$location

#Set the users location from their profile to empty so we can save into it
sanders$user_location_on_twitter_bio <- NA

#loop over the various tweets and their users in the dataframe #this was done for the first 200 tweets
for (user in sanders$screen_name[1:200]){  
  print(c("finding the profile for:",user))
  Sys.sleep(3) #build in a sleeper to prevent Twitter API rate limit error. 
  try(sanders[sanders$screen_name==user,]$user_location_on_twitter_bio <- getUser(user)$location)
}

#Linking google earth platform locator so as to correlate with users location on the earth
sanders$lat <- NA
sanders$lng <- NA

source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")
geocode_apply<-function(x){
  geocode(x, source = "google", output = "all", api_key="AIzaSyDgba3BMFysxhuE8Mi8EkAMoko_fIiJZPk")
}


#create a dataframe for tweets with geo location informations 
sanders_withgeo <- sanders[sanders$user_location_on_twitter_bio != "" & !is.na(sanders$user_location_on_twitter_bio),]

#Get corresponding coordinates for each location of the first 200 tweets

for (name in sanders_withgeo$user_location_on_twitter_bio[1:200]){ #get the coordinate data for the first 10 tweets via Google Map API.
  rowid <- which(sanders_withgeo$user_location_on_twitter_bio == name)
  print(paste0("getting the coordinates for:", name, ", rowid is:", rowid))
  Sys.sleep(1)
  try(geodata <- geocode_apply(name))
  
  
  if (geodata$status=="OK" & length(geodata$results)=="1") {
    print(c("the lat is:", geodata$results[[1]]$geometry$location[[1]]))
    print(c("the lng is:", geodata$results[[1]]$geometry$location[[2]]))
    sanders_withgeo[rowid,]$lat <- geodata$results[[1]]$geometry$location[[1]]
    sanders_withgeo[rowid,]$lng <- geodata$results[[1]]$geometry$location[[2]]
  }else {
    print ("skipping")
  }
}

#create a separate dataframe called castro_tweets_withgeo. This dataframe contains only complete coordinates. 
sanders_tweets_withgeo_show <- sanders_withgeo[!is.na(sanders_withgeo$lat),c("lat","lng", "user_location_on_twitter_bio", "text")]


#Visualize the results using R's leaflet package
map3 <- leaflet() %>% setView(lng = -98.35, lat = 39.50, zoom = 3)
map3 <- leaflet(data = sanders_tweets_withgeo_show) %>% 
  addTiles() %>%
  setView(lng = -98.35, lat = 39.50, zoom = 4) %>% 
  addCircleMarkers(lng = ~lng, lat = ~lat, popup = ~ as.character(user_location_on_twitter_bio), stroke = FALSE, fillOpacity = 0.5) %>% 
  addProviderTiles("CartoDB.Positron") 

map3


#--------
#For Candidate Elizabeth Warren

#Read in the file into R
warren <- read.csv("Warren_keyword.csv", header = TRUE)

#Get the location of the candidate via their profile
user<-getUser("eWarren")
user$location

#Set the users location from their profile to empty so we can save into it
warren$user_location_on_twitter_bio <- NA

#loop over the various tweets and their users in the dataframe #this was done for the first 200 tweets
for (user in warren$screen_name[1:200]){  
  print(c("finding the profile for:",user))
  Sys.sleep(3) #build in a sleeper to prevent Twitter API rate limit error. 
  try(warren[warren$screen_name==user,]$user_location_on_twitter_bio <- getUser(user)$location)
}

#Linking google earth platform locator so as to correlate with users location on the earth
warren$lat <- NA
warren$lng <- NA

source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")
geocode_apply<-function(x){
  geocode(x, source = "google", output = "all", api_key="AIzaSyDgba3BMFysxhuE8Mi8EkAMoko_fIiJZPk")
}


#create a dataframe for tweets with geo location informations 
warren_withgeo <- warren[warren$user_location_on_twitter_bio != "" & !is.na(warren$user_location_on_twitter_bio),]

#Get corresponding coordinates for each location of the first 200 tweets

for (name in warren_withgeo$user_location_on_twitter_bio[1:200]){ #get the coordinate data for the first 10 tweets via Google Map API.
  rowid <- which(warren_withgeo$user_location_on_twitter_bio == name)
  print(paste0("getting the coordinates for:", name, ", rowid is:", rowid))
  Sys.sleep(1)
  try(geodata <- geocode_apply(name))
  
  
  if (geodata$status=="OK" & length(geodata$results)=="1") {
    print(c("the lat is:", geodata$results[[1]]$geometry$location[[1]]))
    print(c("the lng is:", geodata$results[[1]]$geometry$location[[2]]))
    warren_withgeo[rowid,]$lat <- geodata$results[[1]]$geometry$location[[1]]
    warren_withgeo[rowid,]$lng <- geodata$results[[1]]$geometry$location[[2]]
  }else {
    print ("skipping")
  }
}

#create a separate dataframe called castro_tweets_withgeo. This dataframe contains only complete coordinates. 
warren_tweets_withgeo_show <- warren_withgeo[!is.na(warren_withgeo$lat),c("lat","lng", "user_location_on_twitter_bio", "text")]


#Visualize the results using R's leaflet package
map4 <- leaflet() %>% setView(lng = -98.35, lat = 39.50, zoom = 3)
map4 <- leaflet(data = warren_tweets_withgeo_show) %>% 
  addTiles() %>%
  setView(lng = -98.35, lat = 39.50, zoom = 4) %>% 
  addCircleMarkers(lng = ~lng, lat = ~lat, popup = ~ as.character(user_location_on_twitter_bio), stroke = FALSE, fillOpacity = 0.5) %>% 
  addProviderTiles("CartoDB.Positron") 

map4


#--------
#For Candidate Alexandria Ocasio Cortez 

#Read in the file into R
cortez <- read.csv("Ocasio_Cortez_keyword.csv", header = TRUE)

#Get the location of the candidate via their profile
user<-getUser("AOC")
user$location

#Set the users location from their profile to empty so we can save into it
cortez$user_location_on_twitter_bio <- NA

#loop over the various tweets and their users in the dataframe #this was done for the first 200 tweets
for (user in cortez$screen_name[1:200]){  
  print(c("finding the profile for:",user))
  Sys.sleep(3) #build in a sleeper to prevent Twitter API rate limit error. 
  try(cortez[cortez$screen_name==user,]$user_location_on_twitter_bio <- getUser(user)$location)
}

#Linking google earth platform locator so as to correlate with users location on the earth
cortez$lat <- NA
cortez$lng <- NA

source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")
geocode_apply<-function(x){
  geocode(x, source = "google", output = "all", api_key="AIzaSyDgba3BMFysxhuE8Mi8EkAMoko_fIiJZPk")
}


#create a dataframe for tweets with geo location informations 
cortez_withgeo <- cortez[cortez$user_location_on_twitter_bio != "" & !is.na(cortez$user_location_on_twitter_bio),]

#Get corresponding coordinates for each location of the first 200 tweets

for (name in cortez_withgeo$user_location_on_twitter_bio[1:200]){ #get the coordinate data for the first 10 tweets via Google Map API.
  rowid <- which(cortez_withgeo$user_location_on_twitter_bio == name)
  print(paste0("getting the coordinates for:", name, ", rowid is:", rowid))
  Sys.sleep(1)
  try(geodata <- geocode_apply(name))
  
  
  if (geodata$status=="OK" & length(geodata$results)=="1") {
    print(c("the lat is:", geodata$results[[1]]$geometry$location[[1]]))
    print(c("the lng is:", geodata$results[[1]]$geometry$location[[2]]))
    cortez_withgeo[rowid,]$lat <- geodata$results[[1]]$geometry$location[[1]]
    cortez_withgeo[rowid,]$lng <- geodata$results[[1]]$geometry$location[[2]]
  }else {
    print ("skipping")
  }
}

#create a separate dataframe called castro_tweets_withgeo. This dataframe contains only complete coordinates. 
cortez_tweets_withgeo_show <- cortez_withgeo[!is.na(cortez_withgeo$lat),c("lat","lng", "user_location_on_twitter_bio", "text")]


#Visualize the results using R's leaflet package
map5 <- leaflet() %>% setView(lng = -98.35, lat = 39.50, zoom = 3)
map5 <- leaflet(data = cortez_tweets_withgeo_show) %>% 
  addTiles() %>%
  setView(lng = -98.35, lat = 39.50, zoom = 4) %>% 
  addCircleMarkers(lng = ~lng, lat = ~lat, popup = ~ as.character(user_location_on_twitter_bio), stroke = FALSE, fillOpacity = 0.5) %>% 
  addProviderTiles("CartoDB.Positron") 

map5


#--------
#For Candidate President Donald Trump 

#Read in the file into R
trump <- read.csv("Trump_keyword.csv", header = TRUE)

#Get the location of the candidate via their profile
user<-getUser("realDonaldTrump")
user$location

#Set the users location from their profile to empty so we can save into it
trump$user_location_on_twitter_bio <- NA

#loop over the various tweets and their users in the dataframe #this was done for the first 200 tweets
for (user in trump$screen_name[1:500]){  
  print(c("finding the profile for:",user))
  Sys.sleep(3) #build in a sleeper to prevent Twitter API rate limit error. 
  try(trump[trump$screen_name==user,]$user_location_on_twitter_bio <- getUser(user)$location)
}

#Linking google earth platform locator so as to correlate with users location on the earth
trump$lat <- NA
trump$lng <- NA

source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")
geocode_apply<-function(x){
  geocode(x, source = "google", output = "all", api_key="AIzaSyDgba3BMFysxhuE8Mi8EkAMoko_fIiJZPk")
}


#create a dataframe for tweets with geo location informations 
trump_withgeo <- trump[trump$user_location_on_twitter_bio != "" & !is.na(trump$user_location_on_twitter_bio),]

#Get corresponding coordinates for each location of the first 200 tweets

for (name in trump_withgeo$user_location_on_twitter_bio[1:500]){ #get the coordinate data for the first 10 tweets via Google Map API.
  rowid <- which(trump_withgeo$user_location_on_twitter_bio == name)
  print(paste0("getting the coordinates for:", name, ", rowid is:", rowid))
  Sys.sleep(1)
  try(geodata <- geocode_apply(name))
  
  
  if (geodata$status=="OK" & length(geodata$results)=="1") {
    print(c("the lat is:", geodata$results[[1]]$geometry$location[[1]]))
    print(c("the lng is:", geodata$results[[1]]$geometry$location[[2]]))
    trump_withgeo[rowid,]$lat <- geodata$results[[1]]$geometry$location[[1]]
    trump_withgeo[rowid,]$lng <- geodata$results[[1]]$geometry$location[[2]]
  }else {
    print ("skipping")
  }
}

#create a separate dataframe called castro_tweets_withgeo. This dataframe contains only complete coordinates. 
trump_tweets_withgeo_show <- trump_withgeo[!is.na(trump_withgeo$lat),c("lat","lng", "user_location_on_twitter_bio", "text")]


#Visualize the results using R's leaflet package
map6 <- leaflet() %>% setView(lng = -98.35, lat = 39.50, zoom = 3)
map6 <- leaflet(data = trump_tweets_withgeo_show) %>% 
  addTiles() %>%
  setView(lng = -98.35, lat = 39.50, zoom = 4) %>% 
  addCircleMarkers(lng = ~lng, lat = ~lat, popup = ~ as.character(user_location_on_twitter_bio), stroke = FALSE, fillOpacity = 0.5) %>% 
  addProviderTiles("CartoDB.Positron") 

map6
