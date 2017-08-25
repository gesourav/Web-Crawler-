setwd("C:/myWork/New Yelp/Data")

#Load the required libraries
library(xlsx)
library(stringr)
library(rvest)
library(RCurl)
library(httk)
library(httr)
library(httpuv)
library(qdap)

#Load the restaurants Names
restaurantdata <- read.xlsx("list of restaurants.xlsx", sheetName = "Sheet1")
restaurantdata$restaurant_code <- as.factor(restaurantdata$restaurant_code)
restaurantdata$restaurant_name <- as.character(restaurantdata$restaurant_name)
names(restaurantdata) <- c("restaurantCode", "restaurantName", "restaurantSegment")
restaurantdata[,2] <- tolower(restaurantdata[,2])
splitText <- NULL
searchquery <- data.frame()

#Create the Google Search Query
for(i in 1:nrow(restaurantdata))
{
  splitText <- unlist(strsplit(restaurantdata[i,2], "\\s+"))
  searchquery[i,1] <- paste(splitText, collapse = "+")
  searchquery[i,1] <- paste("https://www.google.com/search?q=yelp+reviews+", searchquery[i,1], sep="")
  searchquery[i,2] <- restaurantdata[i,2]
  searchquery[i,3] <- restaurantdata[i,3]
}

names(searchquery) <- c("Google_Search_Query", "RestaurantName", "RestaurantSegment")

restaurant.urls <- NULL
clean.url <- NULL
allinks <- NULL
yelplinks <- NULL

#Find Embedded Yelp Link of the restaurant from the Google Search Result  
for(i in 1:nrow(searchquery))
{
  restaurant.url <- searchquery[i,1]
  dirty.url <- getURL(restaurant.url,
                      .opts = list(proxy='http://19.12.2.105:83'),
                      curl = getCurlHandle())
  all.html <- dirty.url
  
  clean.url <- genXtract(all.html, 'url?q=', '&amp;sa=U&amp;')
  names(clean.url) <- NULL
  allinks <- c(allinks, clean.url[1])
  print(clean.url[1])
  yelplinks <- rbind(clean.url[1], yelplinks)
  clean.url <- NULL
}

yelplinks <- as.data.frame(yelplinks)

# Here u need to omit NAs i.e. Yelp reviews that don't exist and create a final Dataframe
# yelplinks which will have the valid Yelp link in its 1st column

yelplinks[,1] <- as.character(yelplinks[,1])
df <- data.frame("Reviews"= character(0), "Ratings"= character(0), "Date" = character(0))
for(i in 1:nrow(yelplinks))
{
  all.html <- getURL(yelplinks[i,1],
                     .opts = list(proxy='http://19.12.2.105:83'),
                     curl = getCurlHandle())
  nopg.find <- as.numeric(regexpr("Page 1 of ", all.html)) #Search for total pages
  total.pages <- as.numeric(substr(all.html, nopg.find+10, nopg.find+11))
  print(total.pages)
  if(total.pages!=1){ 
    total.pages <- total.pages-1
    lastpage <- total.pages*20 # find the last ?start= n
  }else{
    lastpage <- 20 # if its Page 1 of 1 then no need of ?start=20, 40, . . . . . 
  }
  page.start <- seq(from = 20, to = lastpage, by = 20) #creating a sequence of 20 Reviews/page
  mainq <- as.character(yelplinks[i,1]) # the 1st page search query
  q <- as.character(yelplinks[i,1]) #copy of the 1st page N.B. this will get updated to 20,40
  for(j in 1:length(page.start))
  {
    
    data.html <- getURL(q,
                        .opts = list(proxy='http://19.12.2.105:83'),
                        curl = getCurlHandle())
    reviews <- genXtract(data.html, '<p itemprop=\"description\" lang=\"en\">','</p>')
    names(reviews) <- NULL
    reviews <- as.character(reviews) #extracting all Reviews at current page
    
    dates <- genXtract(data.html, '<meta itemprop=\"datePublished\" content=\"','\">')
    names(dates) <- NULL
    dates<- as.character(dates) #extracting the associated Dates of the Reviews at current pg
    
    ratings <- genXtract(data.html, '<meta itemprop=\"ratingValue\" content=\"' ,'.0\">')
    names(ratings) <- NULL
    ratings <- as.character(ratings) #extracting the associated Dates of the Reviews at current pg
    ratings <- ratings[-1] #The 1st Ratings will be the overall Review of the restaurant itself
    
    tempdf <- data.frame(cbind(reviews, ratings, dates)) #create a df of the data extracted
    
    df <- rbind(df, tempdf) # append it with the old data
    tempdf <- data.frame()
    
    q <- NULL
    q <- paste(mainq, "?start=", as.character(page.start[j]), sep = "") # creating next page search query
    print(q) # print the new query
    reviews <- NULL
    ratings <- NULL
    dates <- NULL
  }
}