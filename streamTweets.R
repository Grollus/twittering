library(streamR)
library(dplyr)
load("my_oauth.Rdata")

collect_tweets <- function(filename, track = NULL, time = 60, tweets = NULL,
                           language = "en"){
  # Basic function to utilize streamR to gather tweets according to parameters
  # Defaults to streaming tweets using twitter's streaming api for 60 seconds
  # 
  # args:
  #   track - a string or string vector of keywords to track.
  #   time - amount of time to keep api connection open
  #   tweets - if specified, the number of tweets you want to collect
  #
  # returns:
  #   a data frame with raw data from twitter api
  
  if(is.null(track)){
    sampleStream(paste0(filename, ".json"), timeout = time, tweets = tweets, 
                 oauth = my_oauth)
    tweet_df <- parseTweets(paste0(filename, ".json"))
    
  }else{
    filterStream(paste0(filename, ".json"), track = track, timeout = time,
                 tweets = tweets, language = language, oauth = my_oauth)
    tweet_df <- parseTweets(paste0(filename, ".json"))
  }

  save(tweet_df, file = paste0(filename, ".Rdata"))
  return(tweet_df)
}

