library(qdap)
library(dplyr)
load("three_min_streamSample.Rdata")

# cleaning function
clean_tweets <- function(tweets){
  # cleans the raw parsedTweet dataframe and reduces it to useful information
  #
  #arg:
  #   tweets - data frame from parseTweets function (filtered to english only)
  #
  #returns:
  #   a data frame with clean text and TODO!!!!!
  
  # removal of useful information from tweet text before cleaning
  tweets$hashtags_in_tweet <- rm_hash(tweets$text, extract = TRUE)
  #combine twitter url and base url removal to one step
  rm_twitter_n_url <- rm_(pattern = pastex("@rm_twitter_url", "@rm_url"))
  tweets$url_in_tweet <- rm_twitter_n_url(tweets$text, extract = TRUE)
  tweets$retweet_from <- rm_between(tweets$text, "RT", ":", extract = TRUE)
  
  
  # Cleaning of tweet text
  tweets$text <- rm_hash(tweets$text)
  tweets$text <- rm_twitter_n_url(tweets$text)
  tweets$text <- rm_non_ascii(tweets$text)
  tweets$text <- rm_between(tweets$text, "RT", ":")
  tweets$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+):", "", tweets$text)
  tweets$text <- gsub("@\\w+", "", tweets$text)
  tweets$text <- L(tweets$text)
  tweets$text <- replace_number(tweets$text)
  tweets$text <- gsub("&amp", "and", tweets$text)
  tweets$text <- rm_non_words(tweets$text)
  tweets$text <- rm_white(tweets$text)
  
  
  # grabbing the platform used for each tweet
  platform <- rm_between(tweets$source, ">", "</a>", extract = TRUE)
  tweets$platform_used <- lapply(strsplit(unlist(platform), split = " "), tail, 1)
  
  # +0000 indicates GMT, remove that first
  tweets$created_at <- gsub("\\+0000", "", tweets$created_at)
  tweets$created_at <- parse_date_time(tweets$created_at, orders = "md hms y")
  
  final_tweet_df <- tweets %>%
                        select(text, retweet_count, in_reply_to_screen_name, created_at,
                               listed_count, location, description, geo_enabled, user_created_at,
                               statuses_count, followers_count, favourites_count, time_zone, 
                               utc_offset, friends_count, screen_name, full_name, 
                               place_lat, place_lon, hashtags_in_tweet, url_in_tweet,
                               retweet_from, platform_used)
  
  final_tweet_df
}




library(tm)
myCorpus <- Corpus(VectorSource(english_tweets$text))

myCorpus <- tm_map(myCorpus, content_transformer(tolower))

tmp <- rm_(pattern = S("@after_", "\\@", 1), extract = TRUE)
