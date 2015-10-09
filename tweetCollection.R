source("streamTweets.R")

# Defining emoticons and gathering data for training model
positive_emoticons <- c(":)", ":-)", ": )", ":D", "=)")
negative_emoticons <- c(":(", ":-(", ": (")

filterStream("positive_emoticon_tweets.json", track = positive_emoticons, language = "en",
             tweets = 100000, oauth = my_oauth)
positive_emoticon_tweets <- parseTweets("positive_emoticon_tweets.json")

filterStream("negative_emoticon_tweets.json", track = negative_emoticons, language = "en",
             tweets = 500000, oauth = my_oauth)
negative_emoticon_tweets <- parseTweets("negative_emoticon_tweets.json")
