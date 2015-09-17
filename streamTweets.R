library(streamR)
library(dplyr)
load("my_oauth.Rdata")

sampleStream("three_minute_sample.json", timeout = 180, oauth = my_oauth)
three_minute_sample_df <- parseTweets("three_minute_sample.json")

# grabbing english only tweets
english_tweets <- three_minute_sample_df %>%
  filter(lang == "en")

save(english_tweets, file =  "three_min_streamSample.Rdata")

sampleStream("one_minute.json", timeout = 60, oauth = my_oauth)

filterStream("IStandWithAhmed.json", timeout = 600, track = "IStandWithAhmed", oauth = my_oauth)
Ahmed <- parseTweets("IStandWithAhmed.json")
