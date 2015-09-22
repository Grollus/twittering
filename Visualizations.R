library(ggplot2)

clean_df$platform_used <- unlist(clean_df$platform_used)

platform <- clean_df %>%
  count(unlist(platform_used))

platforms <- clean_df %>%
  mutate(platform_used = ifelse(!(platform_used %in% c("twitterfeed", "TweetDeck", "Phone",
                                                   "iPhone", "iPad", "iOS", "Instagram",
                                                   "IFTTT", "Google", "fllwrs", 
                                                   "Facebook", "Echofon", "dlvr.it",
                                                   "Client", "Android", "(M5)")), 
                                 "Other", clean_df$platform_used))
  

ggplot(data = platforms, aes(x = platform_used))+
  geom_histogram() +
  coord_flip()


