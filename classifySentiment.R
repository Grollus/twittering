library(stringr)

full_word_list <- read.csv("twitter_sentiment_list.csv", quote = "")
happy_log_probs <- full_word_list[, 1:2]
sad_log_probs <- full_word_list[, c(1, 3)]


`%fin%` <- function(x, table){
  stopifnot(require(fastmatch))
  fmatch(x, table, nomatch = 0L) > 0L
}

classifySentiment_fast <- function(tweet, happy_log_probs = happy_log_probs, sad_log_probs = sad_log_probs){
  
  words <- unlist(str_split(tweet, " "), use.names = FALSE)
  
  happy_probs <- unlist(lapply(words, function(x) if(x %fin% happy_log_probs$word){happy_log_probs$happy[happy_log_probs$word == x]}))
  sad_probs <- unlist(lapply(words, function(x) if(x %fin% sad_log_probs$word){sad_log_probs$sad[sad_log_probs$word == x]}))
  
  tweet_happy_log_prob <- sum(happy_probs)
  tweet_sad_log_prob <- sum(sad_probs)
  prob_happy <- 1/(exp(tweet_sad_log_prob - tweet_happy_log_prob) + 1)
  prob_sad <- 1 - prob_happy
  
  return(c(prob_happy, prob_sad))
}

results <- lapply(clean_pet$text,function(x) classifySentiment_fast(x, happy_log_probs, sad_log_probs))
results_n <- lapply(clean_net$text, function(x) classifySentiment_fast(x, happy_log_probs, sad_log_probs))

probValues <- function(tweet_df, predicted_prob_list){
  tweet_df$prob_happy <- unlist(lapply(predicted_prob_list, function(x) x[[1]][1]))
  tweet_df$prob_sad <- unlist(lapply(predicted_prob_list, function(x) x[[1]][2]))
  return(tweet_df)
}

net_temp <- probValues(net_temp, results_n)
