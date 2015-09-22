library(qdap)
library(stringr)
# qdap actually comes with positive and negative word list included, so we don't need 
# to read it in from a separate file as below
# just call positive.words/negative.words

# pos <- read.table("positive-words.txt", skip = 35)
# neg <- read.table("negative-words.txt", skip = 35)

sentiment_score <- function(tweets, pos.words, neg.words, .progress = 'none'){
  #args:
  #  tweets - vector of text to score
  #  pos.words - vector of positive sentiment words
  #  neg.words - vector of negative sentiment words
  #  .progress - passed to laply() to control progress bar
  #
  #returns:
  #  a data frame with sentiment scores for each tweet
  
  scores <- laply(tweets, function(tweet, pos.words, neg.words){
    word.list <- str_split(tweet, " ")
    words <- unlist(word.list)
    pos.match <- match(words, pos.words)
    neg.match <- match(words, neg.words)
    pos.match <- !is.na(pos.match)
    neg.match <- !is.na(neg.match)
    
    score <- sum(pos.match) - sum(neg.match)
    return(score)
  }, pos.words, neg.words, .progress = .progress)
  scores.df <- data.frame(score = scores, text = tweets)
  return(scores.df)
  
}
scores <- sentiment_score(test2$text, positive.words, negative.words, .progress = 'text')

# rating very positive and negative tweets
scores$very.pos <- as.numeric(scores$score >= 2)
scores$very.neg <- as.numeric(scores$score <= -2)

numpos <- sum(scores$very.pos)
numneg <- sum(scores$very.neg)
# global score
global_score <- round(100 * numpos / (numpos + numneg))

# total score calculation: positive / negative / neutral
stat <- scores
stat$created <- test2$created_at
stat$created <- as.Date(stat$created)
stat <- mutate(stat, tweet = ifelse(stat$score > 0, "positive",
                                    ifelse(stat$score < 0, "negative", "neutral")))
by.tweet <- group_by(stat, tweet, created)
by.tweet <- summarise(by.tweet, number = n())



####################################################################################
# Sentiment using scored word list rather than just positive and negative lists
####################################################################################
twitter_word_list <- read.csv("twitter_sentiment_list.csv", quote = "")
sentiment_score_2 <- function(tweets, words, .progress = 'none'){
  
  counts <- laply(tweets, function(tweet, words){
    word.list <- str_split(tweet, " ")
    words <- unlist(word.list)
    matches <- match(words, twitter_word_list$word)
    matches <- !is.na(matches)
    
    count <- sum(matches)
    return(count)
  }, words, .progress = .progress)
  scores.df <- data.frame(count = counts, text = tweets)
  return(scores.df)
}

scores <- sentiment_score_2(test2$text, twitter_word_list, .progress = 'text')
