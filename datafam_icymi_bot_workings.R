# Tableau ICYMI
# Posts from the Tableau community that you may have missed

# Aim
# Twitter posts that are a few days old
# From users with low number of followers
# not are not spam
# i.e. offering training discounts

# To Do

# merge tableau & datafam tweets
# determine ranking: popularity matched with followers
# possible calc of reach?
# identify and filter out spam accounts
# identify bot accounts: https://github.com/mkearney/tweetbotornot, https://mikewk.shinyapps.io/botornot/
# read: http://www.seancase.net/posts/rtweet-twitterbots-2/
# Look at tone or sentiment of content?
# Google cloud NLP?
# expand urls with: https://github.com/hrbrmstr/longurl

options(stringsAsFactors=FALSE)

# Load libraries
library(rtweet)
library(stringr)
suppressMessages(library(dplyr))
suppressMessages(library(gmailr))

date_from <- Sys.Date()-6
date_to <- Sys.Date()

## search for 18000 tweets using the rstats hashtag
rt <- search_tweets(
  "#tableau", n = 1000, include_rts = FALSE
)

rt2 <- search_tweets(
  "#datafam", n = 1000, include_rts = FALSE
)

rt3 <- search_tweets(
  "#rstats", n = 1000, include_rts = FALSE
)

#tableau
#datafam
#ironviz
#tableaucommunity
#tableaupublic
#tableautiptuesday
#makeovermonday
#rstats
#WOW2020


all_tweets <- unique(rbind(rt,rt2))
all_tweets <- rt2
all_tweets <- rt3

earliest_tweet_time <- min(all_tweets$created_at)
latest_tweet_time <- max(all_tweets$created_at)

num_accounts_tweeting <- length(unique(all_tweets$screen_name))

all_tweets$user_tweets_per_day <- all_tweets$statuses_count/as.integer(Sys.Date() - as.Date(all_tweets$account_created_at))

all_tweets$engagement <- all_tweets$favorite_count + all_tweets$retweet_count

# filter out replies
all_tweets_no_replies <- all_tweets %>% filter(is.na(reply_to_status_id) == TRUE)

all_tweets_just_replies <- all_tweets %>% filter(is.na(reply_to_status_id) == FALSE)

tweeters <- unique(all_tweets_no_replies$screen_name)

flw <- vector("list", length(tweeters))
n <- 75000
# if tweets above 5000
#for (i in seq_along(flw)) {
#for (i in 1:5) {
#  flw[[i]] <- get_followers(tweeters[i], n = n)
#  Sys.sleep(60 * 15)
#}


# Identifying spammmers

all_tweets_no_replies$number_of_hashtags <- lengths(all_tweets_no_replies$hashtags)

tweets_per_account <- all_tweets_no_replies %>% group_by(screen_name) %>% tally() %>% arrange(-n)

community_user_stats <- all_tweets_no_replies %>% 
  group_by(screen_name) %>% 
  summarise(avg_engagement = mean(engagement, na.rm = TRUE)
            ,avg_hashtags = mean(number_of_hashtags, na.rm = TRUE)
            ,avg_following = mean(friends_count, na.rm = TRUE)
            ,avg_followers = mean(followers_count, na.rm = TRUE)
            ,community_tweets = n()
            ,user_tweets_per_day = mean(user_tweets_per_day, na.rm = TRUE)) %>% 
  arrange(-avg_engagement)
# plot on log scale


# example text
(all_tweets_no_replies %>% filter(screen_name == 'couponfree01'))$text

high_hashtags <- community_user_stats %>% filter(avg_hashtags>=8)


test <- all_tweets_no_replies %>% inner_join(high_hashtags, by = c("screen_name" = "screen_name"))
test$text

test2 <- all_tweets_no_replies %>% 
  anti_join(high_hashtags, by = c("screen_name" = "screen_name"))


head(rt$text)
head(rt$favorite_count)
head(rt$retweet_count)

# Followers
test$followers_count
# Following
test$friends_count

rt$popularity <- rt$favorite_count + rt$retweet_count

dataset <- rt

test <- dataset %>% filter(as.Date((dataset$created_at))==Sys.Date()-3)

mean <- mean(test$popularity)
standard_dev <- sd(test$popularity)

library(SmartEDA)

count <- test %>% 
  count(screen_name)
