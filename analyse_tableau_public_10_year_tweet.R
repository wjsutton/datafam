# Tableau Public 10 year anniversary tweet
# Shares of twitter accounts to follow and be inspired by

library(rtweet)
library(dplyr)

tweet_url <- 'https://twitter.com/tableaupublic/status/1313570750960267264?s=20'
tbp_10yr_status <- '1313570750960267264'
tbp_tweet_data <- lookup_tweets(1313570750960267264)

# Can't look up replies to tweets easily, so search tweets to tbp
tweets_to_tbp <- search_tweets(
  "@tableaupublic", 
  n = 10000,
  retryonratelimit = TRUE
)

replys_to_tbp_10yr <- filter(tweets_to_tbp,reply_to_status_id=='1313570750960267264')
saveRDS(replys_to_tbp_10yr,"data/tableau_public_10yr_anniversary_tweet_replies.RDS")

replies <- readRDS("data/tableau_public_10yr_anniversary_tweet_replies.RDS")

accounts_mentioned <- replies$mentions_screen_name
all_accounts <- unlist(accounts_mentioned)
all_user_ids <- unique(unlist(replies$mentions_user_id))

# Seeing top mentioned accounts
#data <- data.frame(acc=all_accounts,stringsAsFactors = F)
#data %>% count(acc, sort = TRUE) 

accounts_i_follow <- get_friends('wjsutton12')

accounts_mentioned_i_dont_follow <- all_user_ids[!(all_user_ids %in% accounts_i_follow$user_id)]
accs_to_follow <- lookup_users(accounts_mentioned_i_dont_follow)

# Test with 1 account
# currently following 390
# post_follow(accounts_mentioned_i_dont_follow[1])
# Worked! Now following 391

for(i in seq(accounts_mentioned_i_dont_follow)){
  print(i)
  post_follow(accounts_mentioned_i_dont_follow[i])
  print(paste0(accs_to_follow$screen_name[i]," followed!"))
  Sys.sleep(3)
}


