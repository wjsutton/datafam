library(rtweet)
library(dplyr)
library(jsonlite)

# Read in dataset of tweets from a hashtag
datafam <- readRDS("data/#datafam_tweets.RDS")

# Reduce dataset to user_id and handles
all_users <- unique(datafam[,c('user_id','screen_name')])

# Removing deleted accounts
all_users <- lookup_users(all_users$screen_name)[,c("user_id","screen_name")]

# Get lists of friends and joining back to all_users
# This section takes a long time due to a 15 minute rate limit every 15 users
friends <- rtweet::get_friends(all_users$screen_name, retryonratelimit = TRUE)
friends <- dplyr::inner_join(friends, all_users, by = c("user_id" = "user_id"))

# Shaping output data.frame
output <- friends[,c('user','screen_name')]
output$user <- paste0("@",output$user)
output$screen_name <- paste0("@",output$screen_name)
names(output) <- c('source','target')

# Converting output data.frame into json and writing locally
output_json <- toJSON(output, pretty = FALSE)
write(output_json, "data/datafam_follows.json")
