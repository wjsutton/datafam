
# A deleted user broke my script to get_friends() for a list of users

# Ran into issue, user deleted account after tweeting
# userid: 1282903872311197696 
# screenname: Vignesh15317680
# which then breaks script
# testing options before running get_friends() 
# 1. lookup_users()
# 2. as_screenname / as_userid()

# Workings 

library(rtweet)
library(dplyr)

datafam <- readRDS("data/#datafam_tweets.RDS")
all_users <- unique(datafam[,c('user_id','screen_name')])
deleted_user <- all_users%>% filter(user_id == 1282903872311197696)

# Lookup returns empty dataframe
lookup_users(deleted_user$screen_name)

# returns all_users with non deletes
lookup_users(all_users$screen_name)[,c("user_id","screen_name")]

