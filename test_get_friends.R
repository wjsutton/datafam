library(rtweet)
library(dplyr)
library(jsonlite)

datafam <- readRDS("data/#datafam_tweets.RDS")


all_users <- unique(datafam[,c('user_id','screen_name')])

for(i in 1:nrow(all_users)){
  
  friends <- rtweet::get_friends(all_users$screen_name[i])
  friends <- dplyr::inner_join(friends, all_users, by = c("user_id" = "user_id"))
  
  if(i == 1){
    all_friends <- friends
  }
  
  if(i != 1){
    all_friends <- rbind(all_friends,friends)
  }
  
}

output <- all_friends[,c('user','screen_name')]
output$user <- paste0("@",output$user)
output$screen_name <- paste0("@",output$screen_name)
names(output) <- c('source','target')

output_json <- toJSON(output, pretty = FALSE)
write(output_json, "data/datafam_follows.json")

# Can get friends for multiple users, 
# takes a long time due to 15min rate limit
test <- rtweet::get_friends(all_users$screen_name[1:5], retryonratelimit = TRUE)

# Ran into issue, user deleted account after tweeting
# userid: 1282903872311197696 
# screenname: Vignesh15317680
# which then breaks script
# testing options before running get_friends() 
# 1. lookup_users()
# 2. as_screenname / as_userid()

