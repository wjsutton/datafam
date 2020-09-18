# Get data from Twitter

library(rtweet)
library(dplyr)
library(jsonlite)

source("functions/function_get_and_save_tweets.R")
source("functions/function_get_tableau_profile_url.R")
source("functions/function_get_tableau_public_api_extract.R")

# Get data
get_and_save_tweets(text = "#tableau", count = 10000, path = "data")
get_and_save_tweets(text = "#datafam", count = 10000, path = "data")
get_and_save_tweets(text = "#makeovermonday", count = 10000, path = "data")

# Read and merge data
tableau <- readRDS("data/#tableau_tweets.RDS")
datafam <- readRDS("data/#datafam_tweets.RDS")
makeovermonday <- readRDS("data/#makeovermonday_tweets.RDS")

all_tweets <- rbind(tableau,datafam,makeovermonday)

all_links <- unlist(all_tweets$urls_expanded_url)

# remove NAs
all_links <- all_links[!is.na(all_links)]

# find tableau links
all_links <- all_links[grepl('public.tableau',all_links)]

profiles <- get_tableau_profile_url(all_links)

profile_urls <- unique(profiles$profile)

profile_name <- ifelse(substr(profile_urls,nchar(profile_urls)-2,nchar(profile_urls))=='#!/',
  substr(profile_urls,36,nchar(profile_urls)-3),
  substr(profile_urls,36,nchar(profile_urls)))

profile_name <- unique(profile_name[!is.na(profile_name)])

for(i in seq(profile_name)){
  profile_df <- get_tableau_profile_api_extract(profile_name[i])
  print(i)
  if(i == 1){
    df <- profile_df
  }
  if(i >= 1){
    df <- rbind(df,profile_df)
  }
}
df <- unique(df)
write.csv(df,'tableau_ff.csv',row.names = F)

source("functions/function_get_last_n_tableau_vizs_extract.R")
source("functions/function_get_tableau_viz_screenshot_url.R")

latest_viz <- filter(df,visible_workbooks>0)

for(i in 1:nrow(latest_viz)){
  print(i)
  viz <- get_last_n_tableau_vizs_extract(profile_name = latest_viz$name[i],n =5)
  
  if(i == 1){
    viz_df <- viz
  }
  if(i != 1){
    viz_df <- rbind(viz_df,viz)
  }
}

#get_tableau_viz_screenshot_url(viz_df$workbooks[1])

for(i in 1:length(viz_df$workbooks)){
  viz_b <- viz_df$workbooks[i]
  
  # Case 2 'profile vizhome'
  if(!is.na(str_locate(pattern="vizhome",viz_b)[1])){
    
    # Start of "?"
    viz_b <- gsub("\\?publish=yes","",viz_b)
    
    # Start of profile
    profile_start_pos <- str_locate_all(pattern="profile",viz_b)[[1]][1,1]
    
    # End of "vizhome"
    vizhome_end_pos <- str_locate_all(pattern="vizhome",viz_b)[[1]][1,2]
    two_letters_of_dash <- substr(viz_b,vizhome_end_pos+2,vizhome_end_pos+3)
    
    knit <- paste0(substr(viz_b,1,profile_start_pos-1)
                   ,'static/images/',two_letters_of_dash,'/'
                   ,substr(viz_b,vizhome_end_pos+2,nchar(viz_b))
                   ,'/1.png')
  }
  if(i == 1){
    imgs <- knit
  }
  if(i != 1){
    imgs <- c(imgs,knit)
  }
}

viz_df$screenshot <- imgs
df$profile_name <- df$name
merged <- left_join(viz_df,df[,c("profile_name","profile_url",'followers')], by = ("profile_name" = "profile_name"))

merged$img_and_profile_link <- paste0("<a href='",merged$profile_url,"'>",
                                        "<img src='",merged$screenshot,"'></a>")

write.csv(merged,'tableau_ff_v2.csv',row.names = F)

profile_call <- paste0('https://public.tableau.com/profile/api/',profile_name[i])

profile_data <- jsonlite::fromJSON(profile_call)

profile_following <- profile_data$totalNumberOfFollowing
profile_followers <- profile_data$totalNumberOfFollowers

profile_twitter <- profile_data$websites$url[grepl('twitter',profile_data$websites$url)]
profile_linkedin <- profile_data$websites$url[grepl('linkedin',profile_data$websites$url)]

profile_last_publish <- profile_data$lastPublishDate
profile_visible_workbooks <- profile_data$visibleWorkbookCount

profile_following <- ifelse(length(profile_following)==1,profile_following,0)
profile_followers <- ifelse(length(profile_followers)==1,profile_followers,0)

profile_twitter <- ifelse(length(profile_twitter)==1,profile_twitter,'')
profile_linkedin <- ifelse(length(profile_linkedin)==1,profile_linkedin,'')

profile_last_publish <- ifelse(length(profile_last_publish)==1,profile_last_publish,0)
profile_visible_workbooks <- ifelse(length(profile_visible_workbooks)==1,profile_visible_workbooks,0)

profile_df <- data.frame(name=profile_name[i],
                         profile_url=paste0('https://public.tableau.com/profile/',profile_name[i],'#!/'),
                         api_call=profile_call,
                         followers=profile_followers,
                         following=profile_following,
                         twitter=profile_twitter,
                         linkedin=profile_linkedin,
                         last_publish=profile_last_publish,
                         visible_workbooks=profile_visible_workbooks,
                         stringsAsFactors = F)

