# Load libraries
library(rtweet)
library(dplyr)
library(longurl)
library(stringr)
library(tidyr)

#followers <- rtweet::get_followers('wjsutton12')
#following <- rtweet::get_friends('wjsutton12')

# unpack links to tweets 


timelines <- rtweet::get_my_timeline(n = 10000)

timelines <- readRDS('data/#ironviz_tweets.RDS')

df <- timelines[,c('screen_name','urls_expanded_url'),]
df <- unnest(df,urls_expanded_url)

# Case 0: not NA
df <- filter(df,!is.na(urls_expanded_url))

# Expand short urls
short_urls <- df$urls_expanded_url[nchar(df$urls_expanded_url)<35]
expanded_urls <- unique((longurl::expand_urls(short_urls)))

# Join longurls back to original dataset and replace if a longurl is available
url_df <- dplyr::left_join(df,expanded_urls, by = c("urls_expanded_url" = "orig_url"))
df$urls <- ifelse(!is.na(url_df$expanded_url),url_df$expanded_url,url_df$urls_expanded_url)

# Find status id
test <- remaining_urls[1:30]
grep("^https://twitter\\.com/[.]*/status/[//d]*",test)
tweet_status <- "^https://twitter\\.com/.*/status/\\d+$"
test2 <- test[grep(tweet_status,test)]
substr(test2,str_locate(test2,'status')[,2]+2,nchar(test2))

#df <- df %>% filter(nchar(urls)>=35)

#df$urls <- df$urls_expanded_url

# Case 1: https://public.tableau.com/views/IronViz2020EveryMotherCounts-ExploringMaternalHealth/EveryMotherCounts?:language=en-GB&:display_count=y&:origin=viz_share_link
pattern1 <- "^https://public\\.tableau\\.com/views(?:/.*)?$"
case_1_urls <- df$urls[grep(pattern1,df$urls)]

library(rvest)
library(httr)
#library(RCurl)
for(i in 1:length(case_1_urls)){
#for(i in 1:5){
  print(i)
  tableau_profile <- case_1_urls[i]

  if(http_status(GET(tableau_profile))$category=='Success'){
    
    tableau_profile_html <- read_html(tableau_profile)
    profile <- paste0(str_extract(as.character(tableau_profile_html),'https://public\\.tableau\\.com/profile/[A-z|0-9|\\.|-]*'),'#!/')
    
  }
  
  if(http_status(GET(tableau_profile))$category!='Success'){
    profile <- ''
  }
  
  if(i == 1){
    case_1_profiles <- c(profile)
  }
  
  if(i != 1){
    case_1_profiles <- c(case_1_profiles,profile)
  }
  
}


# Case 2: https://public.tableau.com/profile/agata1619#!/vizhome/Whattimearebabiesborn/Whattimearebabiesborn?publish=yes
pattern2a <- 'https://public\\.tableau\\.com/profile/[A-z|0-9|\\.|-]*'
#pattern2b <- "^(.*)vizhome(.*)$"
first_pass <- df$urls[grep(pattern2a,df$urls)]
#second_pass <- first_pass[grep(pattern2b,first_pass)]

case_2_profiles <- substr(first_pass,1,str_locate(first_pass,'vizhome')[,1]-1)
case_2_urls <- first_pass

profile_urls <- unique(c(case_1_profiles,case_2_profiles))

remaining_urls <- subset(df$urls, !(df$urls %in% c(case_1_urls,case_2_urls)))
tableau_urls <- unique(c(case_1_urls,case_2_urls))

#remaining_df <- filter(df, df$urls %in% remaining_urls)
#ironviz_df <- filter(df,df$urls %in% ironviz_urls)

