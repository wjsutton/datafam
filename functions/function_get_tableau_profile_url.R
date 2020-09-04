library(longurl)
library(stringr)
library(rvest)
library(httr)
library(dplyr)

get_tableau_profile_url <- function(urls){
  
  df <- data.frame(original_url=urls, stringsAsFactors = FALSE)
  
  # Case A: https://public.tableau.com/views/IronViz2020EveryMotherCounts-ExploringMaternalHealth/EveryMotherCounts?:language=en-GB&:display_count=y&:origin=viz_share_link
  pattern_a <- "https://public\\.tableau\\.com/views(?:/.*)?"
  
  df$case_a_urls <- ifelse(grepl(pattern_a,df$original_url),df$original_url,NA)
  
  # Case B: https://public.tableau.com/profile/agata1619#!/vizhome/Whattimearebabiesborn/Whattimearebabiesborn?publish=yes
  pattern_b <- 'https://public\\.tableau\\.com/profile/[A-z|0-9|\\.|-]*'
  
  df$case_b_urls <- ifelse(grepl(pattern_b,df$original_url),df$original_url,NA)
  
  
  for(i in 1:nrow(df)){
  #for(i in 1:5){
   # print(i)
    #tableau_profile <- df$case_a_urls[i]
    
    if(is.na(df$case_a_urls[i])){
      case_a_profile <- NA
    }
      
    if(!is.na(df$case_a_urls[i])){
      status <- http_status(GET(df$case_a_urls[i]))$category
      case_a_profile <- ifelse(status!='Success',NA,
                               paste0(str_extract(as.character(read_html(df$case_a_urls[i])),'https://public\\.tableau\\.com/profile/[A-z|0-9|\\.|-]*'),'#!/'))
    }
    
    # building list of profiles from case A
    if(i == 1){
      case_a_profiles <- c(case_a_profile)
    }
    
    if(i != 1){
      case_a_profiles <- c(case_a_profiles,case_a_profile)
    }
    
  }
  df$case_a_profiles <- case_a_profiles
  
  # check for viz home
  pattern <- "^(.*)vizhome(.*)$"
  case_when(
    is.na(df$case_b_urls) ~ NA,
    
  )
  df$case_b_urls
  
  
  
  # Case 2: https://public.tableau.com/profile/agata1619#!/vizhome/Whattimearebabiesborn/Whattimearebabiesborn?publish=yes
  pattern2a <- 'https://public\\.tableau\\.com/profile/[A-z|0-9|\\.|-]*'
  
  first_pass <- df$urls[grep(pattern2a,df$urls)]
  #second_pass <- first_pass[grep(pattern2b,first_pass)]
  
  case_2_profiles <- substr(first_pass,1,str_locate(first_pass,'vizhome')[,1]-1)
  case_2_urls <- first_pass
  
  profile_urls <- unique(c(case_1_profiles,case_2_profiles))
  
  
  
  
}