library(dplyr)
library(longurl)
library(stringr)

ironviz <- readRDS("data/#ironviz_tweets.RDS")

# Testing Regex
urls <- unlist(ironviz$urls_expanded_url)

# Case 0: not NA
urls <- urls[!is.na(urls)]

# Expand short urls
short_urls <- urls[nchar(urls)<35]
expanded_urls <- (longurl::expand_urls(short_urls))

url_df <- data.frame(url = urls)
url_df <- dplyr::left_join(url_df,expanded_urls, by = c("url" = "orig_url"))

urls <- ifelse(!is.na(url_df$expanded_url),url_df$expanded_url,url_df$url)

# Case 1: https://public.tableau.com/views/IronViz2020EveryMotherCounts-ExploringMaternalHealth/EveryMotherCounts?:language=en-GB&:display_count=y&:origin=viz_share_link
pattern1 <- "^https://public\\.tableau\\.com/views(?:/.*)?$"
case_1_urls <- urls[grep(pattern1,urls)]

# Case 2: https://public.tableau.com/profile/agata1619#!/vizhome/Whattimearebabiesborn/Whattimearebabiesborn?publish=yes
pattern2a <- "^https://public\\.tableau\\.com/profile(?:/.*)?$"
pattern2b <- "^(.*)vizhome(.*)$"
first_pass <- urls[grep(pattern2a,urls)]
second_pass <- first_pass[grep(pattern2b,first_pass)]

case_2_urls <- second_pass

remaining_urls <- subset(urls, !(urls %in% c(case_1_urls,case_2_urls)))
ironviz_urls <- unique(c(case_1_urls,case_2_urls))


# 'https://public.tableau.com/views/TheImportanceofSleep-IronViz2020mobileapp/Home?:language=en&:display_count=y&publish=yes&:origin=viz_share_link'



# Two types of Tableau links
# 1. 'views'
# 2. 'profile vizhome'

for(i in 1:length(ironviz_urls)){
  viz <- ironviz_urls[i]
  
  # Case 1: views
  if(!is.na(str_locate(pattern="views",viz)[1])){
    
    # Start of "?:"
    position <- (str_locate_all(pattern="\\?:",viz))[[1]][1,1]
    viz <- substr(viz,1,position-1)
    
    # End of "views"
    views_end_pos <- str_locate_all(pattern="views",viz)[[1]][1,2]
    two_letters_of_dash <- substr(viz,views_end_pos+2,views_end_pos+3)
    
    # Start of "views"
    views_start_pos <- str_locate_all(pattern="views",viz)[[1]][1,1]
    
    knit <- paste0(substr(viz,1,views_start_pos-1)
                   ,'static/images/',two_letters_of_dash,'/'
                   ,substr(viz,views_end_pos+2,nchar(viz))
                   ,'/1.png')
    
  }
  
  # Case 2 'profile vizhome'
  if(!is.na(str_locate(pattern="vizhome",viz)[1])){
    
    # Start of "?"
    viz <- gsub("\\?publish=yes","",viz)
    
    # Start of profile
    profile_start_pos <- str_locate_all(pattern="profile",viz)[[1]][1,1]
  
    # End of "vizhome"
    vizhome_end_pos <- str_locate_all(pattern="vizhome",viz)[[1]][1,2]
    two_letters_of_dash <- substr(viz,vizhome_end_pos+2,vizhome_end_pos+3)
    
    knit <- paste0(substr(viz,1,profile_start_pos-1)
                   ,'static/images/',two_letters_of_dash,'/'
                   ,substr(viz,vizhome_end_pos+2,nchar(viz))
                   ,'/1.png')
      
  }
  
  #img <- paste0("<img src='",gsub('/','&#47;',knit),"'>")
  img <- paste0("<img src='",knit,"'>")
  
  
  if(i == 1){
    img_list <- img
  }
  
  if(i > 1){
    img_list <- c(img_list,img)
  }
  img_list <- unique(img_list)
}


# Write list of images to a text file for now
fileConn <- file("img_list.txt")
writeLines(img_list, fileConn)
close(fileConn)


