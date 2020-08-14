library(rtweet)
library(dplyr)
library(longurl)
library(stringr)
library(tidyr)

# Load data
ironviz <- readRDS("data/#ironviz_tweets.RDS")
ironviz2020 <- readRDS("data/#ironviz2020_tweets.RDS")
ironviz <- unique(rbind(ironviz,ironviz2020))

# Tweet is sharing a viz not a submission 
ironviz <- ironviz %>% filter(status_id != '1286048757071646720')
ironviz <- ironviz %>% filter(status_id != '1291346281508503554')
ironviz <- ironviz %>% filter(status_id != '1291267791253835776')
ironviz <- ironviz %>% filter(status_id != '1291183057572179968')
ironviz <- ironviz %>% filter(status_id != '1291176574499328001')

#ironviz %>% filter(status_id == '1290102953924124672')

#missing <- lookup_tweets(c('1290939514177937408','1290952501508943872','1290908538349342721'))
#ironviz <- rbind(ironviz,missing)
#saveRDS(ironviz,"data/#ironviz_tweets.RDS")

# Flatten list in data.frame
df <- ironviz[,c('status_id','screen_name','urls_expanded_url')]
df <- unnest(df,urls_expanded_url)

# fixing links
df$urls_expanded_url <- gsub('^https://public.tableau.com/profile/aashique.s#!/$'
                             ,'https://public.tableau.com/profile/aashique.s#!/vizhome/LifeofaSickleCellWarrior/LifeofaSICKLECELLWARRIOR'
                             ,df$urls_expanded_url)

df$urls_expanded_url <- gsub('^http://shorturl.at/fGLMS$'
                            ,'https://public.tableau.com/views/LEADINGCAUSESOFDEATH-ABORIGINALANDTORRESSTRAITISLANDERVS_NON-INDIGENOUS/Dashboard1?:language=en&:display_count=y&:origin=viz_share_link'
                            ,df$urls_expanded_url)

df$urls_expanded_url <- gsub('^https://lnkd.in/g3m324q$'
                            ,'https://public.tableau.com/views/IButtons_15964398581060/IronViz2020?:language=en&:display_count=n&:origin=viz_share_link'
                            ,df$urls_expanded_url)

df$urls_expanded_url <- gsub('^http://shorturl.at/fFLST$'
                            ,'https://public.tableau.com/profile/marian.fairman#!/vizhome/EvictionALoomingCrisisinPublicHealth/Evictions'
                            ,df$urls_expanded_url)


# Case 0: not NA
df <- filter(df,!is.na(urls_expanded_url))

# Expand short urls
short_urls <- df$urls_expanded_url[nchar(df$urls_expanded_url)<35]
expanded_urls <- unique((longurl::expand_urls(short_urls)))


#url_df <- data.frame(url = urls)
url_df <- dplyr::left_join(df,expanded_urls, by = c("urls_expanded_url" = "orig_url"))

df$urls <- ifelse(!is.na(url_df$expanded_url),url_df$expanded_url,url_df$urls_expanded_url)

df <- df %>% filter(nchar(urls)>=35)

# Case 1: https://public.tableau.com/views/IronViz2020EveryMotherCounts-ExploringMaternalHealth/EveryMotherCounts?:language=en-GB&:display_count=y&:origin=viz_share_link
pattern1 <- "^https://public\\.tableau\\.com/views(?:/.*)?$"
case_1_urls <- df$urls[grep(pattern1,df$urls)]

# Case 2: https://public.tableau.com/profile/agata1619#!/vizhome/Whattimearebabiesborn/Whattimearebabiesborn?publish=yes
pattern2a <- "^https://public\\.tableau\\.com/profile(?:/.*)?$"
pattern2b <- "^(.*)vizhome(.*)$"
first_pass <- df$urls[grep(pattern2a,df$urls)]
second_pass <- first_pass[grep(pattern2b,first_pass)]

case_2_urls <- second_pass

remaining_urls <- subset(df$urls, !(df$urls %in% c(case_1_urls,case_2_urls)))
ironviz_urls <- unique(c(case_1_urls,case_2_urls))

remaining_df <- filter(df, df$urls %in% remaining_urls)
ironviz_df <- filter(df,df$urls %in% ironviz_urls)

# 'https://public.tableau.com/views/TheImportanceofSleep-IronViz2020mobileapp/Home?:language=en&:display_count=y&publish=yes&:origin=viz_share_link'

# Two types of Tableau links
# 1. 'views'
# 2. 'profile vizhome'

for(i in 1:length(ironviz_df$urls)){
  
  viz <- ironviz_df$urls[i]
  # Case 1a: views with a ? clause
#  if(!is.na(str_locate(pattern="views",viz)[1])){
#    
#    # End of "views"
#    views_end_pos <- str_locate_all(pattern="views",viz)[[1]][1,2]
#    two_letters_of_dash <- substr(viz,views_end_pos+2,views_end_pos+3)
#    
#    # Start of "views"
#    views_start_pos <- str_locate_all(pattern="views",viz)[[1]][1,1]
#    
#    knit <- paste0(substr(viz,1,views_start_pos-1)
#                   ,'static/images/',two_letters_of_dash,'/'
#                   ,substr(viz,views_end_pos+2,nchar(viz))
#                   ,'/1.png')
#  }
  
  # Case 1b: views with a ? clause
  if(!is.na(str_locate(pattern="views",viz))[1]){

    if(!is.na(str_locate(pattern="\\?:|\\?%|\\?&",viz))[1]){

      # Start of "?:"
      position <- (str_locate_all(pattern="\\?:|\\?%|\\?&",viz))[[1]][1,1]
      viz <- substr(viz,1,position-1)
      
    }
    
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
  
  img <- knit
  
  if(i == 1){
    img_list <- img
  }
  
  if(i > 1){
    img_list <- c(img_list,img)
  }
  img_list <- img_list
}
ironviz_df$img_links <- img_list
ironviz_df$tweet_link <- paste0('https://twitter.com/',ironviz_df$screen_name,'/status/',ironviz_df$status_id)

# Get earliest tweet for viz
ironviz_df <- ironviz_df %>% 
  group_by(img_links) %>%
  filter(status_id == min(status_id)) 
ironviz_df <- unique(ironviz_df)

ironviz_df$img_and_tweet_link <- paste0("<a href='",ironviz_df$tweet_link,"'>",
                                        "<img src='",ironviz_df$img_links,"'></a>")

# randomise vizzes
ironviz_links <- sample(ironviz_df$img_and_tweet_link
                        ,length(ironviz_df$img_and_tweet_link))


number_of_vizzes <- length(ironviz_df$img_and_tweet_link)

# Update html files

# Write list of images to a text file for now
fileConn <- file("ironviz_html/img_list.txt")
writeLines(ironviz_links, fileConn)
close(fileConn)

blurb_temp_file <- file("ironviz_html/blurb_template.txt")
blurb <- readLines(blurb_temp_file)
blurb <- gsub('NUM_OF_VIZZES',number_of_vizzes,blurb)

blurb_file <- file("ironviz_html/blurb.txt")
writeLines(blurb, blurb_file)
close(blurb_file)
close(blurb_temp_file)

# create full html
header_file <- file("ironviz_html/header.txt")
blurb_file <- file("ironviz_html/blurb.txt")
images_file <- file("ironviz_html/img_list.txt")
footer_file <- file("ironviz_html/footer.txt")

header <- readLines(header_file)
blurb <- readLines(blurb_file)
images <- readLines(images_file)
footer <- readLines(footer_file)

full_html <- c(header,blurb,images,footer)

html_file <- file("ironviz_html/ironviz.html")
writeLines(full_html, html_file)
close(html_file)

close(header_file)
close(blurb_file)
close(images_file)
close(footer_file)

