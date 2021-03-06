# Tableau Public API workings

library(jsonlite)

profile <- 'https://public.tableau.com/profile/api/will7508'

# limits to 300, can use index +1 for next 300
workbooks <- 'https://public.tableau.com/profile/api/will7508/workbooks?count=300&index=0'

# Profile Data extract 
profile_data <- fromJSON(profile)

following <- profile_data$totalNumberOfFollowing
followers <- profile_data$totalNumberOfFollowers

twitter <- profile_data$websites$url[grepl('twitter',profile_data$websites$url)]
linkedin <- profile_data$websites$url[grepl('linkedin',profile_data$websites$url)]

last_publish <- profile_data$lastPublishDate
visible_workbooks <- profile_data$visibleWorkbookCount

# Note workbooks includes invisible_workbooks, determined by the 'showInProfile' TRUE/FALSE flag

library(jsonlite)

get_tableau_profile_api_extract <- function(profile_name){

  
  profile_call <- paste0('https://public.tableau.com/profile/api/',profile_name)
  
  profile_data <- jsonlite::fromJSON(profile_call)
  
  profile_following <- profile_data$totalNumberOfFollowing
  profile_followers <- profile_data$totalNumberOfFollowers
  
  profile_twitter <- profile_data$websites$url[grepl('twitter',profile_data$websites$url)]
  profile_linkedin <- profile_data$websites$url[grepl('linkedin',profile_data$websites$url)]
  
  profile_last_publish <- profile_data$lastPublishDate
  profile_visible_workbooks <- profile_data$visibleWorkbookCount
  
  profile_df <- data.frame(name=profile_name,
                           profile_url=paste0('https://public.tableau.com/profile/',profile_name,'#!/'),
                           api_call=profile_call,
                           followers=profile_followers,
                           following=profile_following,
                           twitter=profile_twitter,
                           linkedin=profile_linkedin,
                           last_publish=profile_last_publish,
                           visible_workbooks=profile_visible_workbooks,
                           stringsAsFactors = F)
  return(profile_df)
}

get_tableau_profile_api_extract('will7508')