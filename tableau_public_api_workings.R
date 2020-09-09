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

