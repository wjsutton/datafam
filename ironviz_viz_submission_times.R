
#submissions <- filter(ironviz,status_id %in% ironviz_df$status_id)
submissions <- inner_join(ironviz,ironviz_df, by = c('status_id' = 'status_id'))


#convert datetimes to PDT
attributes(submissions$created_at)$tzone <- "America/Los_Angeles" 

test <- "2020-08-05T07:59:59"
deadline <- as.POSIXct(test,format="%Y-%m-%dT%H:%M:%OS")
op <- options(digits.secs = 3)
attributes(deadline)$tzone <- "America/Los_Angeles"
deadline

submissions$time_to_deadline <- abs(difftime(sort(submissions$created_at),deadline,units = "hours"))

output <- submissions[,c('created_at','screen_name.x','text','urls','time_to_deadline','img_links','tweet_link')]
write.csv(output,'ironviz_submission_times.csv',row.names = F)

