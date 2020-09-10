library(rvest)
library(httr)

test <- filter(output,!is.na(profile))

for(i in 1:nrow(test)){
  status <- http_status(GET(test$profile[i]))$category
  
  if(status!='Success'){
    print(paste0(i,' ',test$profile[i],' ',status))
  }
  
  if(i %% 10 == 0){
    print(paste0(i,' done!'))
  }
}