library(rvest)
library(httr)

test <- filter(output,!is.na(viz_img))

for(i in 1:nrow(test)){
  status <- http_status(GET(test$viz_img[i]))$category
  
  if(status!='Success'){
    print(paste0(i,' ',test$viz_img[i],' ',status))
  }
  
  if(i %% 10 == 0){
    print(paste0(i,' done!'))
  }
}