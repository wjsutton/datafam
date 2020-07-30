# https://stackoverflow.com/questions/40665269/r-convert-data-frame-to-json

library(jsonlite)
library(rtweet)
library(dplyr)

datafam <- readRDS("data/#datafam_tweets.RDS")

# Shaping data.frame for json file
datafam$handle <- paste0("@",datafam$screen_name)

datafam_nodes <- datafam %>%
  select(handle) %>% 
  count(handle)

names(datafam_nodes)[names(datafam_nodes) == "n"] <- "tweets"

# Converting output data.frame into json and writing locally
datafam_nodes_json <- toJSON(datafam_nodes, pretty = FALSE)
write(datafam_nodes_json, "data/datafam_nodes.json")

