# https://stackoverflow.com/questions/40665269/r-convert-data-frame-to-json

library(jsonlite)
library(rtweet)
library(dplyr)

## search for #rstats tweets
rstats <- search_tweets("#rstats", n = 200)

## create from-to data frame representing retweet/mention/reply connections
rstats_net <- network_data(rstats, "retweet,mention,reply")

# converts b to nested list column
rstats_net2 <- aggregate(to ~ from, rstats_net, list)

rstats_net_json <- toJSON(rstats_net2, pretty = TRUE)
write(rstats_net_json, "data/#rstats_network.json")


datafam <- readRDS("data/#datafam_tweets.RDS")

datafam$handle <- paste0("@",datafam$screen_name)

datafam_nodes <- datafam %>%
  select(handle) %>% 
  count(handle)

names(datafam_nodes)[names(datafam_nodes) == "n"] <- "tweets"
datafam_nodes_json <- toJSON(datafam_nodes, pretty = TRUE)
write(datafam_nodes_json, "data/#datafam_nodes.json")

#datafam_nodes$tweets <- datafam_nodes$n
