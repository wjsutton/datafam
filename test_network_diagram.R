# https://stackoverflow.com/questions/40665269/r-convert-data-frame-to-json

library(jsonlite)
library(rtweet)

## search for #rstats tweets
rstats <- search_tweets("#rstats", n = 200)

## create from-to data frame representing retweet/mention/reply connections
rstats_net <- network_data(rstats, "retweet,mention,reply")

# converts b to nested list column
rstats_net2 <- aggregate(to ~ from, rstats_net, list)

rstats_net_json <- toJSON(rstats_net2, pretty = TRUE)
write(rstats_net_json, "data/#rstats_network.json")