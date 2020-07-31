# Get data from Twitter

source("function_get_and_save_tweets.R")

get_and_save_tweets(text = "#tableau", count = 10000, path = "data")
get_and_save_tweets(text = "#datafam", count = 10000, path = "data")
get_and_save_tweets(text = "#rstats", count = 10000, path = "data")
get_and_save_tweets(text = "#ironviz", count = 10000, path = "data")

