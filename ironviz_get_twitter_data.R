# Get data from Twitter

source("function_get_and_save_tweets.R")

get_and_save_tweets(text = "#ironviz", count = 10000, path = "data")
get_and_save_tweets(text = "#ironviz2020", count = 10000, path = "data")

