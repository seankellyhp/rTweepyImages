
# install reticulate
#install.packages('reticulate', dependencies = TRUE, INSTALL_opts = '--no-lock')
setwd("...wd....")

# Load Python Path
#install.packages("reticulate")
library(reticulate)
use_condaenv("twitter") # You have a conda environment, can also be pyenv, or other python

# Load Functions
source_python("searchTwitter.py")

parse_twitter <- function(search_results) {
  
  ### Loop Over Results 
  nullify <- function(x) {
    outCl <- ifelse(is.null(x), NA, x)
    return(outCl)
  }
  
  mediaDF <- as_tibble(list(
    "media_key" = NA, 
    "link_image" = NA,
    "url_image" = NA,
    "link_type" = NA,
    "view_count" = NA))
  
  tweetDF <- as_tibble(list(
    "user" = NA,
    "id" = NA,
    "lang" = NA,
    "geo" = NA,
    "referenced_tweets" = NA,
    "reply_settings" = NA,
    "created_at" = NA,
    "text" = NA,
    "source" = NA,
    "retweet_count" = NA,
    "reply_count" = NA,
    "like_count" = NA,
    "quote_count" = NA,
    "attachments" = NA,
    "hashtags" = NA
    ))
  
  userDF <- as_tibble(list(
    "user" = NA,
    "name" = NA,
    "username" = NA,
    "location" = NA,
    "verified" = NA,
    "description" = NA))
  
  for (i in 1:length(search_results)) {
    # Subset Results by Page
    dfPage <- search_results[[i]]
    
    for (j in 1:length(dfPage[0])) {
      # Subset Results by Tweet 
      
     dfTweets <- dfPage[0][[j]]
     
    # Tweet Info 
    tweet_info <- list(
      "user" = nullify(dfTweets$author_id),
      "id" = nullify(dfTweets$id),
      "lang" = nullify(dfTweets$lang),
      "geo" = nullify(dfTweets$geo),
      "referenced_tweets" = nullify(dfTweets$data$referenced_tweets[[1]]),
      "reply_settings" = nullify(dfTweets$reply_settings),
      "created_at" = nullify(dfTweets$data$created_at),
      "text" = nullify(dfTweets$text),
      "source" = nullify(dfTweets$source),
      "public_metrics" = nullify(dfTweets$data$public_metrics),
      "retweet_count" = nullify(dfTweets$data$public_metrics$retweet_count),
      "reply_count" = nullify(dfTweets$data$public_metrics$reply_count),
      "like_count" = nullify(dfTweets$data$public_metrics$like_count),
      "quote_count" = nullify(dfTweets$data$public_metrics$quote_count),
      "attachments" = nullify(dfTweets$data$attachments[[1]]),
      "hashtags" = nullify(dfTweets$entities$hashtags[[1]])
      )
    
    tweet_tibble_tmp <- as_tibble(tweet_info)
    tweetDF <- bind_rows(tweetDF, tweet_tibble_tmp)
    
    }
    
    for (j in 1:length(dfPage[1]$media)) {
     
      dfMedia <- dfPage[1]$media[[j]]
    
     # Image Info 
     media_info <- list(
       "media_key" = nullify(dfMedia$media_key),
       "link_image" = nullify(dfMedia$preview_image_url),
       "url_image" = nullify(dfMedia$data$url),
       "link_type" = nullify(dfMedia$data$type),
       "view_count" = nullify(dfMedia$data$public_metrics[[1]]))
     
     media_tibble_tmp <- as_tibble(media_info)
     mediaDF <- bind_rows(mediaDF, media_tibble_tmp)
     
    }
    
    for (j in 1:length(dfPage[1]$users)) {
     
      dfUser <- dfPage[1]$users[[j]]
     
     # User Info 
     user_info <- list(
       "user" = nullify(dfUser$id),
       "name" = nullify(dfUser$name),
       "username" = nullify(dfUser$username),
       "location" = nullify(dfUser$location),
       "verified" = nullify(dfUser$verified),
       "description" = nullify(dfUser$description)
     )
     
     user_tibble_tmp <- as_tibble(user_info)
     userDF <- bind_rows(userDF, user_tibble_tmp)
    }
  }
  
  resultsTotal <- list("tweets" = dplyr::distinct(tweetDF)[-1,], "media" = dplyr::distinct(mediaDF)[-1,], "users" = dplyr::distinct(userDF)[-1,])
  
  return(resultsTotal)
}


# Load Requirements 
Packages <- c("dplyr", "tidyr", "tibble", "lubridate", "stringr")
#install.packages(Packages)

lapply(Packages, library, character.only = TRUE)


#
###### RUN PROGRAM
#
#
#
#
#
#

allDates <- list()
start_date <- lubridate::date("2022-01-01")
for (i in 1:365) { # Loop over every day, helps with controlling Twitter API limit
  
  if (i == 1) {
    
    start_date = start_date
    
  } else {
    
    start_date <- lubridate::add_with_rollback(start_date, days(1))
  }
  
  end_date <- lubridate::add_with_rollback(start_date, days(1))
  
  dates = list("start" = paste0(as.character(start_date), "T00:00:00Z"), "end" = paste0(as.character(end_date), "T00:00:00Z"))
  allDates[[i]] <- dates
}



# Load Params
key = 'KEY' # Twitter API Key
query = '"covid" OR "coronavirus" has:media' # Set Query
max_results = as.integer(100) # Max 100 Tweets Per Page
page_limit = 5 # Number of twitter api pages to iterate through
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"


for (i in 1:length(allDates)) {
  start_time = allDates[[i]]$start
  end_time = allDates[[i]]$end
  print(paste0('starting: ', start_time))
  
  # Search Twitter - Uses Python Command
  searchResults <- search_twitter(bearer_key = key, query = query, start_time = start_time, end_time = end_time, max_results = max_results, page_limit = page_limit)
  
  # Parse Results for R
  resultsCl <- parse_twitter(searchResults)
  
  tempDF <- left_join(resultsCl[['users']], resultsCl[['tweets']], by = "user")
  joinedDF <- left_join(tempDF, resultsCl[['media']], by = c("attachments" = "media_key"))

  joinedDF$tweetURL <- str_extract(joinedDF$text, url_pattern)
  joinedDF <- joinedDF %>% mutate(
    day = day(created_at),
    month = month(created_at),
    year = year(created_at),
    date = as.Date(created_at),
    user_engagement = retweet_count + reply_count + like_count + quote_count
  )
  
  saveRDS(joinedDF, paste0("...wd..../data/scrape_", lubridate::date(start_time), "_", lubridate::date(end_time), ".rds"))
  Sys.sleep(60)
  
}




#
#
#
#
### Load data 
#
#
#
#




