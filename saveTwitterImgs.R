

download_images <- function(parsedTwitter, downloadPath = paste0(getwd(),"/img/"), maxImagesDownload = 100) {
  
  list_images <- parsedTwitter %>% 
    select(attachments, url_image) %>% 
    filter(!is.na(url_image) | url_image != "NULL") %>% 
    distinct() %>% 
    sample_n(maxImagesDownload)
  
  # Define title of each image file
  # There are still some duplicates in the images - 
  title_image = c()
  for (i in 1:nrow(list_images)) {
    title_image[i] = paste0("img_",list_images$attachments[i])
  }
  
  for (i in 1:nrow(list_images)){ # Download Images - this is a sample only - use nrow(list_images)
    try(
      download.file(list_images$url_image[i], 
                    destfile = paste0(downloadPath, title_image[i],".jpg"),
                    mode = 'wb'),
      silent = TRUE)
    Sys.sleep(3)
  }
}


# Load Requirements 
Packages <- c("dplyr", "tidyr", "tibble", "lubridate", "stringr")
#install.packages(Packages)

lapply(Packages, library, character.only = TRUE)


allDates <- list()
start_date <- lubridate::date("2022-01-01")
for (i in 1:31) { 
  
  if (i == 1) {
    
    start_date = start_date
    
  } else {
    
    start_date <- lubridate::add_with_rollback(start_date, days(1))
  }
  
  end_date <- lubridate::add_with_rollback(start_date, days(1))
  
  dates = list("start" = paste0(as.character(start_date), "T00:00:00Z"), "end" = paste0(as.character(end_date), "T00:00:00Z"))
  allDates[[i]] <- dates
}


setwd('...wd..../data/')
allTweets <- list.files('...wd..../data/', pattern = '.rds')

twitterRaw <- readRDS(allTweets[[1]])
for (i in 2:length(allTweets)) {
  twitterTmp <- readRDS(allTweets[[i]])
  twitterRaw <- bind_rows(twitterRaw, twitterTmp)
  
}

dateCounts <- twitterRaw %>% 
  group_by(date) %>% 
  summarise(Count = n())

head(twitterRaw$text, 10)


# Download
setwd('...wd....')
download_images(twitterRaw, maxImagesDownload = 1000) # try NROW(dataframe)