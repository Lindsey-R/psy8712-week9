# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(RedditExtractoR)
library(jsonlite)

# Data Import and Cleaning
## Tried but failed to extract all posts published a month ago so I give up
## Construct the URL 
## url <- find_thread_urls(subreddit="rstats", period = "month")
## Should add sort so sort based on time; other wise, sort based on "top" and get less results
url <- find_thread_urls(subreddit="rstats", sort_by = "new", period = "month")
## WILL TAKE A LONG LONG TIME
## Make the request 
content <- get_thread_content(url$url)

# Do it with JSON DON'T WORK ----
# url_json <- "https://www.reddit.com/r/rstats/new/.json?limit=100"
# first_df <- fromJSON(url_json, flatten = TRUE)
# reddit_data <- first_df$data$children$data
# next_search <- paste0("after =", first_df$data$after)
# # Find the newest data
# newest_date <- first_df$data$children$data[1, "created"]
# # Find date a month ago
# month_ago <- (Sys.Date() - 30) %>% as.POSIXct() %>% as.numeric()
# 
# while (newest_date > month_ago) {
#   next_df <- fromJSON(
#     flatten = TRUE,
#     paste0(url_json, "&", next_search)
#   )
#   newest_date <- next_df$data$children$data[1, "created"]
#   reddit_data <- bind_rows(reddit_data, next_df$data$children$data)
# }

# Try with JSON DON'T RUN ---- 
after <- NULL
posts_list <- data.frame()
one_month_ago <- (Sys.Date() - 30) %>% as.POSIXct() %>% as.numeric()  # Date one month ago

# Loop to fetch posts until you reach posts older than one month
repeat {
  # Construct the URL
  url <- paste0('https://oauth.reddit.com/r/rstats/new/.json?limit=100', ifelse(is.null(after), '', paste0('&after=', after)))
  
  # Make the request
  response <- fromJSON(url, flatten = TRUE)
  
  posts_batch <- response$data$children
  posts_date <- response$data$children$data.created_utc
  posts_list <- bind_rows(posts_list, posts_batch)
    
    # Check the date of the last post in the batch
    last_post_date <- max(posts_date)
    
    # Update the 'after' parameter for the next request
    after <- response$data$after
  
    # Break the loop if the last post is older than one month
    if (last_post_date < one_month_ago) {
      break
    }
}

# Extract the desired information from the posts
titles <- posts_list$data.title
upvotes <- posts_list$data.ups
comments <- posts_list$data.num_comments

# Create a dataframe
rstats_tbl_json <- data.frame(post = titles, upvotes = upvotes, comments = comments)
  
# Back to HW ----
## Extract information
titles <- content$threads$title
upvotes <- content$threads$upvotes
comments <- content$threads$comments

## Create tbl
rstats_tbl <- as_tibble(titles) %>%
  mutate(upvotes = upvotes,
         comments = comments) %>%
  rename(post = value)

# Visualization
## Two continuous variable, so I do a scatter plot 
rstats_tbl %>%
  ggplot(aes(x = upvotes, y = comments)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Upvotes", y = "Comments")

# Analysis
## Obtain correlation between upvotes and comments
correlation <- cor.test(rstats_tbl$upvotes, rstats_tbl$comments)
correlation$estimate
## I observe a correlation of 0.518
correlation$p.value
## I observe a significant p_value.

# Publication
## The correlation between upvotes and comments was r(122) = .52, p = .00. This test was statistically significant.
## Constructing the message
message <- sprintf("The correlation between upvotes and comments was r(%d) = %s, p = %s. This test %s statistically significant.",
                   correlation$parameter, 
                   sub("^0\\.", ".", formatC(correlation$estimate, format = 'f', digits = 2)),
                   sub("^0\\.", ".", formatC(correlation$p.value, format = 'f', digits = 2)),
                   ifelse(correlation$p.value < 0.05, "was", "was not"))

## Printing the message
cat(message)
