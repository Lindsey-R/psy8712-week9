# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(RedditExtractoR)

# Data Import and Cleaning
## Tried but failed to extract all posts published a month ago so I give up
## Construct the URL 
url <- find_thread_urls(subreddit="rstats", period = "month")
## Make the request 
content <- get_thread_content(url$url)
  
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
## I observe a correlation of 0.539
correlation$p.value
## I observe a significant p_value.

# Publication
## The correlation between upvotes and comments was r(121) = 0.54, p = .00. This test was statistically significant.
## Constructing the message
message <- sprintf("The correlation between upvotes and comments was r(%d) = %s, p = %s. This test %s statistically significant.",
                   correlation$parameter, 
                   formatC(correlation$estimate, format = 'f', digits = 2, flag = '-'), 
                   sub("^0\\.", ".", formatC(correlation$p.value, format = 'f', digits = 2)),
                   ifelse(correlation$p.value < 0.05, "was", "was not"))

## Printing the message
cat(message)
