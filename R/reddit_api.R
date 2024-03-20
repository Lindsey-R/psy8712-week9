# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(jsonlite)
library(tidyverse)

# Data Import and Cleaning
## Tried but failed to extract all posts published a month ago so I give up
## Construct the URL
url <- 'https://oauth.reddit.com/r/rstats/.json?limit=100'
## Make the request 
json_data <- fromJSON(url, flatten = TRUE)
  
## Extract information
titles <- json_data$data$children$data.title
upvotes <- json_data$data$children$data.ups
comments <- json_data$data$children$data.num_comments

## Create tbl
rstats_tbl <- as.tibble(titles) %>%
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
## I observe a correlation of 0.628 
correlation$p.value
## I observe a significant p_value.

# Publication
## The correlation between upvotes and comments was r(98) = 0.63, p = 0.00. This test was statistically significant.

## Write the line
## Constructing the message
message <- sprintf("The correlation between upvotes and comments was r(%d) = %s, p = %s. This test %s statistically significant.",
                   correlation$parameter, 
                   formatC(correlation$estimate, format = 'f', digits = 2, flag = '-'), 
                   formatC(correlation$p.value, format = 'f', digits = 2, flag = '-'),
                   ifelse(correlation$p.value < 0.05, "was", "was not"))

## Printing the message
cat(message)
