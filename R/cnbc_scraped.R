# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(rvest)
library(tidyverse)

# Data Import and Cleaning
## Construct section names and set the url
sections <- c("Business", "Investing", "Tech", "Politics")
urls <- c("https://www.cnbc.com/business/",
          "https://www.cnbc.com/investing/",
          "https://www.cnbc.com/technology/",
          "https://www.cnbc.com/politics/")

## Create an empty tibble as place holder
cnbc_tbl <- data.frame(headline = character(), 
                       length = integer(), 
                       source = character(), 
                       stringsAsFactors = FALSE) %>%
  as_tibble()

for (i in 1:length(sections)){
  ## Read in html pages
  page <- read_html(urls[i])
  ## Steps to extract the elements 
  page_elements <- html_elements(page, ".Card-title")
  ## Convert elements to readable texts
  page_text <- html_text(page_elements)
  ## Count number of words in each str
  length <- str_count(page_text, "\\S+")
  ## Generate source variable
  source <- sections[i]
  ## Combine all variables into a tibble
  page_tbl <- data.frame(headline = page_text,
                         length = length,
                         source = source) %>%
    as.tibble()
  # Combine the tibble with existing tibble
  cnbc_tbl <- rbind(cnbc_tbl, page_tbl)
  
}

# Visualization
## I choose a boxplot to show the mean differences on length between sources
cnbc_tbl %>%
  ggplot(aes(x = source, y = length)) +
  geom_boxplot() +
  geom_violin(alpha = 0.5)

# Analysis
## Conduct anova
anova_result <- aov(length ~ source, data = cnbc_tbl)
## Show result summary
summary_result <- summary(anova_result)
summary_result
## No significant differences between lengths across sources

# Publication
## "The results of an ANOVA comparing lengths across sources was F(3, 130) = 1.47, p = .23. This test was not statistically significant."
## assign p value first
p_value <- summary_result[[1]]$'Pr(>F)'[1]
## Generate the message content
message <- sprintf("The results of an ANOVA comparing lengths across sources was F(%d, %d) = %.2f, p = %s. This test %s statistically significant.",
                   dfn = summary_result[[1]]$'Df'[1] , 
                   dfd = summary_result[[1]]$'Df'[2], 
                   F_value = summary_result[[1]]$'F value'[1], 
                   p_value =  sub("^0\\.", ".", formatC(p_value, format = 'f', digits = 2)),
                   ifelse(p_value < 0.05, "was", "was not"))
## Output
message
