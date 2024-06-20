library(dplyr)
library(tidyr)
library(stringr)
library(rvest)
library(SuppDists)
library(tidyverse)


# Collect the base electoral data -----------------------------------------
votes_map_html <- read_html(
  "https://www.realclearpolling.com/maps/president/2024/toss-up/electoral-college"
)


votes_map_nodes <- 
  html_nodes(
    votes_map_html, 
    'body:nth-child(2) main:nth-child(1) div:nth-child(2) div:nth-child(1) section :nth-child(1) :nth-child(2) .hover\\:underline'
  )


# Collect Electoral Vote Counts -------------------------------------------
electoral_votes <- 
  votes_map_nodes %>% 
  html_text()

electoral_votes <- tibble(raw = electoral_votes)

electoral_votes <- 
  electoral_votes %>%  
  separate_wider_regex(
    cols = raw, 
    patterns = c(state = ".*", "\\(", votes = ".*", "\\)"), too_few = "align_start"
  ) %>% 
  mutate(
    state = str_trim(state), 
    votes = as.integer(votes)
  )


# Parse the state level polling -------------------------------------------
polling_links <- 
  votes_map_nodes %>% 
  html_attr('href') 

polling_links <- tibble(raw = polling_links)

misformatted_cases <- c('delaware', 'hawaii', 'district_of_columbia')
repair_state_extraction <- function(extracted_state) {
  return_val <- NA
  for(m in misformatted_cases) {
    extracted_state <- ifelse(grepl(m, extracted_state), m, extracted_state)
  }
  extracted_state
}

polling_links <- 
  polling_links %>% 
  separate_wider_delim(
    raw, '/', names = c(NA, NA, NA, NA, NA, NA, NA, 'state', NA), 
    too_few = "align_start", 
    cols_remove = FALSE
  ) %>% 
  rowwise() %>% 
  mutate(
    state = repair_state_extraction(state)
  )

# Collect the state-by-state polls 
collected_state_data <- NULL
for (i in 1:nrow(polling_links)) {
  row <- polling_links[i,]
  state_data <- 
    read_html_live(row[['raw']]) %>% 
    html_nodes("table") %>%
    html_table()
  
  # print(state_data)
  if(length(state_data) > 0) {
    state_data <- last(state_data)
    state_data$state <- row[['state']]
    # print(state_data)
    state_data <- state_data %>% mutate(across(everything(), as.character))
    collected_state_data <- bind_rows(collected_state_data, state_data)
    # print(collected_state_data) 
    Sys.sleep(2)
  }
}

collected_state_data %>% write_csv('collected_state_data.csv')



test_state <- read_html_live(
  "https://www.realclearpolling.com/polls/president/general/2024/colorado/trump-vs-biden"
  )

test_state$view()

test_state %>% 
  html_nodes("table") %>%
  html_table()


votes_map_html <- read_html(
  "https://www.realclearpolling.com/maps/president/2024/toss-up/electoral-college"
) %>% 


electoral_votes <- 
html_nodes(
  
) %>% 
html_attr('href')
html_text()





