# load libraries
library(tidyverse)
library(recipes)

# enable raw/pre-processed loading of data
load_project_data <- function(convert_categoricals = TRUE, response_to_log = TRUE, outcome_to_numeric = TRUE) {
  df <- readr::read_csv("./final_project_train.csv", col_names = TRUE, col_types = cols())
  
  # i'm going to assume that i never want the row_id until further notice
  df <- df %>%
    select(-rowid)
  
  if (convert_categoricals == TRUE) {
    df <- df %>% mutate_if(is.character, as.factor)
  }
  
  if (response_to_log == TRUE) {
    df <- df %>%
      mutate(
        response_log = log(response),
        response = NULL
      )
  }
  
  if (outcome_to_numeric == TRUE) {
    df <- df %>%
      mutate(
        outcome_numeric = ifelse(outcome == "event", 1, 0),
        outcome = NULL
      )
  }
  
  # if (preprocess_data) {
  #   rec_preprocessed <- recipe(
  #     response ~ .,
  #     data = df_raw
  #   ) %>%
  #     step_normalize(all_numeric_predictors()) %>%
  #     step_log(all_outcomes())  %>%
  #     prep()
  #   
  #   df_raw <- bake(rec_preprocessed, new_data = df_raw)
  # }
  
  return(df)
}

y_vars <-  list(
  as.name("outcome"),
  as.name("response")
)

visualize_correlations <- function(data, column_name_start) {
  data %>%
    select(starts_with(column_name_start)) %>%
    GGally::ggpairs(progress = FALSE) + 
    theme_bw()
}

# visualize distributions of predictors, selecting by column name start
visualize_distributions <- function(x, column_name_start) {
  x %>%
    select(starts_with(column_name_start)) %>%
    pivot_longer(cols = everything()) %>%
    ggplot(mapping = aes(x = value)) +
    geom_histogram(bins = 50) +
    facet_wrap(~name, scales = "free") + 
    theme_bw() +
    theme(axis.text.y = element_blank())
}

# custom knit to output directory
knit_to_dir <- function(input) {
  rmarkdown::render(
    input,
    output_file = paste(
      "./knit/",
      input
    ),
    envir = globalenv()
  )
}