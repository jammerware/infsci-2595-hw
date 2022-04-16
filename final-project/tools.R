# load libraries
library(tidyverse)
library(recipes)

# enable raw/pre-processed loading of data
load_project_data <- function(info = NULL) {
  df <- readr::read_csv("./final_project_train.csv", col_names = TRUE)
  
  if (is.null(info)) {
    info = list(
      convert_data_types = TRUE,
      response_to_log = TRUE
    )
  }
  
  if (info$convert_data_types == TRUE) {
    df <- df %>% mutate_if(is.character, as.factor)
  }
  
  if (info$response_to_log == TRUE) {
    df <- df %>%
      mutate(
        response_log = log(response),
        response = NULL
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

# select semantic groups of variables
bookkeeping_vars <- as.name("rowid")

y_vars <-  list(
  as.name("outcome"),
  as.name("response")
)
