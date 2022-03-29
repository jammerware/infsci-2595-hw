# load tidyverse
library(tidyverse)

# enable raw/pre-processed loading of data
load_project_data <- function(raw = FALSE) {
  df_raw <- readr::read_csv("./final_project_train.csv", col_names = TRUE)
  
  if (raw) {
    return(df_raw)
  }
  
  return(
    df_raw %>% mutate_if(is.character, as.factor)
  )
}

# select semantic groups of variables
bookkeeping_vars <- as.name("rowid")
y_vars <-  list(
  as.name("outcome"),
  as.name("response")
)
