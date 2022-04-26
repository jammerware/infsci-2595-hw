# load libraries
library(tidyverse)
library(recipes)
library(doParallel)

# enable raw/pre-processed loading of data
load_project_data <- function(
    convert_categoricals = TRUE, 
    response_to_log = TRUE, 
    outcome_to_numeric = TRUE,
    center_and_scale = FALSE # caret can center/scale with simple arguments, but rstanarm can't
  ) {
  
  # load and try to suppress overeager messages
  df <- readr::read_csv(
    file.path(getwd(), "data", "final_project_train.csv"),
    col_names = TRUE, 
    col_types = cols()
  )
  
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
  
  if (center_and_scale == TRUE) {
    df <- df %>%
      mutate(across(where(is.numeric), ~ as.numeric(scale(.))))
  }
  
  return(df)
}

generate_prediction_trends_regression <- function(model, viz_grid, predictFn = predict, ...) {
  preds <- predict(model, viz_grid, interval = "confidence") %>%
    as.data.frame %>%
    as_tibble %>%
    rename(
      pred = fit,
      ci_lwr = lwr,
      ci_upr = upr
    ) %>%
    bind_cols(
      predict(
        model,
        viz_grid,
        interval = "prediction"
      ) %>%
        as.data.frame %>%
        as_tibble %>%
        select(
          pred_lwr = lwr,
          pred_upr = upr
        )
    )
  
  viz_grid %>% bind_cols(preds)
}

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

visualize_bayesian_coefficients <- function(model) {
  model %>%
    plot() +
    geom_vline(xintercept = 0, color = "grey", linetype = "dashed", size = 1.) +
    theme_bw()
}

visualize_bayesian_coefficient_distributions <- function(model) {
  as.data.frame(model) %>% 
    tibble::as_tibble() %>% 
    select(all_of(names(model$coefficients))) %>% 
    tibble::rowid_to_column("post_id") %>% 
    pivot_longer(!c("post_id")) %>% 
    ggplot(mapping = aes(x = value)) +
    geom_histogram(bins = 55) +
    facet_wrap(~name, scales = "free", ncol = 3) +
    theme_bw() +
    theme(axis.text.y = element_blank())
}

visualize_varimp <- function(model, varImpfn = varImp) {
  model %>%
    varImpfn %>%
    arrange(desc(Overall)) %>%
    head(15) %>%
    rownames_to_column() %>%
    rename(
      predictor = rowname,
      importance = Overall
    ) %>%
    ggplot(mapping = aes(x = importance, y = factor(predictor, level = predictor))) +
    geom_col()
}

get_significant_bayesian_coefficients <- function(model) {
  model %>%
    posterior_interval %>%
    as.data.frame %>%
    rownames_to_column %>%
    as_tibble %>%
    filter(!(!!as.symbol("5%")  < 0 & !!as.symbol("95%") > 0)) %>%
    arrange(desc(abs(!!as.symbol("5%"))))
}

my_supertrain <- function(formula, data, method, info, ...) {
  parallel_cluster <- makeCluster(detectCores() - 1)
  registerDoParallel(parallel_cluster)
  
  out <- tryCatch(
    {
      set.seed(2022)
      
      model <- train(
        formula,
        data = data,
        method = method,
        preProcess = c("center", "scale"),
        metric = info$metric,
        trControl = info$ctrl,
        ...
      )
      
      model   
    },
    error = function(cond) {
      message(paste("Error in supertrain:", cond))
      return(NULL)
    },
    finally = {
      if (exists("parallel_cluster")) {
        stopCluster(parallel_cluster)
      }
    }
  )
  
  return(out)
}

load_model <- function(name) {
  readr::read_rds(paste("./models/", name, sep = ""))
}

save_model <- function (name, model, prefix = "") {
  readr::write_rds(model, paste("./models/", prefix, name ,".rds", sep = ""))
}