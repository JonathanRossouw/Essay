# Hyperparameter tuning wrapper

hyp_tune_func <- function(data_lists = data_lists,
                          data_actual = data_val,
                          hyperparams = hyp,
                          fit_func,
                          type){
    # Run iteratively through hyperparameter grid
    hyp_fit <- map(hyperparams, fit_func, data_lists)

    hyp_res <- map(hyp_fit, ~data_actual %>% mutate(pred = .x$val) %>%
                       summarise(mean((pred - y)^2, na.rm = TRUE)) %>% .[[1]])
    # Store Results
    res <- map2_df(hyperparams, hyp_res, ~ .x %>%
                       as_tibble() %>%
                       mutate(mse = .y))
    res
}
