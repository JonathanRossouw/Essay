# Performance and Plotting Function

perf_plot <- function(data_lists = data_lists,
                        data_train = data_train,
                        data_val = data_val,
                        hyperparams = hyp,
                        fit_func,
                        type,
                        train_title,
                        val_title
                        ){

    hyp_fit <- fit_func(hyperparams, data_lists)


    train_err <- data_train %>% mutate(pred = hyp_fit$train) %>%
                     summarise(mean((pred - y)^2, na.rm = TRUE)) %>% .[[1]]

    hyp_res <- data_val %>% mutate(pred = hyp_fit$val) %>%
                     summarise(mean((pred - y)^2, na.rm = TRUE)) %>% .[[1]]
    # Store Results

    train_res <- hyperparams %>%
      as_tibble() %>%
      mutate(mse = train_err)

    res <- hyperparams %>%
      as_tibble() %>%
      mutate(mse = hyp_res)

    # Plot training and validation forecasts

    train_forecast <- data_train %>% mutate(pred = hyp_fit$train)

    train_plot <- vol_plot_func(data_train, train_forecast, title = train_title)

    val_forecast <- data_val %>% mutate(sigma = hyp_fit$val)

    val_plot <- vol_plot_func(data_val, val_forecast, title = val_title)

    return(list(train_plot = train_plot, val_plot = val_plot, performance = res, training_error = train_res))
}
