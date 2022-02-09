##### LSTM Fit Function

lstm_fit_func <- function(hyperparams,
                          data_lists){
    # Set seed for reproducibility
    set_random_seed(seed = 43675)
    dim_1 <- dim(data_lists$x_train_array)[2:3]
    dim_2 <- dim(data_lists$y_train_array)[2]
    # Create Keras Sequential Model
    lstm_model <- keras_model_sequential()
    # Add Layers
    lstm_model %>%
        layer_lstm(units = hyperparams$lstm_units, # size of the layer
                   input_shape = dim_1,
                   # batch size, timesteps, features
                   return_sequences = hyperparams$return_sequences,
                   stateful = FALSE) %>%
        layer_dropout(rate = hyperparams$dropout_rate)
    if(hyperparams$lstm_layers == 2 & hyperparams$return_sequences == TRUE)
    {
        lstm_model %>%
        layer_lstm(units = hyperparams$lstm_units, # size of the layer
                   input_shape = dim_1,
                   # batch size, timesteps, features
                   return_sequences = FALSE,
                   stateful = FALSE) %>%
                layer_dropout(rate = hyperparams$dropout_rate)
    }
        lstm_model %>%
        layer_dense(units = dim_2)
    # Set model parameters
    lstm_model %>%
        compile(loss = hyperparams$loss, optimizer = hyperparams$optimizer, metrics = hyperparams$loss)
    # Fit Model
    lstm_model %>%
        fit(
        x = data_lists$x_train_array,
        y = data_lists$y_train_array,
        epochs = hyperparams$epochs,
        verbose = 1,
        shuffle = FALSE)
    # Predict training data
    lstm_train_forecast <- lstm_model %>%
        predict(data_lists$x_train_array) %>% t(.) %>% c(.)
    # Predict power
    lstm_val_forecast <- lstm_model %>%
        predict(data_lists$x_val_array) %>% t(.) %>% c(.)
    # Store Results
    return(list(train = lstm_train_forecast, val = lstm_val_forecast))
}
