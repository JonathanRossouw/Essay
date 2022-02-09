###### Simple LSTM Array Function

data_array_func <- function(data, val, initial, assess, skip, type){

    inner_func <- function(data, initial, assess, skip, type){
        # Select Appropriate Data
        data <- data %>%
            dplyr::select(date, y, sigma) #%>% head((nrow(data) - 14))
        # Creating Rolling Data Splits
        data_rol <- rolling_origin(data,
                                   initial = initial,
                                   assess = assess,
                                   skip = skip,
                                   cumulative = FALSE)
        # Create Training Predictor Array
        x_array_y <- lapply(data_rol$splits, FUN = function(X){analysis(X)$y})
        x_array_sigma <- lapply(data_rol$splits, FUN = function(X){analysis(X)$sigma})

        x_array <- array(data = c(unlist(x_array_y),
                                        unlist(x_array_sigma)),
                               dim = c(length(x_array_y), initial, 2))

        if(type == "three_day"){
            x_array <- x_array %>% head(-2)
        }

        # Create Training Target Array
        y_array <- lapply(data_rol$splits, FUN = function(X){testing(X)$y})
        y_array <- do.call("rbind", y_array) %>%
            array(., dim = c(length(y_array), assess, 1))

        if(type == "three_day"){
            y_array <- y_array %>% tail(-2)
        }
        return(list(x_array, y_array))
    }

    train_arrays <- inner_func(data = data, initial = initial, assess = assess, skip = skip, type = type)
    val_arrays <- inner_func(data = val, initial = initial, assess = assess, skip = skip, type = type)

    return(list(x_train_array = train_arrays[[1]],
                y_train_array = train_arrays[[2]],
                x_val_array = val_arrays[[1]],
                y_val_array = val_arrays[[2]]))
}
