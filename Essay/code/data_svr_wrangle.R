#### Function for SVR data wrangling

data_svr_wrangle <- function(data_array = data_array){

        # Create training list for each forcast ahead
        x_train <- data_array$x_train_array %>% as_tibble()
        if(dim(data_array$y_train_array)[2] > 1){y_train <- data_array$y_train_array[,,1] %>% split(., rep(1:ncol(.), each = nrow(.)))
        train <- map(y_train, ~cbind(Y=.x, x_train))
        } else {train <- list(cbind(Y = data_array$y_train_array[,,1], x_train))}


        # Create validation list for each forecast ahead

        x_val <- data_array$x_val_array %>% as_tibble()
        if(dim(data_array$y_val_array)[2] > 1){y_train <- data_array$y_val_array[,,1] %>% split(., rep(1:ncol(.), each = nrow(.)))
        validation <- map(y_val, ~cbind(Y=.x, x_val))
        } else {validation <- list(cbind(Y = data_array$y_val_array[,,1], x_val))}

        return(list(train_list = train, val_list = validation))
}
