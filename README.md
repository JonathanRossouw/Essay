# Purpose

This README is for my Fin Metrics research essay. The aim of the essay
is determine whether volatility forecasting can be improved through the
use of Machine Learning techniques using South African financial data.
The baseline model used will be a GARCH model which is to be compared to
Support Vector Regression and Long-Short Term Memory Recurrent Neural
Network. Squared returns data will be used as the measure for
volatility. Observations will consist of 10 days volatility which
corresponds to two weeks. One day ahead and three day ahead forecasts
will be made. The data is to be split into training, validation and test
sets. Training data will consist of from 2009-2016, validation from
2017-2018, and test for 2019.

``` r
rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
```

    ##          used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
    ## Ncells 422376 22.6     873334 46.7         NA   666908 35.7
    ## Vcells 804567  6.2    8388608 64.0     102400  1824439 14.0

``` r
library(tidyverse)
pacman::p_load(rugarch, cowplot, tbl2xts, fmxdat, ggplot2, sugrrants, kableExtra, rsample, glue, tictoc)
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% .[!grepl('Old_code', .)] %>% as.list() %>% walk(~source(.))

# Set whether cached hyperparameter tuning results are used
cache <- TRUE
```

# Data Split

The data used in the practical is the total return index (TRI) of the
ALSI. This is price adjusted for dividends, stock splits and other
corporate actions. The data is split into training, validation and test
series. The training set is from 05-01-2009 to 30-12-2016, the
validation set is from 03-01-2017 to 31-12-2018, and the test set is
from 03-01-2019 to 31-12-2019. Returns, dlogret, are calculated using
log difference of TRI, log(TRI) - log(lag(TRI)). The volatility, sigma,
is calculated as dlogret^2.

``` r
#### Data Wrangling Function

data_wrangling_func <- function(data, start_date, final_date, type = "normal"){
    # Filter dates and replace NAs with 0
    data <- data  %>% filter(date < as.Date(final_date))
    # Create dlog returns, volatility given by 5 day moving average of squared dlog returns
    data <- data %>%
        filter(date > as.Date(start_date) & date < as.Date(final_date)) %>%
        mutate(dlogret = log(TRI) - lag(log(TRI))) %>%
        filter(date > first(date)) %>%
        mutate(y = dlogret^2) %>%
        mutate(y = zoo::rollmean(y, 5, fill = NA, align = 'right')) %>%
        tail(-4)
    # Create AR(1) residuals for sigma
    ar <- arima(data$dlogret, order = c(1,0,0))
    data <- data %>% mutate(sigma = ar$residuals^2)

    if(type == "scaled"){
        # Rescale Min Max
        data <- data %>%
            mutate(sigma = (sigma - min(sigma))/(max(sigma) - min(sigma))) %>%
            mutate(y = (y - min(y))/(max(y) - min(y))) %>%
            select(date, y, sigma)
        return(data)
    }
    data
}
```

``` r
data <- fmxdat::Jalshtr

# Wranlge Data and Create data sets
data_train <- data_wrangling_func(data = data, start_date = "2009-01-01", final_date = "2017-01-01")
  
data_val <- data_wrangling_func(data = data, start_date = "2017-01-01", final_date = "2019-01-01")

data_test <- data_wrangling_func(data = data, start_date = "2019-01-01", final_date ="2020-01-01")

# Create Min-Max Scaled Data Sets

data_train_scaled <- data_wrangling_func(data = data, 
                                  start_date = "2009-01-01", 
                                  final_date = "2017-01-01", 
                                  type = "scaled")
  
data_val_scaled <- data_wrangling_func(data = data, 
                                start_date = "2017-01-01", 
                                final_date = "2019-01-01", 
                                type = "scaled")

data_train_full_scaled <- data_wrangling_func(data = data, 
                                start_date = "2009-01-01", 
                                final_date = "2019-01-01", 
                                type = "scaled")

data_test_scaled <- data_wrangling_func(data = data, 
                                 start_date = "2019-01-01", 
                                 final_date ="2020-01-01", 
                                type = "scaled")
```

# GARCH Model

``` r
# Test for conditional heteroskedasticity
test_cond_het_func <- function(data = data){

    Plotdata <- data %>%
        mutate(Returns = dlogret,
               Returns_Sqd = dlogret^2,
               Returns_Abs = abs(dlogret))

    Plotdata <- Plotdata %>%
        pivot_longer(c("Returns", "Returns_Sqd", "Returns_Abs"), names_to = "ReturnType", values_to = "Returns")

    ret_plot <- Plotdata %>% ggplot() +
        geom_line(aes(x = date, y = Returns, colour = ReturnType), alpha = 0.8) +

        ggtitle("Return Type Persistence") +
        facet_wrap(~ReturnType, nrow = 3, ncol = 1, scales = "free") +

        guides(alpha = "none", colour = "none") +
        fmxdat::theme_fmx()

    acf_1 <- Plotdata %>%
        ggplot() + geom_acf(aes(x = ..lag.., y = dlogret)) + theme_bw() + labs(subtitle = "ACF of Dlog Ret", y = "")
    acf_2 <- Plotdata %>%
        ggplot() + geom_acf(aes(x = ..lag.., y = dlogret^2)) + theme_bw() + labs(subtitle = "ACF of Sqaured Dlog Ret", y = "")
    acf_3 <- Plotdata %>%
        ggplot() + geom_acf(aes(x = ..lag.., y = abs(dlogret))) + theme_bw() + labs(subtitle = "ACF of Absolute value of Dlog Ret", y = "")

    acf_plots <- plot_grid(acf_1, acf_2, acf_3, nrow = 1)

    box_stats <- Box.test(data$dlogret^2, type = "Ljung-Box", lag = 12)

    out <- list(`Return Plots` = ret_plot,
                `ACF Plots` = acf_plots,
                `Box Statistics` = box_stats)
    out

}

# Select Best Garch Model
vol_select_func <- function(data = data){

    data <- data %>% select(date, dlogret) %>%  tbl_xts()
    models = 1:4
    model.list = list()

    for (p in models) {

        garchfit = ugarchspec(
            variance.model = list(model = c("sGARCH","gjrGARCH","eGARCH","apARCH")[p], garchOrder = c(1, 1)),

            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),

            distribution.model = c("norm", "snorm", "std", "sstd", "ged", "sged", "nig", "ghyp", "jsu")[1])

        garchfit1 = ugarchfit(spec = garchfit,data=as.numeric(data))

        model.list[[p]] = garchfit1
    }

    names(model.list) <- c("sGARCH","gjrGARCH","eGARCH","apARCH")

    fit.mat = sapply(model.list, infocriteria)
    # Note: sapply can apply a function (infocriteria here) to a list...

    rownames(fit.mat) = rownames(infocriteria(model.list[[1]]))

    fit.mat

}

# Fit GARCH function
vol_func <- function(data = data, model = "sGARCH"){

    data <- data %>% select(date, dlogret) %>%  tbl_xts()

    gjrgarch11 = ugarchspec(variance.model = list(model = model,

                                                  garchOrder = c(1, 1)),

                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),

                            distribution.model = c("norm", "snorm", "std", "sstd", "ged", "sged", "nig", "ghyp", "jsu")[3])
    # Now to fit, I use as.matrix and the data - this way the plot functions we will use later will work.
    garchfit2 = ugarchfit(spec = gjrgarch11, data = as.matrix(data))

    garchfit2
}

# Plot Smoothed Ret^2
vol_plot_func <- function(data = data, fit = garch_fit, title){
    # To view the conditional variance plot, use:
    sigma <- fit

    gg <- data %>%

        ggplot() +
        geom_line(aes(date, y), alpha = 0.8) +
        geom_line(data = sigma, aes(x = date, y = sigma), color = "red", size = 1.2, alpha = 0.8) +
        labs(title = title, x = "", y = "Comparison of estimated volatility") +
        fmxdat::theme_fmx(title = ggpts(25))
    fmxdat::finplot(gg, y.pct_acc = 1)

}
```

Fit GARCH Models

``` r
# Test for GARCH effects
cond_het <- test_cond_het_func(data_train)
cond_het
```

    ## $`Return Plots`

![](README_files/figure-markdown_github/garch-1.png)

    ## 
    ## $`ACF Plots`

![](README_files/figure-markdown_github/garch-2.png)

    ## 
    ## $`Box Statistics`
    ## 
    ##  Box-Ljung test
    ## 
    ## data:  data$dlogret^2
    ## X-squared = 632.57, df = 12, p-value < 2.2e-16

``` r
# Find best model
best_mod <- vol_select_func(data_train)
best_mod
```

    ##                 sGARCH  gjrGARCH    eGARCH    apARCH
    ## Akaike       -6.452107 -6.482330 -6.490124 -6.476212
    ## Bayes        -6.438554 -6.466067 -6.473860 -6.457238
    ## Shibata      -6.452119 -6.482347 -6.490140 -6.476235
    ## Hannan-Quinn -6.447141 -6.476371 -6.484164 -6.469259

``` r
# Train

# Fit Model
garch_fit <- vol_func(data_train, "eGARCH")
# Model Coefficients
garch_fit@fit$matcoef
```

    ##             Estimate   Std. Error       t value     Pr(>|t|)
    ## mu      0.0003515747 2.130849e-04     1.6499274 9.895778e-02
    ## ar1     0.0125580135 3.000059e-02     0.4185922 6.755142e-01
    ## omega  -0.1158799416 3.678887e-03   -31.4986395 0.000000e+00
    ## alpha1 -0.1229697781 1.276882e-02    -9.6304719 0.000000e+00
    ## beta1   0.9876109595 3.831107e-05 25778.7351700 0.000000e+00
    ## gamma1  0.0827979223 8.778569e-03     9.4318240 0.000000e+00
    ## shape  11.8303799861 2.471258e+00     4.7871886 1.691339e-06

``` r
# Plot Model
vol_plot_func(data = data_train_scaled, 
              fit = sigma(garch_fit) %>% xts_tbl()%>% 
                rename(sigma = coredata.xts.) %>% 
                mutate(date = as.Date(date)) %>% 
                mutate(sigma = (sigma - min(data_train$y))/(max(data_train$y) - min(data_train$y))/100),
              title = "Comparison: Returns Sigma vs Sigma from Garch")
```

![](README_files/figure-markdown_github/garch-3.png)

``` r
garch_mse <- mean((sigma(garch_fit) %>% xts_tbl()%>% 
                rename(sigma = coredata.xts.) %>% 
                mutate(date = as.Date(date)) %>% 
                mutate(sigma = (sigma - min(data_train$y))/(max(data_train$y) - min(data_train$y))/100)%>% pull(sigma) - data_train_scaled %>% pull(y))^2)

# Val

# Fit Model
garch_fit_val <- vol_func(data_val, "eGARCH")
# Model Coefficients
garch_fit_val@fit$matcoef
```

    ##             Estimate   Std. Error      t value     Pr(>|t|)
    ## mu      0.0002864073 0.0003012782    0.9506407 3.417868e-01
    ## ar1     0.0264570892 0.0442049551    0.5985096 5.495000e-01
    ## omega  -0.3238038491 0.0070088366  -46.1993718 0.000000e+00
    ## alpha1 -0.1486355485 0.0223898006   -6.6385383 3.168088e-11
    ## beta1   0.9667674863 0.0001089335 8874.8383943 0.000000e+00
    ## gamma1  0.0614327405 0.0272134808    2.2574378 2.398073e-02
    ## shape   8.2762798876 2.8918533902    2.8619293 4.210709e-03

``` r
# Plot Model
vol_plot_func(data = data_val_scaled, 
              fit = sigma(garch_fit_val) %>% xts_tbl()%>% 
                rename(sigma = coredata.xts.) %>% 
                mutate(date = as.Date(date)) %>% 
                mutate(sigma = (sigma - min(data_val$y))/(max(data_val$y) - min(data_val$y))/100),
              title = "Comparison: Returns Sigma vs Sigma from Garch")
```

![](README_files/figure-markdown_github/garch-4.png)

``` r
garch_mse <- mean((sigma(garch_fit_val) %>% xts_tbl()%>% 
                rename(sigma = coredata.xts.) %>% 
                mutate(date = as.Date(date)) %>% 
                mutate(sigma = (sigma - min(data_val$y))/(max(data_val$y) - min(data_val$y))/100) %>% pull(sigma) - data_val_scaled %>% pull(y))^2)
```

# LSTM Model

Connect to python virtual environment with reticulate. Install and load
tensorflow and keras.

``` r
pacman::p_load(reticulate)

reticulate::use_virtualenv("/Users/jonathanrossouw/Desktop/Masters/Dat Sci/ML/Project/.venv", require = TRUE)
# reticulate::py_config()
# Install Tensorflow and Keras in virtual environment
### virtualenv_install(".venv/", "tensorflow")
### install_tensorflow()
### install_keras()
# Load Tensorflow and Keras
library(tensorflow)
library(keras)
```

Wrangle data for LSTM. Use min-max scaling on series. Store min-max
values for later rescaling. Plot new scaled training data.

## One Day Forecast

Create lists of arrays with correct structure for LSTM model. The
previous two days return and two days volatility are used as inputs with
a single day???s volatility being predicted.

``` r
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
```

``` r
### Create LSTM Data Arrays
data_lstm_lists <- data_array_func(data = data_train_scaled, 
                              val = data_val_scaled,
                              initial = 1, 
                              assess = 1, 
                              skip = 0, 
                              type = "one_day")
```

Create grid of hyperparameters for tuning. Epochs, number of LSTM
layers, number of LSTM units, whether or not there is a return sequence
and the dropout rate are used as hyperparameters.

``` r
##### Set hyperparameter grid
lstm_hyp <- expand.grid(loss = c("mse", "mae"),
                        optimizer = c("adam"),
                        epochs = c(10, 20, 50),
                        lstm_layers = c(1, 2),
                        lstm_units = c(10, 20, 40),
                        return_sequences = c(TRUE, FALSE),
                        dropout_rate = c(0, 0.1)) %>%
    filter(!(return_sequences == TRUE & lstm_layers == 1)) %>%
    split(., seq(nrow(.)))
```

Using a grid search, train the LSTM model and predict the one-day ahead
forecast for the validation set. Select the combination of
hyperparameters that result in the lowest MSE for the validation set.
Plot the training data and forecast and plot the validation set and the
forecast.

``` r
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
```

``` r
#### Fit the best univariate LSTM according to the results of 
# the hyperparameter tuning
if(!cache){
  tic()
lstm_fit <- hyp_tune_func(data_lists = data_lstm_lists,
                         data_actual = data_val_scaled %>% tail(-1), 
                         hyperparams = lstm_hyp, 
                         fit_func = lstm_fit_func,
                         type = "lstm")
toc()
write.csv(lstm_fit, "cache/lstm_tune")
}

if(cache){
  lstm_fit <- read.csv("cache/lstm_tune", row.names = 1) %>% as_tibble()
}

best_lstm_tune <- lstm_fit %>%  
  filter(mse == min(mse))

lstm_perf <- perf_plot(data_lists = data_lstm_lists,
                       data_val = data_val_scaled %>% tail(-1), 
                       data_train = data_train_scaled %>% tail(-1),
                       hyperparams = best_lstm_tune,
                       fit_func = lstm_fit_func,
                       type = "lstm",
                       train_title = "LSTM One-Day Ahead Train Plot",
                       val_title = "LSTM One-Day Ahead Validation Plot")

lstm_perf$train_plot
```

![](README_files/figure-markdown_github/lstm_one_day-1.png)

``` r
lstm_perf$training_error
```

    ## # A tibble: 1 ?? 8
    ##   loss  optimizer epochs lstm_layers lstm_units return_sequences dropout_rate
    ##   <chr> <chr>      <int>       <int>      <int> <lgl>                   <dbl>
    ## 1 mse   adam          20           2         40 TRUE                        0
    ## # ??? with 1 more variable: mse <dbl>

``` r
lstm_perf$val_plot
```

![](README_files/figure-markdown_github/lstm_one_day-2.png)

``` r
lstm_perf$performance
```

    ## # A tibble: 1 ?? 8
    ##   loss  optimizer epochs lstm_layers lstm_units return_sequences dropout_rate
    ##   <chr> <chr>      <int>       <int>      <int> <lgl>                   <dbl>
    ## 1 mse   adam          20           2         40 TRUE                        0
    ## # ??? with 1 more variable: mse <dbl>

## Three Day Forecast

Create lists of arrays with correct structure for LSTM model. The
previous four days return and four days volatility are used as inputs
with the following three day???s volatility being forecast.

``` r
### Create LSTM Data Arrays
data_lstm_lists_3 <- data_array_func(data = data_train_scaled, 
                              val = data_val_scaled,
                              initial = 2,
                              assess = 1,
                              skip = 0, 
                              type = "three_day")
```

Create grid of hyperparameters for tuning. Epochs, number of LSTM
layers, number of LSTM units, whether or not there is a return sequence
and the dropout rate are used as hyperparameters.

``` r
##### Set hyperparameter grid
lstm_hyp_3 <- expand.grid(loss = c("mse", "mae"),
                        optimizer = c("adam"),
                        epochs = c(10, 20, 50),
                        lstm_layers = c(1, 2),
                        lstm_units = c(10, 20, 40),
                        return_sequences = c(TRUE, FALSE),
                        dropout_rate = c(0, 0.1)) %>%
    filter(!(return_sequences == TRUE & lstm_layers == 1)) %>%
    split(., seq(nrow(.)))
```

Using a grid search, train the LSTM model and predict the three-day
ahead forecast for the validation set. Select the combination of
hyperparameters that result in the lowest MSE for the validation set.
Plot the training data and forecast and plot the validation set and the
forecast.

``` r
#### Fit the best univariate LSTM according to the results of 
# the hyperparameter tuning
if(!cache){
  tic()
lstm_fit_3 <- hyp_tune_func(data_lists = data_lstm_lists_3,
                         data_actual = data_val_scaled %>% tail(-4), 
                         hyperparams = lstm_hyp_3, 
                         fit_func = lstm_fit_func,
                         type = "lstm")
write.csv(lstm_fit, "cache/lstm_tune_3")
toc()
}

if(cache){
  lstm_fit_3 <- read.csv("cache/lstm_tune_3", row.names = 1) %>% as_tibble()
}

best_lstm_tune_3 <- lstm_fit_3 %>%  
  filter(mse == min(mse))

lstm_perf_3 <- perf_plot(data_lists = data_lstm_lists_3,
                         data_train = data_train_scaled %>% tail(-4),
                         data_val = data_val_scaled %>% tail(-4), 
                       hyperparams = best_lstm_tune_3,
                       fit_func = lstm_fit_func,
                       type = "lstm",
                       train_title = "LSTMThree-Day Ahead Train Plot",
                       val_title = "LSTM Three-Day Ahead Validation Plot")

lstm_perf_3$train_plot
```

![](README_files/figure-markdown_github/lstm_3-1.png)

``` r
lstm_perf_3$training_error
```

    ## # A tibble: 1 ?? 8
    ##   loss  optimizer epochs lstm_layers lstm_units return_sequences dropout_rate
    ##   <chr> <chr>      <int>       <int>      <int> <lgl>                   <dbl>
    ## 1 mse   adam          20           2         40 TRUE                        0
    ## # ??? with 1 more variable: mse <dbl>

``` r
lstm_perf_3$val_plot
```

![](README_files/figure-markdown_github/lstm_3-2.png)

``` r
lstm_perf_3$performance
```

    ## # A tibble: 1 ?? 8
    ##   loss  optimizer epochs lstm_layers lstm_units return_sequences dropout_rate
    ##   <chr> <chr>      <int>       <int>      <int> <lgl>                   <dbl>
    ## 1 mse   adam          20           2         40 TRUE                        0
    ## # ??? with 1 more variable: mse <dbl>

# SVR

Load in the support vector machine package. Create the list of arrays
where the previous two day???s returns and volatility are used as the
variables and the next days volatility is the target. Create data frames
from the arrays to be used by the SVM.

## One Day Forecast

``` r
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
```

``` r
library(liquidSVM)

## One Day Forecast
# Create list of training and validation sets

data_svr_array <- data_array_func(data = data_train_scaled,
                                  val = data_val_scaled,
                                  initial = 1,
                                  assess = 1,
                                  skip = 0, 
                                  type = "one_day")

data_svr_lists <- data_svr_wrangle(data_array = data_svr_array)
```

Create a grid of hyperparameters. The gamma and lambda are tuned over.
Using a grid-search, fit the SVM to the training data and forecast the
one-day ahead volatility from the validation set.

``` r
# Support Vector Regression fit function

svr_fit_func <- function(hyperparams, data_lists){
    set.seed(12439078)
    SVR_fit_list <- list()
    SVR_pred_train <- list()
    SVR_pred_val <- list()
    for(i in 1:length(data_lists$train_list)){
        SVR_fit_list[[i]] <- svmRegression(x = data_lists$train_list[[i]][,-1],
                                           y = data_lists$train_list[[i]][,"Y"],
                                           gamma = hyperparams$gamma,
                                           lambda = hyperparams$lambda)

        SVR_pred_train[[i]] <- predict(SVR_fit_list[[i]], data_lists$train_list[[i]][,-1])
        SVR_pred_val[[i]] <- predict(SVR_fit_list[[i]], data_lists$val_list[[i]][,-1])
        print(paste(i,"DONE!!"))
    }

    SVR_train_df <- SVR_pred_train %>%  do.call("cbind", .) %>% t(.) %>% c(.)
    SVR_val_df <- SVR_pred_val %>%  do.call("cbind", .) %>% t(.) %>% c(.)
    print("DONE!!")
    list(train = SVR_train_df, val = SVR_val_df)
}
```

``` r
# Tune SVR regressions
svr_hyp <- expand.grid(gamma = c(exp(seq(-2.5, 8, 1.5))),
                       lambda = c(exp(seq(-2.5, 8, 1.5)))) %>%
    split(., seq(nrow(.)))
```

Select the combination of hyperparameters that resulted in the lowest
MSE of the validation set. Create plots of the training data and
forecasts and the valdiation data and forecasts. Determine forcast
performance with MSE.

``` r
if(!cache){
  tic()
  svr_fit <- hyp_tune_func(data_lists = data_svr_lists,
                         data_actual = data_val_scaled %>% tail(-1),
                         hyperparams = svr_hyp,
                         fit_func = svr_fit_func, 
                         type = "svr")
  write.csv(svr_fit, "cache/svr_tune")
  toc()
}

if(cache){
  svr_fit <- read.csv("cache/svr_tune", row.names = 1) %>% as_tibble()
}

# Best hyperparameters

best_tune_svr <- svr_fit %>% filter(mse == min(mse))

# Plot performance
svr_perf <- perf_plot(data_lists = data_svr_lists,
                      data_train = data_train_scaled %>% tail(-1),
                      data_val = data_val_scaled %>% tail(-1), 
                      hyperparams = best_tune_svr[1,],
                      fit_func = svr_fit_func,
                      type = "svr",
                      train_title = "SVR One-Day Ahead Train Plot",
                      val_title = "SVR One-Day Ahead Validation Plot")
```

    ## [1] "1 DONE!!"
    ## [1] "DONE!!"

``` r
svr_perf$train_plot
```

![](README_files/figure-markdown_github/svr_eval_perf-1.png)

``` r
svr_perf$training_error
```

    ## # A tibble: 1 ?? 3
    ##    gamma lambda     mse
    ##    <dbl>  <dbl>   <dbl>
    ## 1 0.0821 0.0821 0.00342

``` r
svr_perf$val_plot
```

![](README_files/figure-markdown_github/svr_eval_perf-2.png)

``` r
svr_perf$performance
```

    ## # A tibble: 1 ?? 3
    ##    gamma lambda     mse
    ##    <dbl>  <dbl>   <dbl>
    ## 1 0.0821 0.0821 0.00968

## Three Day Forecast

Create the list of arrays where the previous four day???s returns and
volatility are used as the variables and the next three days??? volatility
is the target. Create data frames from the arrays to be used by the SVM.

``` r
# Create list of training and validation sets

data_svr_array_3 <- data_array_func(data = data_train_scaled,
                                  val = data_val_scaled,
                                  initial = 2,
                                  assess = 1,
                                  skip = 0, 
                                  type = "three_day")

data_svr_lists_3 <- data_svr_wrangle(data_array = data_svr_array_3)
```

Create a grid of hyperparameters. The gamma and lambda are tuned over.
Using a grid-search, fit the SVM to the training data and forecast the
three-day ahead volatility from the validation set.

``` r
# Tune SVR regressions
svr_hyp_3 <- expand.grid(gamma = c(exp(seq(-2.5, 8, 1.5))),
                       lambda = c(exp(seq(-2.5, 8, 1.5)))) %>%
    split(., seq(nrow(.)))
```

Select the combination of hyperparameters that resulted in the lowest
MSE of the validation set. Create plots of the training data and
forecasts and the validation data and forecasts. Determine forecast
performance with MSE.

``` r
if(!cache){
  tic()
  svr_fit_3 <- hyp_tune_func(data_lists = data_svr_lists_3,
                         data_actual = data_val_scaled %>% tail(-4),
                         hyperparams = svr_hyp_3,
                         fit_func = svr_fit_func, 
                         type = "svr")
  write.csv(svr_fit_3, "cache/svr_tune_3")
  toc()
}

if(cache){
  svr_fit_3 <- read.csv("cache/svr_tune_3", row.names = 1) %>% as_tibble()
}

# Best hyperparameters

best_tune_svr_3 <- svr_fit_3 %>% filter(mse == min(mse))

# Plot performance
svr_perf_3 <- perf_plot(data_lists = data_svr_lists_3,
                         data_train = data_train_scaled %>% tail(-4),
                         data_val = data_val_scaled %>% tail(-4), 
                      hyperparams = best_tune_svr_3[1,],
                      fit_func = svr_fit_func,
                      type = "svr",
                      train_title = "SVR Three-Day Ahead Train Plot",
                      val_title = "SVR Three-Day Ahead Validation Plot")
```

    ## [1] "1 DONE!!"
    ## [1] "DONE!!"

``` r
svr_perf_3$train_plot
```

![](README_files/figure-markdown_github/svr_3_eval_perf-1.png)

``` r
svr_perf_3$training_error
```

    ## # A tibble: 1 ?? 3
    ##    gamma lambda     mse
    ##    <dbl>  <dbl>   <dbl>
    ## 1 0.0821 0.0821 0.00813

``` r
svr_perf_3$val_plot
```

![](README_files/figure-markdown_github/svr_3_eval_perf-2.png)

``` r
svr_perf_3$performance
```

    ## # A tibble: 1 ?? 3
    ##    gamma lambda    mse
    ##    <dbl>  <dbl>  <dbl>
    ## 1 0.0821 0.0821 0.0462

# Test

Combine training and validation set. Fit model using the best
combination of hyperparameters. Forecast using the test set and
determine performance.

## One Day Forecasts

``` r
### Create LSTM Data Arrays
data_lstm_lists_test <- data_array_func(data = data_train_full_scaled, 
                              val = data_test_scaled,
                              initial = 1, 
                              assess = 1, 
                              skip = 0, 
                              type = "one_day")

# Fit Model and Evaluate Performance
lstm_perf_test <- perf_plot(data_lists = data_lstm_lists_test,
                       data_val = data_test_scaled %>% tail(-1), 
                       data_train = data_train_full_scaled %>% tail(-1),
                       hyperparams = best_lstm_tune,
                       fit_func = lstm_fit_func,
                       type = "lstm",
                       train_title = "LSTM One-Day Ahead Full Train Plot",
                       val_title = "LSTM One-Day Ahead Test Plot")

lstm_perf_test$train_plot
```

![](README_files/figure-markdown_github/test_one_day-1.png)

``` r
lstm_perf_test$training_error
```

    ## # A tibble: 1 ?? 8
    ##   loss  optimizer epochs lstm_layers lstm_units return_sequences dropout_rate
    ##   <chr> <chr>      <int>       <int>      <int> <lgl>                   <dbl>
    ## 1 mse   adam          20           2         40 TRUE                        0
    ## # ??? with 1 more variable: mse <dbl>

``` r
lstm_perf_test$val_plot
```

![](README_files/figure-markdown_github/test_one_day-2.png)

``` r
lstm_perf_test$performance
```

    ## # A tibble: 1 ?? 8
    ##   loss  optimizer epochs lstm_layers lstm_units return_sequences dropout_rate
    ##   <chr> <chr>      <int>       <int>      <int> <lgl>                   <dbl>
    ## 1 mse   adam          20           2         40 TRUE                        0
    ## # ??? with 1 more variable: mse <dbl>

## Three Day Forecasts

``` r
# Create list of training and validation sets

data_svr_array_3_test <- data_array_func(data = data_train_full_scaled,
                                  val = data_test_scaled,
                                  initial = 2,
                                  assess = 1,
                                  skip = 0, 
                                  type = "three_day")

data_svr_lists_3_test <- data_svr_wrangle(data_array = data_svr_array_3_test)

# Plot performance
svr_perf_3_test <- perf_plot(data_lists = data_svr_lists_3_test,
                         data_train = data_train_full_scaled %>% tail(-4),
                         data_val = data_test_scaled %>% tail(-4), 
                      hyperparams = best_tune_svr_3[1,],
                      fit_func = svr_fit_func,
                      type = "svr",
                      train_title = "SVR Three-Day Ahead Full Training Plot",
                      val_title = "SVR Three-Day Ahead Test Plot")
```

    ## Warning in selectSVMs(model): Solution may not be optimal: try training again
    ## using min_gamma=0.04

    ## [1] "1 DONE!!"
    ## [1] "DONE!!"

``` r
svr_perf_3_test$train_plot
```

![](README_files/figure-markdown_github/test_three_day-1.png)

``` r
svr_perf_3_test$training_error
```

    ## # A tibble: 1 ?? 3
    ##    gamma lambda     mse
    ##    <dbl>  <dbl>   <dbl>
    ## 1 0.0821 0.0821 0.00372

``` r
svr_perf_3_test$val_plot
```

![](README_files/figure-markdown_github/test_three_day-2.png)

``` r
svr_perf_3_test$performance
```

    ## # A tibble: 1 ?? 3
    ##    gamma lambda    mse
    ##    <dbl>  <dbl>  <dbl>
    ## 1 0.0821 0.0821 0.0523
