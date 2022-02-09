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
