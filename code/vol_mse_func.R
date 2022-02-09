# Volatility MSE function

vol_mse_func <- function(fit, pred){
    sigma <- sigma(fit) %>% xts_tbl()
    colnames(sigma) <- c("date", "sigma")
    sigma <- sigma %>% mutate(date = as.Date(date)) %>% filter(date > first(date))

    sigma <- sigma %>% left_join(., pred %>% select(date, vol), by = "date")

    sigma_mse <- sigma %>% summarise(MSE = mean((sigma - vol)^2, na.rm = T))
    sigma_mse
}
