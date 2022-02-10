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
