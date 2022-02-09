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
