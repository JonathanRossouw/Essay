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
