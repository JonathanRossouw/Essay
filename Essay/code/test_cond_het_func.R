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
