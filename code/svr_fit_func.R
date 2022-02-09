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
