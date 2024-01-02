
# Library -----------------------------------------------------------------
library(tidyverse)
library(caret)
library(mice)

# User functions ----------------------------------------------------------
## Custom fun
paste_boot_ci <- function(x) {
    # med <- round(median(x), 2)
    q1  <- round(quantile(x, probs = 0.025), 2)
    q3  <- round(quantile(x, probs = 0.975), 2)
    
    return(paste0("(", q1, ", ", q3, ")"))
}


# Data Input --------------------------------------------------------------
df_covid <- read_csv("input/df_icu_adm_COVID_RespSupport_first24h_selected_2020_2021.csv")

df_covid_model <- 
    df_covid %>% 
    mutate(
        rrt_outcome = factor(ResourceIsRenalReplacementTherapy, 
                             levels = c(0, 1), labels = c("negative", "positive"))
    ) %>%
    select(rrt_outcome, 
           Age, 
           Gender, 
           MFI_level, 
           Urea,
           LowestGlasgowComaScale1h, 
           LowestPlateletsCount1h, 
           # HighestBilirubin1h,
           HighestCreatinine1h,
           diabetes, 
           chronic_kidney, 
           hypertension, 
           IsNonInvasiveVentilation1h, 
           IsMechanicalVentilation1h, 
           IsVasopressors1h
           ) %>% 
    mutate_at(vars(starts_with("Is")), 
              function(x) { case_when(x == 1 ~ "yes", TRUE ~ "no")} ) %>% 
    mutate_at(c("diabetes", "chronic_kidney", "hypertension"),
              function(x) { case_when(x == 1 ~ "yes", TRUE ~ "no")} 
    ) %>% 
    # mutate_at(vars(starts_with("Is")), 
    #           function(x) { if_else(x == 1, "yes", "no")} ) %>% 
    # mutate_at(c("diabetes", "chronic_kidney", "hypertension"),
    #           function(x) { if_else(x == 1, "yes", "no")} 
    # ) %>% 
    mutate_if(is.character, as.factor) %>% 
    rename(
        IsNonInvasiveVentilation = IsNonInvasiveVentilation1h, 
        IsMechanicalVentilation = IsMechanicalVentilation1h,
        IsVasopressors = IsVasopressors1h 
    ) %>% 
    mutate(
        MFI_level = factor(MFI_level, levels = c("non_frail", "pre_frail","frail"))
    )
    # %>% 
    # drop_na()

# Data Pre-processing -----------------------------------------------------

## Making data split (randomly selected rows at 80/20 train/test ratio)
set.seed(2^31-1)
trainIndex <- createDataPartition(df_covid_model$rrt_outcome, 
                                  p = .8, 
                                  list = FALSE, 
                                  times = 1)

df_covid_train <- df_covid_model[trainIndex, ]
df_covid_test <- df_covid_model[-trainIndex, ]


## Imputing train set
df_covid_train_imp <- 
    mice(df_covid_train, m = 30) %>% 
    complete(., "long") %>% 
    as_tibble() %>%
    group_by(.id) %>% 
    mutate(
        HighestCreatinine1h      = mean(HighestCreatinine1h),
        # HighestBilirubin1h       = mean(HighestBilirubin1h),
        LowestPlateletsCount1h   = mean(LowestPlateletsCount1h),
        LowestGlasgowComaScale1h = mean(LowestGlasgowComaScale1h)
    ) %>% 
    ungroup() %>% 
    filter(.imp == 1) %>% 
    select(-c(.imp, .id))

write_csv(df_covid_train_imp, "output/df_covid_train_imp.csv")


## Imputing test set
df_covid_test_imp <- 
    mice(df_covid_test, m = 30) %>% 
    complete(., "long") %>% 
    as_tibble() %>%
    group_by(.id) %>% 
    mutate(
        HighestCreatinine1h      = mean(HighestCreatinine1h),
        # HighestBilirubin1h       = mean(HighestBilirubin1h),
        LowestPlateletsCount1h   = mean(LowestPlateletsCount1h),
        LowestGlasgowComaScale1h = mean(LowestGlasgowComaScale1h)
    ) %>% 
    ungroup() %>% 
    filter(.imp == 1) %>% 
    select(-c(.imp, .id))


## Checking correlated variables
cor(df_covid_train_imp %>% 
        select(Age, Urea, LowestGlasgowComaScale1h,
               LowestPlateletsCount1h,
               HighestCreatinine1h), method = "spearman")


## Creting pre-processing object
data_pre_process_train <- preProcess(df_covid_train_imp %>% select(-rrt_outcome),
                                     method = c("range", "nzv", "corr")) 


write_rds(data_pre_process_train, "input/data_pre_process_train.rds")

## Pre-processed data frames
df_covid_train_process <- predict(data_pre_process_train, df_covid_train_imp)
df_covid_test_process  <- predict(data_pre_process_train, df_covid_test_imp)


##### Adding custom fuction of summary metrics #####
BigSummary <- function (data, lev = NULL, model = NULL) {
    
    pr_auc <- try(MLmetrics::PRAUC(data[, lev[2]],
                                   ifelse(data$obs == lev[2], 1, 0)),
                  silent = TRUE)
    brscore <- try(mean((data[, lev[2]] - ifelse(data$obs == lev[2], 1, 0)) ^ 2),
                   silent = TRUE)
    rocObject <- try(pROC::roc(ifelse(data$obs == lev[2], 1, 0), data[, lev[2]],
                               direction = "<", quiet = TRUE), silent = TRUE)
    if (inherits(pr_auc, "try-error")) pr_auc <- NA
    if (inherits(brscore, "try-error")) brscore <- NA
    rocAUC <- if (inherits(rocObject, "try-error")) {
        NA
    } else {
        rocObject$auc
    }
    tmp <- unlist(e1071::classAgreement(table(data$obs,
                                              data$pred)))[c("diag", "kappa")]
    out <- c(Acc = tmp[[1]],
             # Kappa = tmp[[2]],
             AUCROC = rocAUC,
             AUCPR = pr_auc,
             Brier = brscore,
             # logLoss = caret:::mnLogLoss(data, lev = lev, model = model),
             # Precision = caret:::precision.default(data = data$pred,
             #                                       reference = data$obs,
             #                                       relevant = lev[2]),
             # Recall = caret:::recall.default(data = data$pred,
             #                                 reference = data$obs,
             #                                 relevant = lev[2]),
             F_score = caret:::F_meas.default(data = data$pred, reference = data$obs,
                                              relevant = lev[2]))
    out
}

## Variable selection
# rfe_stats <- function(...) {c(twoClassSummary(...), BigSummary(...))}
SummaryFuns <- function(...) {c(twoClassSummary(...), BigSummary(...), mnLogLoss(...))}
treebagFuncs$summary <- SummaryFuns
# caretFuncs$summary <- SummaryFuns


set.seed(2^31-1)
var_selection <- rfe(y = df_covid_train_process$rrt_outcome,
                     x = df_covid_train_process[, -1],
                     metric = "Brier", maximize = FALSE,
                     sizes = 1:(ncol(df_covid_train_process) - 1),
                     rfeControl = rfeControl(functions = treebagFuncs,
                                             method = "cv", number = 5,
                                             verbose = TRUE)
                     # returnResamp = "none"
                     )

selected_vars <- predictors(var_selection)

write_csv(tibble(variables = selected_vars), "output/selected_vars_rfe.csv")


df_covid_train_process <-
    df_covid_train_process %>% 
    select(rrt_outcome, selected_vars)
    # mutate(
    #     MFI_level = factor(MFI_level, levels = c("non_frail", "pre_frail","frail"))
    # )
    # mutate(
    #     rrt_outcome = factor(rrt_outcome, levels = c(0, 1), labels = c("X0", "X1"))
    # )



write_rds(df_covid_train_process, "input/df_covid_train_process.rds")

write_rds(df_covid_test_process %>% 
              select(rrt_outcome, selected_vars), "input/df_covid_test_process.rds")



###########################################################################
###########################################################################
###########################################################################
###########################################################################
# Model: Logistic Regression ----------------------------------------------
train_params_LR <- list(
        sampling_control = NULL,
        model_method = "glm",
        model_params = NULL
        )

train_control_LR <-  trainControl(method = "cv",
                                  number = 5,
                                  classProbs = TRUE,
                                  verboseIter = FALSE,
                                  summaryFunction = SummaryFuns,
                                  allowParallel = TRUE)


library(rms)



set.seed(2^31 - 1)
model_train_LR <- train(rrt_outcome ~ 
                            splines::ns(Age) + 
                            chronic_kidney +
                            diabetes + 
                            Gender +
                            # rms::rcs(HighestBilirubin1h) +
                            splines::ns(HighestCreatinine1h) +
                            hypertension +
                            IsMechanicalVentilation + 
                            IsNonInvasiveVentilation +
                            IsVasopressors +
                            splines::ns(LowestGlasgowComaScale1h) +
                            splines::ns(LowestPlateletsCount1h) +
                            MFI_level +
                            splines::ns(Urea),
                        data = df_covid_train_process,
                        method = train_params_LR$model_method,
                        family = "binomial",
                        metric = "Brier", 
                        maximize = FALSE,
                        trControl = train_control_LR,
                        tuneGrid = train_params_LR$model_params
                        )

write_rds(model_train_LR, "input/trained_models/model_train_LR.rds")


# Model: Logistic Regression  with regularization ------------------------
train_params_LR_reg <- list(sampling_control = NULL,
                        model_method = "glmnet",
                        model_params = 
                            expand.grid(
                                alpha = c(0, 1),
                                lambda = 10 ^ seq(2,-2,length = 100)
                                    )
                        )

train_control_LR_reg <-  trainControl(method = "cv",
                                  number = 5,
                                  classProbs = TRUE,
                                  verboseIter = FALSE,
                                  summaryFunction = SummaryFuns,
                                  allowParallel = TRUE)

set.seed(2^31 - 1)
model_train_LR_reg <- train(rrt_outcome ~ 
                                splines::ns(Age) + 
                                chronic_kidney +
                                diabetes + 
                                Gender +
                                # rms::rcs(HighestBilirubin1h) +
                                splines::ns(HighestCreatinine1h) +
                                hypertension +
                                IsMechanicalVentilation + 
                                IsNonInvasiveVentilation +
                                IsVasopressors +
                                splines::ns(LowestGlasgowComaScale1h) +
                                splines::ns(LowestPlateletsCount1h) +
                                MFI_level +
                                splines::ns(Urea),
                            data = df_covid_train_process,
                        method = train_params_LR_reg$model_method,
                        family = "binomial",
                        metric = "Brier",
                        maximize = FALSE,
                        trControl = train_control_LR_reg,
                        tuneGrid = train_params_LR_reg$model_params)

write_rds(model_train_LR_reg, "input/trained_models/model_train_LR_reg.rds")



# Model: XGBoosting ------------------------
train_params_XGB <- list(
    sampling_control = NULL,
    model_method = "xgbTree",
    model_params = expand.grid(
        nrounds   = seq(100, 500, 50),
        max_depth = c(2, 3, 4, 5, 6),
        eta       = c(0.05, 0.1),
        gamma     = c(0.01, 0.05),
        colsample_bytree = c(0.75),
        subsample        = c(0.50),
        min_child_weight = c(0)
        )
    )


train_control_XGB <-  trainControl(method = "cv",
                                  number = 5,
                                  classProbs = TRUE,
                                  verboseIter = FALSE,
                                  summaryFunction = SummaryFuns)

set.seed(2^31 - 1)
model_train_XGB <- train(
                         # y = df_covid_train_process %>% pull("rrt_outcome"),
                         # x = df_covid_train_process_dummy,
                         rrt_outcome ~ .,
                         data = df_covid_train_process,
                         method = train_params_XGB$model_method,
                         metric = "Brier", maximize = FALSE,
                         trControl = train_control_XGB,
                         verbose = F, 
                         tuneGrid = train_params_XGB$model_params)


write_rds(model_train_XGB, "input/trained_models/model_train_XGB.rds")


# Model: GBM ------------------------
train_params_GBM <- list(
    sampling_control = NULL,
    model_method = "gbm",
    model_params = expand.grid(
        n.trees = seq(100, 500, 50), 
        interaction.depth = c(1, 2), 
        shrinkage = c(0.001, 0.05, 0.1, 0.5),
        n.minobsinnode = c(5, 10, 20)
        )
    )



train_control_GBM <-  trainControl(method = "cv",
                                   number = 5,
                                   classProbs = TRUE,
                                   verboseIter = TRUE,
                                   summaryFunction = SummaryFuns)

set.seed(2^31 - 1)
model_train_GBM <- train(
    rrt_outcome ~ .,
    data = df_covid_train_process,
    method = train_params_GBM$model_method,
    metric = "Brier", maximize = FALSE,
    trControl = train_control_GBM,
    verbose = F, 
    tuneGrid = train_params_GBM$model_params)


write_rds(model_train_GBM, "input/trained_models/model_train_GBM.rds")


# Model: RF ------------------------
train_params_RF <- list(
    sampling_control = NULL,
    model_method = "ranger",
    model_params = expand.grid(
        .mtry = 1:9, 
        .splitrule = c("gini", "extratrees", "hellinger"), 
        .min.node.size = c(1, 10, 20)
        )
    )


train_control_RF <-  trainControl(method = "cv",
                                   number = 5,
                                   classProbs = TRUE,
                                   verboseIter = TRUE,
                                   summaryFunction = SummaryFuns)

set.seed(2^31 - 1)
model_train_RF <- train(
    rrt_outcome ~ .,
    data = df_covid_train_process,
    method = train_params_RF$model_method,
    metric = "Brier", maximize = FALSE,
    trControl = train_control_RF,
    verbose = F, 
    tuneGrid = train_params_RF$model_params)


write_rds(model_train_RF, "input/trained_models/model_train_RF.rds")






# Model: NB ------------------------
train_params_NB <- list(sampling_control = NULL,
                         model_method = "nb",
                         model_params = NULL
                        )

train_control_NB <-  trainControl(method = "cv",
                                   number = 5,
                                   classProbs = TRUE,
                                   verboseIter = FALSE,
                                   summaryFunction = SummaryFuns)


set.seed(2^31 - 1)
model_train_NB <- train(rrt_outcome ~ .,
                         data = df_covid_train_process,
                         method = train_params_NB$model_method,
                         metric = "Brier", maximize = FALSE,
                         trControl = train_control_NB,
                         tuneGrid = train_params_NB$model_params)


write_rds(model_train_NB, "input/trained_models/model_train_NB.rds")




# Model: SVM ------------------------
train_params_SVM <- list(sampling_control = NULL,
                        model_method = "svmRadial",
                        model_params = expand.grid(
                            sigma = c(0.05, 0.1, 0.2),
                            C = c(1, 2, 5)
                            )
                        )

train_control_SVM <-  trainControl(method = "cv",
                                  number = 5,
                                  classProbs = TRUE,
                                  verboseIter = FALSE,
                                  summaryFunction = SummaryFuns)


set.seed(2^31 - 1)
model_train_SVM <- train(rrt_outcome ~ .,
                        data = df_covid_train_process,
                        method = train_params_SVM$model_method,
                        metric = "Brier", maximize = FALSE,
                        trControl = train_control_SVM,
                        tuneGrid = train_params_SVM$model_params)



write_rds(model_train_SVM, "input/trained_models/model_train_SVM.rds")


# Model: NN ------------------------
train_params_NN <- list(sampling_control = NULL,
                        model_method = "nnet",
                        model_params = expand.grid(
                            size  = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
                            decay = c(0.001, 0.01, 0.1)
                        ))

train_control_NN <-  trainControl(method = "cv",
                                  number = 5,
                                  classProbs = TRUE,
                                  verboseIter = FALSE,
                                  summaryFunction = SummaryFuns)


set.seed(2^31 - 1)
model_train_NN <- train(rrt_outcome ~ .,
                        data = df_covid_train_process,
                        method = train_params_NN$model_method,
                        metric = "Brier", maximize = FALSE,
                        trControl = train_control_NN,
                        tuneGrid = train_params_NN$model_params)


write_rds(model_train_NN, "input/trained_models/model_train_NN.rds")




