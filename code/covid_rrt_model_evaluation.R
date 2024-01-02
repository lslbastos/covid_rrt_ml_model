
# Library -----------------------------------------------------------------
library(tidyverse)
library(caret)

# User functions ----------------------------------------------------------
## Custom fun
paste_boot_ci <- function(x) {
    # med <- round(median(x), 2)
    q1  <- round(quantile(x, probs = 0.025), 2)
    q3  <- round(quantile(x, probs = 0.975), 2)
    
    return(paste0("(", q1, ", ", q3, ")"))
}


# Data Input --------------------------------------------------------------

## Input models

model_train_LR     <- read_rds("input/trained_models/model_train_LR.rds")
model_train_LR_reg <- read_rds("input/trained_models/model_train_LR_reg.rds")
model_train_NB  <- read_rds("input/trained_models/model_train_NB.rds")
model_train_GBM <- read_rds("input/trained_models/model_train_GBM.rds")
model_train_XGB <- read_rds("input/trained_models/model_train_XGB.rds")
model_train_RF  <- read_rds("input/trained_models/model_train_RF.rds")
model_train_SVM <- read_rds("input/trained_models/model_train_SVM.rds")
model_train_NN  <- read_rds("input/trained_models/model_train_NN.rds")

## Input select predictors
selected_vars <- read_csv("output/selected_vars_rfe.csv") %>% pull(variables)


## Training set input
df_covid_train_process <- read_rds("input/df_covid_train_process.rds") %>% 
    select(rrt_outcome, selected_vars)

## Test set input
df_covid_test_process <- read_rds("input/df_covid_test_process.rds") %>% 
    select(rrt_outcome, selected_vars)





# Obtaining CV metrics ----------------------------------------------------
cv_resamples_metrics <- 
    resamples(
        list(
            LR = model_train_LR,
            LR_reg = model_train_LR_reg,
            RF = model_train_RF,
            XGB = model_train_XGB,
            GBM = model_train_GBM,
            SVM = model_train_SVM,
            NB = model_train_NB,
            NN = model_train_NN
        )
    ) %>% 
    summary()


df_cv_resamples_metrics <- 
    cv_resamples_metrics$statistics %>% 
    imap_dfr(
        function(tb = .x, metric = .y) {
            
            df <- bind_cols(
                model = rownames(tb),
                as_tibble(tb)
            ) %>% 
                mutate(
                    metric = metric
                ) %>% 
                select(
                    model,
                    metric,
                    Median,
                    Q1 = `1st Qu.`,
                    Q3 = `3rd Qu.`
                )
        }
    ) %>% 
    mutate(
        median_iqr = paste0(
            round(Median, 3), " (", 
            round(Q1, 3), " ,", 
            round(Q3, 3), ")" 
        )
    ) %>% 
    select(model, metric, median_iqr) %>% 
    pivot_wider(names_from = "metric", values_from = "median_iqr")



writexl::write_xlsx(df_cv_resamples_metrics, "output/df_cv_resamples_metrics.xlsx")


# Metrics on testing samples ----------------------------------------------
library(rms)
## Obtaining predictions
df_model_obs_pred_test <-
    df_covid_test_process %>% 
    select(rrt_outcome) %>% 
    mutate(
        rrt_outcome = if_else(rrt_outcome == "positive", 1, 0),
        LR_pred = predict(model_train_LR, 
                          type = "prob", 
                          newdata = df_covid_test_process[, -1])[, "positive"],
        LR_reg_pred = predict(model_train_LR_reg, 
                              type = "prob", 
                              newdata = df_covid_test_process[, -1])[, "positive"],
        RF_pred = predict(model_train_RF,
                          type = "prob",
                          newdata = df_covid_test_process[, -1])[, "positive"],
        GBM_pred = predict(model_train_GBM, 
                           type = "prob", 
                           newdata = df_covid_test_process[, -1])[, "positive"],
        XGB_pred = predict(model_train_XGB, 
                           type = "prob", 
                           newdata = df_covid_test_process[, -1])[, "positive"],
        SVM_pred = predict(model_train_SVM, 
                           type = "prob", 
                           newdata = df_covid_test_process[, -1])[, "positive"],
        NB_pred = predict(model_train_NB, 
                          type = "prob", 
                          newdata = df_covid_test_process[, -1])[, "positive"],
        NN_pred = predict(model_train_NN, 
                          type = "prob", 
                          newdata = df_covid_test_process[, -1])[, "positive"]
    ) 


df_model_obs_pred_test_metrics <- 
    df_model_obs_pred_test %>% 
    pivot_longer(-rrt_outcome, names_to = "model", values_to = "pred_prob") %>% 
    group_by(model) %>% 
    summarise(
        auc = as.numeric(pROC::auc(pROC::roc(rrt_outcome, pred_prob))),
        brier = ModelMetrics::brier(actual = rrt_outcome, predicted = pred_prob),
        au_pr = PRROC::pr.curve(scores.class0 = pred_prob, weights.class0 = rrt_outcome)$auc.integral,
    )


set.seed(2^31-1)
df_model_obs_pred_test_metrics_boot <- 
    df_model_obs_pred_test %>% 
    infer::rep_sample_n(., size = nrow(.), replace = TRUE, reps = 2000) %>% 
    pivot_longer(-c(rrt_outcome, replicate), names_to = "model", values_to = "pred_prob") %>% 
    group_by(model, replicate) %>% 
    summarise(
        auc   = as.numeric(pROC::auc(pROC::roc(rrt_outcome, pred_prob, quiet = TRUE))),
        brier = ModelMetrics::brier(actual = rrt_outcome, predicted = pred_prob),
        au_pr = PRROC::pr.curve(scores.class0 = pred_prob, weights.class0 = rrt_outcome)$auc.integral
    ) %>% 
    group_by(model) %>% 
    summarise(
        auc_ci   = paste_boot_ci(auc),
        brier_ci = paste_boot_ci(brier),
        au_pr_ci = paste_boot_ci(au_pr),
    ) %>% 
    left_join(
        df_model_obs_pred_test_metrics
    ) %>% 
    mutate(
        auc_ci = paste(round(auc, 2), auc_ci),
        brier_ci = paste(round(brier, 2), brier_ci),
        au_pr_ci = paste(round(au_pr, 2), au_pr_ci)
    ) %>% 
    select(model, auc_ci, brier_ci, au_pr_ci)

writexl::write_xlsx(df_model_obs_pred_test_metrics_boot, "output/df_model_obs_pred_test_metrics_boot.xlsx")


df_model_obs_pred_test_cutoff <-
    df_covid_test_process %>% 
    select(rrt_outcome) %>% 
    mutate(
        # rrt_outcome = if_else(rrt_outcome == "positive", 1, 0),
        LR_pred = predict(model_train_LR, 
                          # type = "response", 
                          newdata = df_covid_test_process[, -1]),
        LR_reg_pred = predict(model_train_LR_reg, 
                              # type = "prob", 
                              newdata = df_covid_test_process[, -1]),
        RF_pred = predict(model_train_RF,
                          # type = "prob",
                          newdata = df_covid_test_process[, -1]),
        GBM_pred = predict(model_train_GBM, 
                           # type = "prob", 
                           newdata = df_covid_test_process[, -1]),
        XGB_pred = predict(model_train_XGB, 
                           # type = "prob", 
                           newdata = df_covid_test_process[, -1]),
        SVM_pred = predict(model_train_SVM, 
                           # type = "prob", 
                           newdata = df_covid_test_process[, -1]),
        NB_pred = predict(model_train_NB, 
                          # type = "prob", 
                          newdata = df_covid_test_process[, -1]),
        NN_pred = predict(model_train_NN, 
                          # type = "prob", 
                          newdata = df_covid_test_process[, -1])
    ) 
    # %>% 
    # mutate_at(vars(ends_with("_pred")), function(x){case_when(x == "positive" ~ 1, TRUE ~ 0)})



# caret::confusionMatrix()    

df_model_obs_pred_test_metrics_boot_cutoff <- 
    df_model_obs_pred_test_cutoff %>% 
    infer::rep_sample_n(., size = nrow(.), replace = TRUE, reps = 2000) %>% 
    pivot_longer(-c(rrt_outcome, replicate), names_to = "model", values_to = "pred_prob") %>% 
    group_by(model, replicate) %>% 
    summarise(
        sensitivity = caret::confusionMatrix(table(pred_prob, rrt_outcome), positive = "positive")$byClass["Sensitivity"],
        specificity = caret::confusionMatrix(table(pred_prob, rrt_outcome), positive = "positive")$byClass["Specificity"],
        PPV = caret::confusionMatrix(table(pred_prob, rrt_outcome), positive = "positive")$byClass["Pos Pred Value"],
        NPV = caret::confusionMatrix(table(pred_prob, rrt_outcome), positive = "positive")$byClass["Neg Pred Value"],
        F1 = caret::confusionMatrix(table(pred_prob, rrt_outcome), positive = "positive")$byClass["F1"]
    ) %>% 
    group_by(model) %>% 
    summarise(
        sensitivity_ci   = paste_boot_ci(sensitivity),
        specificity_ci = paste_boot_ci(specificity),
        PPV_ci = paste_boot_ci(PPV),
        NPV_ci = paste_boot_ci(NPV),
        F1_ci = paste_boot_ci(NPV)
    ) %>% 
    left_join(
        df_model_obs_pred_test_cutoff %>% 
            pivot_longer(-c(rrt_outcome), names_to = "model", values_to = "pred_prob") %>% 
            group_by(model) %>% 
            summarise(
                sensitivity = caret::confusionMatrix(table(pred_prob, rrt_outcome), positive = "positive")$byClass["Sensitivity"],
                specificity = caret::confusionMatrix(table(pred_prob, rrt_outcome), positive = "positive")$byClass["Specificity"],
                PPV = caret::confusionMatrix(table(pred_prob, rrt_outcome), positive = "positive")$byClass["Pos Pred Value"],
                NPV = caret::confusionMatrix(table(pred_prob, rrt_outcome), positive = "positive")$byClass["Neg Pred Value"],
                F1 = caret::confusionMatrix(table(pred_prob, rrt_outcome), positive = "positive")$byClass["F1"]
            )
        ) %>% 
    mutate(
        sensitivity_ci = paste(round(sensitivity, 2), sensitivity_ci),
        specificity_ci = paste(round(specificity, 2), specificity_ci),
        PPV_ci = paste(round(PPV, 2), PPV_ci),
        NPV_ci = paste(round(NPV, 2), NPV_ci),
        F1_ci = paste(round(F1, 2), F1_ci)
    ) %>%
    select(model, sensitivity_ci, specificity_ci, PPV_ci, NPV_ci, F1_ci)


writexl::write_xlsx(df_model_obs_pred_test_metrics_boot_cutoff, 
                    "output/df_model_obs_pred_test_metrics_boot_cutoff.xlsx")



# AUROC  ------------------------------------------------------------------

plot_rocs_label <- 
    c(LR     = paste0("Logistic Regression\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "LR_pred"), "auc_ci"]),
      LR_reg = paste0("Reg. Logistic Regression\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "LR_reg_pred"), "auc_ci"]),
      NB = paste0("Naive Bayes\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "NB_pred"), "auc_ci"]),
      RF = paste0("Random Forest\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "RF_pred"), "auc_ci"]),
      XGB = paste0("XGBoosting\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "XGB_pred"), "auc_ci"]),
      GBM = paste0("SGBoosting\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "GBM_pred"), "auc_ci"]),
      SVM = paste0("SVM\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "SVM_pred"), "auc_ci"]),
      NN = paste0("Neural Network\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "NN_pred"), "auc_ci"])
      )
      
plot_rocs <- pROC::ggroc(
    list(
        LR  = pROC::roc(df_model_obs_pred_test$rrt_outcome, df_model_obs_pred_test$LR_pred),
        LR_reg  = pROC::roc(df_model_obs_pred_test$rrt_outcome, df_model_obs_pred_test$LR_reg_pred),
        NB = pROC::roc(df_model_obs_pred_test$rrt_outcome, df_model_obs_pred_test$NB_pred),
        RF  = pROC::roc(df_model_obs_pred_test$rrt_outcome, df_model_obs_pred_test$RF_pred),
        XGB = pROC::roc(df_model_obs_pred_test$rrt_outcome, df_model_obs_pred_test$XGB_pred),
        GBM = pROC::roc(df_model_obs_pred_test$rrt_outcome, df_model_obs_pred_test$GBM_pred),
        SVM = pROC::roc(df_model_obs_pred_test$rrt_outcome, df_model_obs_pred_test$SVM_pred),
        NN  = pROC::roc(df_model_obs_pred_test$rrt_outcome, df_model_obs_pred_test$NN_pred)
    ), legacy.axes = "TRUE") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    geom_abline(aes(slope = 1,intercept = 0), linetype = "dashed") +
    scale_color_discrete(name = "",
                         labels = plot_rocs_label) +
    labs(x = "1 - Specificity", y = "Sensitivity") +
    theme_bw() +
    theme(legend.position = "bottom")

# 
# ggsave("output/plot_rocs_auc_models.png", plot_rocs, units = "in", dpi = 800,
#        width = 6, height = 5)
# 







#### AUPR Plots
# library(PRROC)
# 
# pr_RL <- pr.curve(df_model_obs_pred_test$LR_pred[df_model_obs_pred_test$rrt_outcome == 1],
#                   df_model_obs_pred_test$LR_pred[df_model_obs_pred_test$rrt_outcome == 0],
#                   curve = TRUE)
# 
# pr_RL_reg <- pr.curve(df_model_obs_pred_test$LR_reg_pred[df_model_obs_pred_test$rrt_outcome == 1],
#                       df_model_obs_pred_test$LR_reg_pred[df_model_obs_pred_test$rrt_outcome == 0],
#                       curve = TRUE)
# 
# pr_NB <- pr.curve(df_model_obs_pred_test$NB_pred[df_model_obs_pred_test$rrt_outcome == 1],
#                   df_model_obs_pred_test$NB_pred[df_model_obs_pred_test$rrt_outcome == 0],
#                   curve = TRUE)
# 
# pr_RF <- pr.curve(df_model_obs_pred_test$RF_pred[df_model_obs_pred_test$rrt_outcome == 1],
#                   df_model_obs_pred_test$RF_pred[df_model_obs_pred_test$rrt_outcome == 0],
#                   curve = TRUE)
# 
# pr_XGB <- pr.curve(df_model_obs_pred_test$XGB_pred[df_model_obs_pred_test$rrt_outcome == 1],
#                    df_model_obs_pred_test$XGB_pred[df_model_obs_pred_test$rrt_outcome == 0],
#                    curve = TRUE)
# 
# pr_GBM <- pr.curve(df_model_obs_pred_test$GBM_pred[df_model_obs_pred_test$rrt_outcome == 1],
#                    df_model_obs_pred_test$GBM_pred[df_model_obs_pred_test$rrt_outcome == 0],
#                    curve = TRUE)
# 
# pr_SVM <- pr.curve(df_model_obs_pred_test$SVM_pred[df_model_obs_pred_test$rrt_outcome == 1],
#                    df_model_obs_pred_test$SVM_pred[df_model_obs_pred_test$rrt_outcome == 1],
#                    curve = TRUE)
# 
# pr_NN <- pr.curve(df_model_obs_pred_test$NN_pred[df_model_obs_pred_test$rrt_outcome == 1],
#                   df_model_obs_pred_test$NN_pred[df_model_obs_pred_test$rrt_outcome == 0],
#                   curve = TRUE)
# 
# df_pr_curves <- bind_rows(
#     pr_RL$curve %>%
#         as_tibble() %>%
#         select(V1, V2) %>%
#         rename(recall = V1,
#                precision = V2
#         ) %>%
#         mutate(model = "RL"),
#     pr_RL_reg$curve %>%
#         as_tibble() %>%
#         select(V1, V2) %>%
#         rename(recall = V1,
#                precision = V2
#         ) %>%
#         mutate(model = "RL_reg"),
#     pr_NB$curve %>%
#         as_tibble() %>%
#         select(V1, V2) %>%
#         rename(recall = V1,
#                precision = V2
#         ) %>%
#         mutate(model = "NB"),
#     pr_RF$curve %>%
#         as_tibble() %>%
#         select(V1, V2) %>%
#         rename(recall = V1,
#                precision = V2
#         ) %>%
#         mutate(model = "RF"),
#     pr_GBM$curve %>%
#         as_tibble() %>%
#         select(V1, V2) %>%
#         rename(recall = V1,
#                precision = V2
#         ) %>%
#         mutate(model = "GBM"),
#     pr_XGB$curve %>%
#         as_tibble() %>%
#         select(V1, V2) %>%
#         rename(recall = V1,
#                precision = V2
#         ) %>%
#         mutate(model = "XGB"),
#     pr_SVM$curve %>%
#         as_tibble() %>%
#         select(V1, V2) %>%
#         rename(recall = V1,
#                precision = V2
#         ) %>%
#         mutate(model = "SVM"),
#     pr_NN$curve %>%
#         as_tibble() %>%
#         select(V1, V2) %>%
#         rename(recall = V1,
#                precision = V2
#         ) %>%
#         mutate(model = "NN")
# ) %>%
#     mutate(model = factor(model, levels = c("RL", "RL_reg", "NB", "RF", "XGB", "SVM", "NN")))
# 
# 
# plot_pr_curve <- df_pr_curves %>%
#     ggplot() +
#     geom_path(aes(x = recall, y = precision, color = model)) +
#     guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
#     scale_y_continuous(limits = c(0, 1))+
#     # scale_color_discrete(name = "", labels = c("Age + WFNS (4 or 5)\n[AUPR: 0.72]",
#     #                                            "nSAPS-3\n[AUPR: 0.73]",
#     #                                            "nSOFA\n[AUPR: 0.70]",
#     #                                            "Age +  WFNS (4 or 5) +\nnSAPS-3 + nSOFA + \nVasopressor\n[AUPR:0.78]")) +
#     labs(x = "Recall",
#          y = "Precision"
#     ) +
#     theme_bw() +
#     theme(legend.position = "bottom")
# 
# 
# ggsave("../output/figures/plot_pr.png", plot_pr_curve, units = "in", dpi = 800,
#        width = 5, height = 4.5)
# 
# 
# df_au_pr <- tibble(model = c("RL", "RL_reg", "NB", "RF", "GBM", "XGB", "SVM", "NN"),
#                    au_pr = c(pr_RL$auc.integral,
#                              pr_RL_reg$auc.integral,
#                              pr_NB$auc.integral,
#                              pr_RF$auc.integral,
#                              pr_GBM$auc.integral,
#                              pr_XGB$auc.integral,
#                              pr_SVM$auc.integral,
#                              pr_NN$auc.integral
#                              )
# )
# 
# write_csv(df_au_pr, "../output/tables/df_au_pr.csv")












# Calibration Belt (Test data) -----------------------------------------------
library(givitiR)

## The functions may take a while to run, depending on the sample size.

## Calibration Belts - Original
plot_calibelt_LR <-
    givitiCalibrationBelt(
        o = df_model_obs_pred_test$rrt_outcome, # outcome variable (binary 1 or 0)
        e = df_model_obs_pred_test$LR_pred, # predicted probabilities (numeric, range from 0-1)
        devel = "external"
    )

plot_calibelt_LR_reg <-
    givitiCalibrationBelt(
        o = df_model_obs_pred_test$rrt_outcome, # outcome variable (binary 1 or 0)
        e = df_model_obs_pred_test$LR_reg_pred, # predicted probabilities (numeric, range from 0-1)
        devel = "external"
    )

plot_calibelt_RF <-
    givitiCalibrationBelt(
        o = df_model_obs_pred_test$rrt_outcome, # outcome variable (binary 1 or 0)
        e = df_model_obs_pred_test$RF_pred, # predicted probabilities (numeric, range from 0-1)
        devel = "external"
    )


plot_calibelt_XGB <-
    givitiCalibrationBelt(
        o = df_model_obs_pred_test$rrt_outcome, # outcome variable (binary 1 or 0)
        e = df_model_obs_pred_test$XGB_pred, # predicted probabilities (numeric, range from 0-1)
        devel = "external"
    )

plot_calibelt_GBM <-
    givitiCalibrationBelt(
        o = df_model_obs_pred_test$rrt_outcome, # outcome variable (binary 1 or 0)
        e = df_model_obs_pred_test$GBM_pred, # predicted probabilities (numeric, range from 0-1)
        devel = "external"
    )

plot_calibelt_SVM <-
    givitiCalibrationBelt(
        o = df_model_obs_pred_test$rrt_outcome, # outcome variable (binary 1 or 0)
        e = df_model_obs_pred_test$SVM_pred, # predicted probabilities (numeric, range from 0-1)
        devel = "external"
    )

plot_calibelt_NB <-
    givitiCalibrationBelt(
        o = df_model_obs_pred_test$rrt_outcome, # outcome variable (binary 1 or 0)
        e = df_model_obs_pred_test$NB_pred, # predicted probabilities (numeric, range from 0-1)
        devel = "external"
    )

plot_calibelt_NN <-
    givitiCalibrationBelt(
        o = df_model_obs_pred_test$rrt_outcome, # outcome variable (binary 1 or 0)
        e = df_model_obs_pred_test$NN_pred, # predicted probabilities (numeric, range from 0-1)
        devel = "external"
    )

## Exporting combined figures
# png("output/Models_Calibelts_Test_Set.png", 
#     width = 12, height = 7, units = 'in', res = 800, pointsize = 11)
pdf("output/Models_Calibelts_Test_Set.pdf", 
    width = 12, height = 7, pointsize = 11)


par(mfrow = c(2,4))
plot(plot_calibelt_LR,
     main = paste0("Logistic Regression\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "LR_pred"), "brier_ci"]),
     xlab = "Expected",
     ylab = "Observed", 
     pvalueString = T, 
     nString = FALSE, 
     polynomialString = FALSE)

plot(plot_calibelt_LR_reg,
     main = paste0("Regularized Logistic Regression\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "LR_reg_pred"), "brier_ci"]),
     xlab = "Expected",
     ylab = "Observed", 
     pvalueString = T, 
     nString = FALSE, 
     polynomialString = FALSE)

plot(plot_calibelt_NB,
     main = paste0("Naive Bayes\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "NB_pred"), "brier_ci"]),
     xlab = "Expected",
     ylab = "Observed",
     pvalueString = T,
     nString = FALSE, 
     polynomialString = FALSE)

plot(plot_calibelt_RF,
     main = paste0("Random Forest\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "RF_pred"), "brier_ci"]),
     xlab = "Expected",
     ylab = "Observed",
     pvalueString = T,
     nString = FALSE, 
     polynomialString = FALSE)

plot(plot_calibelt_GBM,
     main = paste0("SGBoosting\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "GBM_pred"), "brier_ci"]),
     xlab = "Expected",
     ylab = "Observed",
     pvalueString = T,
     nString = FALSE, 
     polynomialString = FALSE)


plot(plot_calibelt_XGB,
     main = paste0("XGBoosting\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "XGB_pred"), "brier_ci"]),
     xlab = "Expected",
     ylab = "Observed",
     pvalueString = T,
     nString = FALSE, 
     polynomialString = FALSE)

plot(plot_calibelt_SVM,
     main = paste0("SVM\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "SVM_pred"), "brier_ci"]),
     xlab = "Expected",
     ylab = "Observed",
     pvalueString = T,
     nString = FALSE, 
     polynomialString = FALSE)

plot(plot_calibelt_NN,
     main = paste0("Neural Network\n", df_model_obs_pred_test_metrics_boot[which(df_model_obs_pred_test_metrics_boot$model == "NN_pred"), "brier_ci"]),
     xlab = "Expected",
     ylab = "Observed",
     pvalueString = T,
     nString = FALSE, 
     polynomialString = FALSE)

dev.off()









# Variable importance and feature value SHAP (best model) -----------------
### The chosen model was: XGB boosting (better calibration)

library(fastshap)

# Prediction wrapper
pfun <- function(object, newdata) {
    
    # Subtracts one to adjust for probabilities in predict()
    # 1- predict(object, newdata = newdata, type = "prob")
    predict(object, data = as.data.frame(newdata))$predictions[, 2]
}


## Creating xgboosting Matrix-format data
dummy_creation <- dummyVars(~ . , 
                            data = df_covid_train_process[, -1], sep = "", fullRank = T, drop2nd = F)

df_covid_train_process_dummy <- predict(dummy_creation, df_covid_train_process[, -1])

# 
# predict(object = model_train_RF$finalModel, 
#         data = as.data.frame(df_covid_train_process_dummy))$predictions[, 2]

pfun(object = model_train_RF$finalModel,
     newdata = df_covid_train_process_dummy)
# pfun(model_train_XGB$finalModel,
#      newdata = xgboost::xgb.DMatrix(df_covid_train_process_dummy))


## Obtaining SHAP values
set.seed(2^31 - 1)
shap <- explain(object = model_train_RF$finalModel,
                X = df_covid_train_process_dummy,
                pred_wrapper = pfun, nsim = 10, adjust = TRUE)

# 
# shap <- explain(object = model_train_XGB$finalModel,
#                 X = df_covid_train_process_dummy,
#                 pred_wrapper = pfun, nsim = 100)
# 


## SHAP Variable importance
shap_imp <- data.frame(
    Variable = names(shap),
    Importance = apply(shap, MARGIN = 2, FUN = function(x) sum(abs(x)))
) %>% 
  arrange(-Importance)

plot_shap_var_importance <- 
    ggplot(shap_imp, aes(reorder(Variable, Importance), Importance)) +
    geom_col() +
    coord_flip() +
    xlab("") +
    ylab("mean(|Shapley value|)")

# ggsave("output/plot_shap_var_importance.png", plot_shap_var_importance, units = "in", dpi = 800,
#        width = 6, height = 4.5)




# 
# shap_dep_x3 <- data.frame(x3 = df_covid_train_process_dummy[, "IsMechanicalVentilationyes"],
#                           shap = shap[["IsMechanicalVentilationyes"]])
# shap_dep_x3 <- data.frame(x3 = df_covid_train_process_dummy[, "HighestCreatinine1h"],
#                           shap = shap[["HighestCreatinine1h"]])
# shap_dep_x3 <- data.frame(x3 = df_covid_train_process_dummy[, "LowestGlasgowComaScale1h"],
#                           shap = shap[["LowestGlasgowComaScale1h"]])
# shap_dep_x3 <- data.frame(x3 = df_covid_train_process_dummy[, "HighestBilirubin1h"],
#                           shap = shap[["HighestBilirubin1h"]])
# 
# ggplot(shap_dep_x3, aes(x3, shap)) +
#     geom_point(alpha = 0.3) +
#     # geom_smooth() +
#     ylab("Shapley value")


## SHAP Variable individual
plot_shap_var_label <- 
    c(
        HighestCreatinine1h     = "Creatinine",
        Urea                    = "Urea",
        LowestPlateletsCount1h  = "Platelets",
        HighestBilirubin1h      = "Bilirubin",
        Age                     = "Age", 
        LowestGlasgowComaScale1h    = "Glasgow Coma Scale", 
        IsVasopressorsyes           = "Vasopressors", 
        IsMechanicalVentilationyes  = "IMV", 
        IsNonInvasiveVentilationyes  = "NIV", 
        hypertensionyes  = "Hypertension", 
        MFI_levelpre_frail = "MFI: Pre-frail", 
        MFI_levelnon_frail = "MFI: Non-frail", 
        MFI_levelfrail     = "MFI: Frail",
        chronic_kidneyyes = "Chronic Kidney Disease",
        GenderM = "Gender: Male",
        diabetesyes = "Diabetes"
    )
           


plot_shap_var_value <- 
    autoplot(shap, type = "contribution") +
    scale_x_discrete(labels = plot_shap_var_label) +
    # scale_fill_discrete() +
    labs(
        y = "Shapley values"
    ) +
    theme_bw()


df_shap_ind <- 
  as_tibble(shap) %>% 
  mutate(shap_id = 1:n()) %>% 
  pivot_longer(-shap_id, names_to = "var", values_to = "shap") %>% 
  ungroup() %>% 
  left_join(
    
    as_tibble(df_covid_train_process_dummy) %>%
      mutate(shap_id = 1:n()) %>% 
      pivot_longer(-shap_id, names_to = "var", values_to = "val")
    
  ) %>% 
  left_join(
    shap_imp,
    by = c("var" = "Variable")
  ) %>% 
  arrange(Importance) %>% 
  mutate(
    var = factor(var, levels = rev(shap_imp$Variable))
  )



plot_shap_var_value_summary <-
  df_shap_ind %>% 
    ggplot() +
    ggbeeswarm::geom_quasirandom(aes(x = shap, y = var, color = val), size = 0.7) +
    # geom_point(aes(x = shap, y = reorder(var, Importance), color = val)) +
    scale_color_gradient(name = "Feature Value", low = "blue", high = "red", 
                         breaks = c(0, 1), labels = c("Low", "High")) + 
    scale_y_discrete(labels = plot_shap_var_label) +
    # scale_fill_discrete() +
    labs(
        x = "Shapley values",
        y = ""
    ) +
    theme_bw() +
    theme(legend.position = "bottom")

# autoplot(shap, type = "dependence", feature = "MFI_levelpre_frail", X = df_covid_train_process_dummy) +
#     # scale_x_discrete(labels = plot_shap_var_label) +
#     # scale_fill_discrete() +
#     # labs(
#     #     y = "Shapley values\n(Best model: XGBoosting)"
#     # ) +
#     theme_bw()


library(patchwork)

plot_comb_auc_shap <- 
    (plot_rocs / plot_shap_var_value) + plot_annotation(tag_levels = "A")
    
ggsave("output/plot_comb_auc_shap.pdf", plot_comb_auc_shap, units = "in", dpi = 800,
       width = 8, height = 10)



plot_comb_auc_shap <- 
    (plot_rocs / plot_shap_var_value_summary) + plot_annotation(tag_levels = "A")
    
ggsave("output/plot_comb_auc_shap_summary.pdf", plot_comb_auc_shap, units = "in", dpi = 400,
       width = 8, height = 10)










