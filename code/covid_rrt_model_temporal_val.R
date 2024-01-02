
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


# External Validation -----------------------------------------------------

## Input data
df_covid_external <- read_csv("input/df_icu_adm_COVID_RespSupport_first24h_selected_2022.csv")

## Selecting predictors
df_covid_model_external <- 
    df_covid_external %>% 
    mutate(
        rrt_outcome = if_else(ResourceIsRenalReplacementTherapy == 1, "positive", "negative")
    ) %>%
    select(rrt_outcome, 
           Age, 
           Gender, 
           MFI_level, 
           # SofaScore, 
           Urea, 
           LowestGlasgowComaScale1h, 
           LowestPlateletsCount1h, 
           # HighestBilirubin1h,
           # BUN, 
           HighestCreatinine1h,
           diabetes, 
           chronic_kidney, 
           hypertension, 
           IsNonInvasiveVentilation1h, 
           IsMechanicalVentilation1h, 
           IsVasopressors
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
    ) %>% 
    mutate(
        MFI_level = factor(MFI_level, levels = c("non_frail", "pre_frail","frail"))
    )


## Data imputation

library(mice)
set.seed(2^31-1)
df_covid_model_imp_external <- mice(df_covid_model_external, m = 30) %>% 
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

write_csv(df_covid_model_imp_external, "output/df_covid_model_imp_external.csv")



## Input train parameteres for normalisation
data_pre_process_train <- read_rds("input/data_pre_process_train.rds")

## Input select predictors
selected_vars <- read_csv("output/selected_vars_rfe.csv") %>% pull(variables)

## Normalised dataset (external validation)
df_covid_ext_process <- predict(data_pre_process_train, df_covid_model_imp_external) %>% 
    select(rrt_outcome, selected_vars)

write_rds(df_covid_ext_process, "input/df_covid_ext_process.rds")



### Input Model

final_model <- read_rds("input/trained_models/model_train_RF.rds")















# External Validation metrics ---------------------------------------------
## External validation metrics
df_model_obs_pred_ext <-
    df_covid_ext_process %>% 
    select(rrt_outcome) %>% 
    mutate(
        rrt_outcome = if_else(rrt_outcome == "positive", 1, 0),
        ext_pred = predict(final_model, 
                          type = "prob", 
                          newdata = df_covid_ext_process[, -1])[, "positive"]
        ) 


df_model_obs_pred_ext_metrics <- 
    df_model_obs_pred_ext %>% 
    pivot_longer(-rrt_outcome, names_to = "model", values_to = "pred_prob") %>% 
    group_by(model) %>% 
    summarise(
        auc = as.numeric(pROC::auc(pROC::roc(rrt_outcome, pred_prob))),
        brier = ModelMetrics::brier(actual = rrt_outcome, predicted = pred_prob),
        au_pr = PRROC::pr.curve(scores.class0 = pred_prob, weights.class0 = rrt_outcome)$auc.integral
    )


set.seed(2^31-1)
df_model_obs_pred_ext_metrics_boot <- 
    df_model_obs_pred_ext %>% 
    infer::rep_sample_n(., size = nrow(.), replace = TRUE, reps = 200) %>% 
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
        au_pr_ci = paste_boot_ci(brier),
    ) %>% 
    left_join(
        df_model_obs_pred_ext_metrics
    ) %>% 
    mutate(
        auc_ci = paste(round(auc, 2), auc_ci),
        brier_ci = paste(round(brier, 2), brier_ci),
        au_pr_ci = paste(round(au_pr, 2), brier_ci)
    ) %>% 
    select(model, auc_ci, brier_ci, au_pr_ci)

writexl::write_xlsx(df_model_obs_pred_ext_metrics_boot, "output/df_model_obs_pred_ext_metrics_boot.xlsx")








### AUROC Plots

plot_rocs_label <- 
    c(
        model = paste0("Random Forest (temporal validation)\n", df_model_obs_pred_ext_metrics_boot[which(df_model_obs_pred_ext_metrics_boot$model == "ext_pred"), "auc_ci"])
    )

plot_rocs <- pROC::ggroc(
    list(
        model  = pROC::roc(df_model_obs_pred_ext$rrt_outcome, df_model_obs_pred_ext$ext_pred)
    ), legacy.axes = "TRUE") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_color_manual(name = "", 
                         labels = plot_rocs_label, values = "#00BFC4") +
    geom_abline(aes(slope = 1,intercept = 0), linetype = "dashed") +
    labs(x = "1 - Specificity", y = "Sensitivity") +
    theme_bw() +
    theme(legend.position = "bottom")


ggsave("output/plot_rocs_auc_model_temporal.png", plot_rocs, units = "in", dpi = 800,
       width = 5, height = 4.5)






### Calibration Belt
library(givitiR)

plot_calibelt_ext_val <-
    givitiCalibrationBelt(
        o = df_model_obs_pred_ext$rrt_outcome, # outcome variable (binary 1 or 0)
        e = df_model_obs_pred_ext$ext_pred, # predicted probabilities (numeric, range from 0-1)
        devel = "external"
    )

png("output/Models_Calibelts_TemporalVal.png", 
    width = 5, height = 4.5, units = 'in', res = 800, pointsize = 11)

plot(plot_calibelt_ext_val,
     main = paste0("Random Forest (temporal validation)\n", df_model_obs_pred_ext_metrics_boot[which(df_model_obs_pred_ext_metrics_boot$model == "ext_pred"), "brier_ci"]),
     xlab = "Expected",
     ylab = "Observed", 
     # pvalueString = FALSE, 
     nString = FALSE, 
     polynomialString = FALSE)

dev.off()



### Analysis of Risk groups
df_covid_external_risks <- 
    df_covid_external %>%
    mutate(
        ResourceIsRenalReplacementTherapy = if_else(ResourceIsRenalReplacementTherapy == 1, "RRT", "Non-RRT"),
        outcome_icu = if_else(outcome_icu == "Death", 1, 0),
        outcome_hosp = if_else(outcome_hosp == "Death", 1, 0)
    ) %>% 
    select(
        Age,
        Gender,
        CharlsonComorbidityIndex, 
        MFIpoints,
        MFI_level,
        AdmissionSourceName_group,
        Saps3Points,
        SofaScore,
        hypertension,
        diabetes,
        obesity,
        imunossupression,
        cardio_disease,
        copd_asthma,
        malignancy,
        cerebro_disease,
        chronic_kidney,
        tobacco,
        liver_cirrhosis,
        other_comorb,
        LowestGlasgowComaScale1h,
        LowestPlateletsCount1h,
        Urea, 
        BUN,
        HighestCreatinine1h, 
        # IsRenalReplacementTherapy1h,
        IsNonInvasiveVentilation1h,
        IsMechanicalVentilation1h,
        IsVasopressors1h,
        ResourceIsRenalReplacementTherapy,
        ResourceIsNonInvasiveVentilation,
        ResourceIsHFNC,
        had_mv,
        total_MV_days,
        IsEcmo,
        ResourceIsVasopressors,
        outcome_icu,
        outcome_hosp,
        UnitLengthStay,
        hosp_los_adj
    ) %>% 
    mutate(
        rrt_risk = predict(final_model, 
                           type = "prob", 
                           newdata = df_covid_ext_process[, -1])[, "positive"]
    ) %>% 
    mutate(
        rrt_risk_group = cut_number(rrt_risk, n = 4)
    )



## Descriptive Table -- All MV patients - Comparing the three sets
labels_desc_ext_risk <- list(
    Age ~ "Age, median (IQR)",
    Gender ~ "Gender, N (%)",
    CharlsonComorbidityIndex ~ "Charlson Comorbidity Index, Median (IQR)", 
    MFIpoints ~ "Modified Frailty Index (MFI), median (IQR)",
    MFI_level ~ "",
    AdmissionSourceName_group ~ "Admission Source, N(%)",
    Saps3Points ~ "SAPS-3, median (IQR)",
    SofaScore ~ "SOFA score, median (IQR)",
    hypertension ~ "Hypertension, N (%)",
    diabetes ~ "Diabetes, N (%)",
    obesity ~ "Obesity, N (%)",
    imunossupression ~ "Immunosupression, N (%)",
    cardio_disease ~ "Cardiovascular disease, N (%)",
    copd_asthma ~ "Chronic obstructive pulmonary disease (COPD) or Asthma, N (%)",
    malignancy ~ "Malignancy, N (%)",
    cerebro_disease ~ "Cerebrovascular disease, N (%)",
    chronic_kidney ~ "Chronic Kidney disease, N(%)",
    tobacco ~ "Tobacco history, N (%)",
    liver_cirrhosis ~ "Liver cirrhosis, N (%)",
    other_comorb ~ "Other commorbidities, N (%)",
    LowestGlasgowComaScale1h ~ "Glasgow Coma Scale, median (IQR)",
    Urea ~ "Urea, median (IQR)", 
    BUN ~ "BUN, median (IQR)",
    LowestPlateletsCount1h ~ "Platelets count, median (IQR)",
    HighestCreatinine1h ~ "Creatinine, median (IQR)", 
    IsNonInvasiveVentilation1h ~ "Noninvasive Ventilation at admission, N (%)",
    IsMechanicalVentilation1h ~ "Mechanical ventilation at admission, N (%)",
    IsVasopressors1h ~ "Vasopressors at admission, N (%)",
    # IsRenalReplacementTherapy1h ~ "Renal replacement therapy at admission, N (%)",
    ResourceIsRenalReplacementTherapy ~ "Renal Replacement therapy, N (%)",
    rrt_risk ~ "Predicted RRT risk, median (IQR)",
    ResourceIsNonInvasiveVentilation ~ "Noninvasive ventilation support, N (%)",
    ResourceIsHFNC ~ "High-flow nasal cannula, N (%)",
    had_mv ~ "Mechanical ventilation, N (%)",
    total_MV_days ~ "Days on mechanical ventilation, median (IQR)",
    IsEcmo ~ "ECMO, N (%)",
    ResourceIsVasopressors ~ "Vasopressors, N (%)",
    outcome_icu ~ "ICU mortality, N (%)",
    outcome_hosp ~ "In-hospital mortality, N (%)" ,
    UnitLengthStay ~ "ICU LOS, median (IQR)",
    hosp_los_adj ~ "Hospital LOS, median (IQR)"
)



## RRT Risks descriptive
library(gtsummary)

tb_descriptive_risks <- 
    df_covid_external_risks %>%
    # mutate(
    #     outcome_icu = if_else(outcome_icu == "Death", 1, 0),
    #     outcome_hosp = if_else(outcome_hosp == "Death", 1, 0)
    # ) %>% 
    select(
        Age,
        Gender,
        CharlsonComorbidityIndex, 
        MFIpoints,
        MFI_level,
        AdmissionSourceName_group,
        Saps3Points,
        SofaScore,
        hypertension,
        diabetes,
        obesity,
        imunossupression,
        cardio_disease,
        copd_asthma,
        malignancy,
        cerebro_disease,
        chronic_kidney,
        tobacco,
        liver_cirrhosis,
        other_comorb,
        LowestGlasgowComaScale1h,
        LowestPlateletsCount1h,
        Urea, 
        BUN,
        HighestCreatinine1h, 
        # IsRenalReplacementTherapy1h,
        IsNonInvasiveVentilation1h,
        IsMechanicalVentilation1h,
        IsVasopressors1h,
        ResourceIsRenalReplacementTherapy,
        rrt_risk,
        ResourceIsNonInvasiveVentilation,
        ResourceIsHFNC,
        had_mv,
        total_MV_days,
        IsEcmo,
        ResourceIsVasopressors,
        outcome_icu,
        outcome_hosp,
        UnitLengthStay,
        hosp_los_adj,
        rrt_risk_group
    ) %>% 
    tbl_summary(
        by = "rrt_risk_group",
        type = list(MFIpoints ~ "continuous"),
        missing = "no",
        label = labels_desc_ext_risk
        # by = "ResourceIsMechanicalVentilation"
    ) %>% 
    add_n() %>%
    add_overall() %>%
    add_p()


flextable::save_as_docx(as_flex_table(tb_descriptive_risks), 
                        path = "output/tb_descriptive_COVID_RRT_External_Risk_group.docx")
