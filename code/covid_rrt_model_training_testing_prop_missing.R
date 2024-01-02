
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

# write_csv(df_covid_train_imp, "output/df_covid_train_imp.csv")


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




# Comparison before/after imputation --------------------------------------
library(gtsummary)
library(flextable)

## Training
df_imp_set_train <- 
    bind_rows(
        df_covid_train %>%
            select(Urea, LowestGlasgowComaScale1h, LowestPlateletsCount1h,
                   HighestCreatinine1h) %>% 
            mutate(imp = "before"),
        df_covid_train_imp %>%
            select(Urea, LowestGlasgowComaScale1h, LowestPlateletsCount1h,
                   HighestCreatinine1h)%>% 
            mutate(imp = "after")
        )

# df_imp_set_train %>% 
#     tbl_summary(by = imp) %>% 
#     flextable::as_flex_table(.) %>% 
#     flextable::save_as_docx(., path = "output/df_imp_set_train.docx")

save_as_docx(as_flex_table(df_imp_set_train %>% 
                               tbl_summary(by = imp)), path = "output/df_imp_set_train.docx")

## Test
df_imp_set_test <- 
    bind_rows(
        df_covid_test %>%
            select(Urea, LowestGlasgowComaScale1h, LowestPlateletsCount1h,
                   HighestCreatinine1h) %>% 
            mutate(imp = "before"),
        df_covid_test_imp %>%
            select(Urea, LowestGlasgowComaScale1h, LowestPlateletsCount1h,
                   HighestCreatinine1h)%>% 
            mutate(imp = "after")
    )

save_as_docx(as_flex_table(df_imp_set_test %>% 
                               tbl_summary(by = imp)), path = "output/df_imp_set_test.docx")
