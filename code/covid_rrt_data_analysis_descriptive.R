
# Library -----------------------------------------------------------------
library(tidyverse)


# Data Input --------------------------------------------------------------
df_covid_mv <- 
    read_csv("input/df_icu_adm_COVID_RespSupport_first24h_selected_2020_2021.csv") %>% 
    mutate(
        rrt_outcome = factor(ResourceIsRenalReplacementTherapy, 
                             levels = c(0, 1), labels = c("negative", "positive"))
    )


set.seed(2^31-1)
trainIndex <- caret::createDataPartition(df_covid_mv$rrt_outcome, 
                                  p = .8, 
                                  list = FALSE, 
                                  times = 1)


df_covid_mv_set <- 
    df_covid_mv %>% 
    mutate(set = case_when(
        row_number() %in% trainIndex ~ "train",
        !(row_number() %in% trainIndex) ~ "test"
    )
    
               
               
               )
    
# Data Analysis -----------------------------------------------------------
## Descriptive Table -- All MV patients
library(gtsummary)
labels_desc <- list(
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
    IsNonInvasiveVentilation ~ "Noninvasive Ventilation (24h), N (%)",
    IsMechanicalVentilation ~ "Mechanical ventilation (24h), N (%)",
    IsVasopressors ~ "Vasopressors (24h), N (%)",
    IsRenalReplacementTherapy ~ "Renal replacement therapy (24h), N (%)",
    # ResourceIsRenalReplacementTherapy ~ "Renal Replacement Therapy, N (%)",
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

tb_descriptive <- 
    df_covid_mv_set %>%
    filter(set %in% c("train", "test")) %>% 
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
        IsRenalReplacementTherapy,
        IsNonInvasiveVentilation,
        IsMechanicalVentilation,
        IsVasopressors,
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
    tbl_summary(
        by = "ResourceIsRenalReplacementTherapy",
        type = list(MFIpoints ~ "continuous"),
        missing = "no", label = labels_desc
        # by = "ResourceIsMechanicalVentilation"
    ) %>% 
    add_n() %>%
    add_overall() %>% 
    add_p()
    


# library(flextable)

flextable::save_as_docx(as_flex_table(tb_descriptive), 
             path = "output/tb_descriptive_COVID_RRT.docx")



## Descriptive Table -- All MV patients - Comparing the three sets
labels_desc_sets <- list(
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
    IsNonInvasiveVentilation ~ "Noninvasive Ventilation (24h), N (%)",
    IsMechanicalVentilation ~ "Mechanical ventilation (24h), N (%)",
    IsVasopressors ~ "Vasopressors (24h), N (%)",
    IsRenalReplacementTherapy ~ "Renal replacement therapy (24h), N (%)",
    ResourceIsRenalReplacementTherapy ~ "Renal Replacement therapy, N (%)",
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


tb_descriptive_sets <- 
    df_covid_mv_set %>%
    bind_rows(
        read_csv("input/df_icu_adm_COVID_RespSupport_first24h_selected_2022.csv")
    ) %>% 
    mutate(
        outcome_icu = if_else(outcome_icu == "Death", 1, 0),
        outcome_hosp = if_else(outcome_hosp == "Death", 1, 0)
    ) %>% 

    # filter(set %in% c("train", "test")) %>% 
    # mutate(
    #     ResourceIsRenalReplacementTherapy = if_else(ResourceIsRenalReplacementTherapy == 1, "RRT", "Non-RRT")
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
        IsRenalReplacementTherapy,
        IsNonInvasiveVentilation,
        IsMechanicalVentilation,
        IsVasopressors,
        ResourceIsRenalReplacementTherapy,
        # RenalReplacementTherapyDuration,
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
        set
    ) %>% 
    tbl_summary(
        by = "set",
        type = list(MFIpoints ~ "continuous"),
        missing = "no",
		label = labels_desc_sets
        # by = "ResourceIsMechanicalVentilation"
    ) %>% 
    add_n() %>%
    add_overall() %>%
	add_p()
    

flextable::save_as_docx(as_flex_table(tb_descriptive_sets), 
             path = "output/tb_descriptive_COVID_RRT_Sets.docx")





# Temporal Analysis -------------------------------------------------------

###########################################################################
###########################################################################
###########################################################################
###########################################################################

# Temporal plots ----------------------------------------------------------


df_covid_mv_day <- 
    df_covid_mv %>% 
    group_by(UnitAdmissionDate) %>% 
    summarise(
        admissions = n()
    ) %>% 
    ungroup() %>%
    left_join(
        df_covid_mv %>% 
            filter(outcome_icu == "Death") %>% 
            group_by(UnitDischargeDate) %>% 
            summarise(
                deaths = n()
            ) %>% 
            ungroup(),
        by = c("UnitAdmissionDate" = "UnitDischargeDate")
    ) %>% 
    mutate(
        admissions = replace_na(admissions, 0),
        deaths = replace_na(deaths, 0)
    ) %>% 
    arrange(UnitAdmissionDate) %>% 
    mutate(
        admissions_MM7 = zoo::rollmean(admissions, k = 7, align = "right", fill = NA),
        deaths_MM7     = zoo::rollmean(deaths, k = 7, align = "right", fill = NA)
    )






plot_admissions_day <-
    df_covid_mv_day %>% 
    ggplot() +
    geom_line(aes(x = UnitAdmissionDate, y = admissions_MM7)) +
    scale_x_date(date_labels = "%b/%y") +
    labs(
        x = "",
        y = "New MV admissions"
    ) +
    ylim(0, 100) +
    # scale_x_date() +
    theme_classic()

plot_death_day <-
    df_covid_mv_day %>% 
    ggplot() +
    geom_line(aes(x = UnitAdmissionDate, y = deaths_MM7)) +
    scale_x_date(date_labels = "%b/%y") +
    labs(
        title = "",
        x = "",
        y = "New MV deaths"
    ) +
    # scale_x_date() +
    theme_classic() +
    theme(legend.position = "top")








df_covid_mv_day_rrt <- 
    df_covid_mv %>% 
    group_by(UnitAdmissionDate, ResourceIsRenalReplacementTherapy) %>% 
    summarise(
        admissions = n()
    ) %>% 
    mutate(
        ResourceIsRenalReplacementTherapy = as.character(ResourceIsRenalReplacementTherapy) 
    ) %>% 
    ungroup() %>% 
    left_join(
        df_covid_mv %>% 
            filter(outcome_icu == "Death") %>% 
            group_by(UnitDischargeDate, ResourceIsRenalReplacementTherapy) %>% 
            summarise(
                deaths = n()
            ) %>% 
            mutate(
                ResourceIsRenalReplacementTherapy = as.character(ResourceIsRenalReplacementTherapy) 
            ) %>% 
            ungroup()
        , by = c("UnitAdmissionDate" = "UnitDischargeDate",
                 "ResourceIsRenalReplacementTherapy" = "ResourceIsRenalReplacementTherapy")
    ) %>% 
    mutate(
        admissions = replace_na(admissions, 0),
        deaths = replace_na(deaths, 0)
    ) %>% 
    group_by(ResourceIsRenalReplacementTherapy) %>% 
    arrange(UnitAdmissionDate) %>% 
    mutate(
        admissions_MM7 = zoo::rollmean(admissions, k = 7, align = "right", fill = NA),
        deaths_MM7     = zoo::rollmean(deaths, k = 7, align = "right", fill = NA)
    ) %>% 
    ungroup()


plot_admissions_day_RRT <-
    df_covid_mv_day_rrt %>% 
    ggplot() +
    geom_line(aes(x = UnitAdmissionDate, y = admissions_MM7, color = ResourceIsRenalReplacementTherapy)) +
    scale_color_discrete(name = "Renal Replacement Therapy", labels = c("No", "Yes")) +
    scale_x_date(date_labels = "%b/%y") +
    labs(
        x = "",
        y = "New MV admissions"
    ) +
    # scale_x_date() +
    theme_classic() +
    theme(legend.position = "top")


plot_death_day_RRT <-
    df_covid_mv_day_rrt %>% 
    ggplot() +
    geom_line(aes(x = UnitAdmissionDate, y = deaths_MM7, color = ResourceIsRenalReplacementTherapy)) +
    scale_color_discrete(name = "Renal Replacement Therapy", labels = c("No", "Yes")) +
    scale_x_date(date_labels = "%b/%y") +
    labs(
        title = "",
        x = "",
        y = "New MV deaths"
    ) +
    # scale_x_date() +
    theme_classic() +
    theme(legend.position = "top")

# ggsave(filename = "plot_death_day_RRT.png",
#        plot = plot_death_day_RRT,
#        width = 5, height = 5, dpi = 800, unit = "in")



library(patchwork)

plot_comb <- 
    (plot_admissions_day + 
         plot_admissions_day_RRT + 
         plot_death_day +
         plot_death_day_RRT) + 
    plot_annotation(tag_level = "A") +
    plot_layout(guides = "collect") &
    theme(
        legend.position = "top", legend.justification = "right"
    )

ggsave(filename = "output/plot_death_day_RRT.png",
       plot = plot_comb,
       width = 7, height = 6, dpi = 800, unit = "in")
# ggsave(filename = "output/plot_death_day_RRT.png",
#        plot = plot_comb,
#        width = 7, height = 6, dpi = 800, unit = "in")

