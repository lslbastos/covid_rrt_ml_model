
# Library -----------------------------------------------------------------
library(tidyverse)


# Data Input --------------------------------------------------------------
df_admissions <- read_csv("input/df_icu_adm_MV_2022-05-31.csv")

df_covid_admissions_mv <- 
    df_admissions %>% 
    # filter(UnitAdmissionDate < as.Date("2022-01-01")) %>% 
    filter(StatusCOVID19 == "confirmado") %>% 
    filter(Age > 15 & Age < 120) %>% 
    filter(Gender != "U") %>% 
    # filter(BMI < 45 | is.na(BMI)) %>% 
    filter(outcome_hosp != "Ongoing") %>% 
    filter(IsNonInvasiveVentilation == 1 | IsMechanicalVentilation == 1) %>% 
    select(-UnitLengthStay) %>%
    rename(
        UnitLengthStay = icu_los_adj
    ) %>% 
    mutate(
        HighestCreatinine1h = case_when(
            between(HighestCreatinine1h, 0.1, 10) ~ HighestCreatinine1h
        ),
        Urea  = case_when(
            between(Urea, 5, 200) ~ Urea 
        ),
        LowestPlateletsCount1h = case_when(
            between(LowestPlateletsCount1h, 5, 500) ~ LowestPlateletsCount1h
        ),
        HighestBilirubin1h = case_when(
            between(HighestBilirubin1h, 0.1, 20) ~ HighestBilirubin1h
        )
    )



## Creating train/test/partitions
sample_lines <- caret::createDataPartition(
    y = df_covid_admissions_mv$ResourceIsRenalReplacementTherapy[df_covid_admissions_mv$UnitAdmissionDate < as.Date("2022-01-01")],
    p = .8,
    list = FALSE,
    times = 1
    )

df_covid_admissions_mv_sets <-
    df_covid_admissions_mv %>%
    mutate(
        set = case_when(
            UnitAdmissionDate < as.Date("2022-01-01") & row_number() %in% sample_lines ~ "train",
            UnitAdmissionDate < as.Date("2022-01-01") & !(row_number() %in% sample_lines) ~ "test",
            TRUE ~ "external_val"
        )
    )





write_csv(df_covid_admissions_mv_sets, "input/df_icu_adm_COVID_RespSupport_first24h_selected_2020_2022.csv")
write_csv(df_covid_admissions_mv_sets %>% filter(set %in% c("train", "test")), "input/df_icu_adm_COVID_RespSupport_first24h_selected_2020_2021.csv", na = "")
write_csv(df_covid_admissions_mv_sets %>% filter(set %in% c("external_val")), "input/df_icu_adm_COVID_RespSupport_first24h_selected_2022.csv", na = "")
# writexl::write_xlsx(df_covid_mv, "df_icu_adm_COVID_RespSupport_selected_2020_2021.xlsx")
