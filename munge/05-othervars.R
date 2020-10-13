

# Additional variables from mainly SHF ------------------------------------

# income

inc <- pdata %>%
  group_by(shf_indexyear) %>%
  summarise(incmed = quantile(scb_dispincome,
    probs = 0.5,
    na.rm = TRUE
  ), .groups = "drop_last")

pdata <- left_join(
  pdata,
  inc,
  by = "shf_indexyear"
) %>%
  mutate(
    scb_dispincome_cat2 = case_when(
      scb_dispincome < incmed ~ 1,
      scb_dispincome >= incmed ~ 2
    ),
    scb_dispincome_cat2 = factor(scb_dispincome_cat2,
      levels = 1:2,
      labels = c("Below medium within year", "Above medium within year")
    )
  ) %>%
  select(-incmed)

# ntprobnp

ntprobnp_med <- quantile(pdata$shf_ntpropbnp, probs = c(0.5), na.rm = TRUE)

pdata <- pdata %>%
  mutate(
    ## for change over time analysis
    shf_bpsys_diff = shf_bpsys - shf_bpsys_pre,
    supremumdose_diff = supremumdose - supremumdose_pre,

    treatsbp_diff = factor(case_when(
      shf_bpsys_diff > -10 ~ 1,
      shf_bpsys_diff <= -10 & supremumdose_diff > 0 ~ 2,
      shf_bpsys_diff <= -10 & supremumdose_diff <= 0 ~ 3
    ),
    levels = 1:3,
    labels = c("Stable group", "Descrease SBP + increase treat", "Descrease SBP + stable treat")
    ),

    shf_indexdtm_diff = shf_indexdtm - shf_indexdtm_pre,

    # hf hosp between are also counted as pats with hf hosp at last visit
    hfhosp_between = shf_indexdtm_diff >= sos_outtime_hosphf_pre & sos_out_hosphf_pre == "Yes",
    changepop1 = !is.na(shf_indexdtm_pre),
    changepop2 = !is.na(shf_indexdtm_pre) & !hfhosp_between,

    shf_ef = droplevels(shf_ef),

    shf_indexyear_cat = case_when(
      shf_indexyear <= 2005 ~ "2000-2005",
      shf_indexyear <= 2010 ~ "2006-2010",
      shf_indexyear <= 2015 ~ "2011-2015",
      shf_indexyear <= 2018 ~ "2016-2018"
    ),

    shf_smoking_cat = factor(case_when(
      shf_smoking %in% c("Former", "Never") ~ 1,
      shf_smoking == "Current" ~ 2
    ),
    labels = c("Former/Never", "Current"),
    levels = 1:2
    ),

    shf_device_cat = factor(case_when(
      is.na(shf_device) ~ NA_real_,
      shf_device %in% c("CRT", "CRT & ICD", "ICD") ~ 2,
      TRUE ~ 1
    ),
    labels = c("No/Pacemaker", "CRT/ICD"),
    levels = 1:2
    ),

    shf_bmi_cat = case_when(
      is.na(shf_bmi) ~ NA_character_,
      shf_bmi < 30 ~ "<30",
      shf_bmi >= 30 ~ ">=30"
    ),

    shf_bpsys_cat = factor(case_when(
      shf_bpsys < 90 ~ 1,
      shf_bpsys < 100 ~ 2,
      shf_bpsys < 120 ~ 3,
      shf_bpsys < 140 ~ 4,
      shf_bpsys >= 140 ~ 5
    ),
    levels = 1:5,
    labels = c("<90", "90-99", "100-119", "120-139", ">=140")
    ),

    # create comb variable bp/sup treat
    sysbp_suptreat = factor(case_when(
      shf_bpsys_cat == "<90" & supremumdose_cat == "<50%" ~ 20,
      shf_bpsys_cat == "<90" & supremumdose_cat == "50%" ~ 19,
      shf_bpsys_cat == "<90" & supremumdose_cat == "51-99%" ~ 18,
      shf_bpsys_cat == "<90" & supremumdose_cat == "100%" ~ 17,

      shf_bpsys_cat == "90-99" & supremumdose_cat == "<50%" ~ 16,
      shf_bpsys_cat == "90-99" & supremumdose_cat == "50%" ~ 15,
      shf_bpsys_cat == "90-99" & supremumdose_cat == "51-99%" ~ 14,
      shf_bpsys_cat == "90-99" & supremumdose_cat == "100%" ~ 13,

      shf_bpsys_cat == "100-119" & supremumdose_cat == "<50%" ~ 12,
      shf_bpsys_cat == "100-119" & supremumdose_cat == "50%" ~ 11,
      shf_bpsys_cat == "100-119" & supremumdose_cat == "51-99%" ~ 10,
      shf_bpsys_cat == "100-119" & supremumdose_cat == "100%" ~ 9,

      shf_bpsys_cat == "120-139" & supremumdose_cat == "<50%" ~ 8,
      shf_bpsys_cat == "120-139" & supremumdose_cat == "50%" ~ 7,
      shf_bpsys_cat == "120-139" & supremumdose_cat == "51-99%" ~ 6,
      shf_bpsys_cat == "120-139" & supremumdose_cat == "100%" ~ 5,

      shf_bpsys_cat == ">=140" & supremumdose_cat == "<50%" ~ 4,
      shf_bpsys_cat == ">=140" & supremumdose_cat == "50%" ~ 3,
      shf_bpsys_cat == ">=140" & supremumdose_cat == "51-99%" ~ 2,
      shf_bpsys_cat == ">=140" & supremumdose_cat == "100%" ~ 1
    ),
    levels = 1:20,
    labels = c(
      "SBP >=140 mmHg & ST 100%",
      "SBP >=140 mmHg & ST 51-99%",
      "SBP >=140 mmHg & ST 50%",
      "SBP >=140 mmHg & ST <50%",

      "SBP 120-139 mmHg & ST 100%",
      "SBP 120-139 mmHg & ST 51-99%",
      "SBP 120-139 mmHg & ST 50%",
      "SBP 120-139 mmHg & ST <50%",

      "SBP 100-119 mmHg & ST 100%",
      "SBP 100-119 mmHg & ST 51-99%",
      "SBP 100-119 mmHg & ST 50%",
      "SBP 100-119 mmHg & ST <50%",

      "SBP 90-99 mmHg & ST 100%",
      "SBP 90-99 mmHg & ST 51-99%",
      "SBP 90-99 mmHg & ST 50%",
      "SBP 90-99 mmHg & ST <50%",

      "SBP <90 mmHg & ST 100%",
      "SBP <90 mmHg & ST 51-99%",
      "SBP <90 mmHg & ST 50%",
      "SBP <90 mmHg & ST <50%"
    )
    ),

    shf_gfrckdepi_cat = factor(case_when(
      is.na(shf_gfrckdepi) ~ NA_real_,
      shf_gfrckdepi < 60 ~ 1,
      shf_gfrckdepi >= 60 ~ 2,
    ),
    labels = c("<60", ">=60"),
    levels = 1:2
    ),

    shf_ntpropbnp_cat = factor(case_when(
      shf_ntpropbnp < ntprobnp_med ~ 1,
      shf_ntpropbnp >= ntprobnp_med ~ 2
    ),
    levels = 1:2,
    labels = c("Below median", "Above median")
    ),

    shf_logntpropbnp = log(shf_ntpropbnp),

    shf_sos_com_af = case_when(
      sos_com_af == "Yes" |
        shf_af == "Yes" |
        shf_ekg == "Atrial fibrillation" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_ihd = case_when(
      sos_com_ihd == "Yes" |
        shf_revasc == "Yes" |
        sos_com_pci == "Yes" |
        sos_com_cabg == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_hypertension = case_when(
      shf_hypertension == "Yes" |
        sos_com_hypertension == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_diabetes = case_when(
      shf_diabetes == "Yes" |
        sos_com_diabetes == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    # Anemia
    shf_anemia = case_when(
      is.na(shf_hb) ~ NA_character_,
      shf_sex == "Female" & shf_hb < 120 | shf_sex == "Male" & shf_hb < 130 ~ "Yes",
      TRUE ~ "No"
    ),

    # Outcomes
    sos_out_deathcvhosphf = case_when(
      sos_out_deathcv == "Yes" |
        sos_out_hosphf == "Yes" ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  mutate_if(is_character, factor)
