
# Create supremum tretment variable ---------------------------------------

# Needs to be created on WHOLE dataset due to repeated measures analysis

pdata <- rsdata315 %>%
  filter(casecontrol == "Case") %>%
  mutate(
    shf_ras = case_when(
      is.na(shf_arb) | is.na(shf_acei) ~ NA_character_,
      shf_arb == "Yes" | shf_acei == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_aceidosemax = case_when(
      shf_aceisub == "Captopril" ~ shf_aceidose / 150,
      shf_aceisub == "Cilazapril" ~ shf_aceidose / 5,
      shf_aceisub == "Enalapril" ~ shf_aceidose / 10,
      shf_aceisub == "Fosinopril" ~ shf_aceidose / 20,
      shf_aceisub == "Kinapril" ~ shf_aceidose / 40,
      shf_aceisub == "Lisinopril" ~ shf_aceidose / 20,
      shf_aceisub == "Perindopril" ~ shf_aceidose / 8,
      shf_aceisub == "Ramipril" ~ shf_aceidose / 10,
      shf_aceisub == "Trandolapril" ~ shf_aceidose / 4
    ),
    shf_arbdosemax = case_when(
      shf_arbsub == "Candesartan" ~ shf_arbdose / 32,
      shf_arbsub == "Eprosartan" ~ shf_arbdose / 600,
      shf_arbsub == "Irbesartan" ~ shf_arbdose / 300,
      shf_arbsub == "Losartan" ~ shf_arbdose / 150,
      shf_arbsub == "Telmisartan" ~ shf_arbdose / 80,
      shf_arbsub == "Valsartan" ~ shf_arbdose / 320
    ),
    shf_rasdosemax = case_when(
      shf_ras == "No" ~ 0,
      shf_acei == "Yes" & shf_arb == "No" ~ shf_aceidosemax,
      shf_acei == "No" & shf_arb == "Yes" ~ shf_arbdosemax
    ),
    shf_rasdosemax_cat = case_when(
      shf_ras == "No" ~ 0,
      shf_rasdosemax < 0.5 ~ 0.5,
      shf_rasdosemax >= 0.5 ~ 1
    ),
    shf_bbldosemax = case_when(
      shf_bbl == "No" ~ 0,
      shf_bblsub == "Bisoprolol" ~ shf_bbldose / 10,
      shf_bblsub == "Carvedilol" ~ shf_bbldose / 50,
      shf_bblsub == "Metoprolol" ~ shf_bbldose / 200
    ),
    shf_bbldosemax_cat = case_when(
      shf_bbl == "No" ~ 0,
      shf_bbldosemax < 0.5 ~ 0.5,
      shf_bbldosemax >= 0.5 ~ 1
    ),
    shf_mradosemax = case_when(
      shf_mra == "No" ~ 0,
      shf_mra == "Yes" ~ 1
    ),
    shf_mradosemax_cat = shf_mradosemax,
    shf_mradosemax_sens = case_when(
      shf_indexyear < 2014 ~ NA_real_,
      shf_mra == "No" ~ 0,
      shf_mra == "Yes" ~ shf_mradose / 50
    ),
    shf_mradosemax_cat_sens = case_when(
      shf_indexyear < 2014 ~ NA_real_,
      shf_mra == "No" ~ 0,
      shf_mradosemax_sens < 1 ~ 0.5,
      shf_mradosemax_sens >= 1 ~ 1
    ),
    
    supremumdose = round((shf_rasdosemax_cat +
      shf_bbldosemax_cat +
      shf_mradosemax_cat) / 3, 2),
    supremumdose_sens = round((shf_rasdosemax_cat +
      shf_bbldosemax_cat +
      shf_mradosemax_cat_sens) / 3, 2),
  
    supremumdose_cat = factor(case_when(
      supremumdose < 0.5 ~ 1,
      supremumdose == 0.5 ~ 2,
      supremumdose > 0.5 & supremumdose < 1 ~ 3,
      supremumdose >= 1 ~ 4
    ),
    labels = c("<50%", "50%", "51-99%", "100%"),
    levels = 1:4
    ),
    supremumdose_sens_cat = factor(case_when(
      supremumdose_sens < 0.5 ~ 1,
      supremumdose_sens == 0.5 ~ 2,
      supremumdose_sens > 0.5 & supremumdose_sens < 1 ~ 3,
      supremumdose_sens >= 1 ~ 4
    ),
    labels = c("<50%", "50%", "51-99%", "100%"),
    levels = 1:4
    ),
    
    shf_rasdosemax_cat = factor(shf_rasdosemax_cat,
                                labels = c("No", "<50%", ">=50%"),
                                levels = c(0, 0.5, 1)
    ),
    
    shf_bbldosemax_cat = factor(shf_bbldosemax_cat,
                                labels = c("No", "<50%", ">=50%"),
                                levels = c(0, 0.5, 1)
    ),
    
    shf_mradosemax_cat = factor(shf_mradosemax_cat,
                                labels = c("No", "Yes"),
                                levels = c(0, 1)
    ),
    
    shf_mradosemax_cat_sens = factor(shf_mradosemax_cat_sens,
                                     labels = c("No", "<100%", ">=100%"),
                                     levels = c(0, 0.5, 1)
    )
  ) 