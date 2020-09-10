
# Variables for tabs/mods -------------------------------------------------

tabvars <- c(
  # demo
  "shf_sex", 
  "shf_age",
  
  # socec
  "scb_famtype", 
  "scb_child",
  "scb_education",
  "scb_dispincome_cat",
  
  # organizational
  "shf_location",
  "shf_indexyear_cat",
  "shf_followuphfunit", 
  "shf_followuplocation",
  
  # clinical factors and lab measurments
  "shf_bmi", "shf_bmi_cat",
  "shf_ef",
  "shf_smoking_cat",
  "shf_durationhf",
  "shf_nyha",
  "shf_bpsys", "shf_bpsys_cat", 
  "shf_bpdia",
  "shf_map",
  "shf_heartrate",
  "shf_hb", "shf_hb_cat",
  "shf_potassium",
  "shf_gfrckdepi", "shf_gfrckdepi_cat",
  "shf_ntpropbnp", "shf_ntpropbnp_cat", "shf_logntpropbnp",
  
  # treatments
  "shf_diuretic",
  "shf_acei",
  "shf_arb",
  "shf_ras",
  "shf_mra",
  "shf_digoxin",
  "shf_asaantiplatelet",
  "shf_anticoagulantia",
  "shf_statin",
  "shf_nitrate",
  "shf_device_cat",
  
  # comorbs
  "shf_sos_com_hypertension",
  "shf_sos_com_diabetes",
  "shf_sos_com_ihd",
  "sos_com_mi",
  "sos_com_pci",
  "sos_com_cabg",
  "sos_com_peripheralartery",
  "shf_sos_com_af",
  "sos_com_stroketia",
  "sos_com_bleed",
  "sos_com_valvular",
  "sos_com_liver",
  "sos_com_copd",
  "sos_com_alcohol",
  "sos_com_muscoloskeletal3y",
  "sos_com_cancer3y",
  "sos_com_depression"
)

# vars for  regs
tabvars_not_in_mod <- c(
  "shf_ef",
  "shf_acei",
  "shf_arb",
  "shf_map",
  "shf_bpdia",
  "shf_bpsys_cat", 
  "shf_bmi",
  "shf_hb",
  "shf_gfrckdepi_cat",
  "shf_logntpropbnp",
  "shf_ntpropbnp_cat"
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]
# 
# kontvars <- c(
#   "shf_age", "shf_map", "shf_heartrate", "shf_hb",
#   "shf_potassium", "shf_gfrckdepi", "shf_ntpropbnp"
# )
# 
# stratavars <- c("shf_location", "shf_rasarni")
# 
# modvarspartial <- c(
#   "shf_age", "shf_sex",
#   "sos_com_mi", "sos_com_pci",
#   "sos_com_cabg",
#   "sos_com_af",
#   "sos_com_copd",
#   "sos_com_hypertension"
# )
# 
# 
# modvarsns <- modvars
# modvarsns[modvarsns %in% kontvars] <-
#   paste0("ns(", kontvars, ", 3)")
# 
# modvarsnsstrata <- modvarsns
# modvarsnsstrata[modvarsnsstrata %in% stratavars] <-
#   paste0("strata(", stratavars, ")")