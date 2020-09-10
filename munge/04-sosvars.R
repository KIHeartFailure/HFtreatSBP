
# Additional variables from NPR -------------------------------------------

pdata <- pdata %>%
  mutate(censdtm = shf_indexdtm + sos_outtime_death)

pdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "hospstroke",
  diakod = " I6[0-4]",
  censdate = censdtm,
  valsclass = "num",
  meta_reg = "NPR (in)/CDR",
  meta_pos = "HDIA/ULORSAK",
  warnings = FALSE
)

pdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "hospacs",
  diakod = " I200| I21| I22",
  censdate = censdtm,
  valsclass = "num",
  meta_reg = "NPR (in)/CDR",
  meta_pos = "HDIA/ULORSAK",
  warnings = FALSE
)

pdata <- pdata %>%
  mutate(
    sos_out_hospstroke = factor(sos_out_hospstroke, labels = c("No", "Yes")),
    sos_out_hospacs = factor(sos_out_hospacs, labels = c("No", "Yes")),
    sos_out_deathhospstroke = case_when(
      sos_out_hospstroke == "Yes" | stringr::str_starts(sos_deathcause, "I60|I61|I62|I63|I64") ~ "Yes",
      TRUE ~ "No"
    ),
    sos_out_deathhospacs = case_when(
      sos_out_hospacs == "Yes" | stringr::str_starts(sos_deathcause, "I200|I21|I22") ~ "Yes",
      TRUE ~ "No"
    ),
    sos_out_deathhf = case_when(
      stringr::str_starts(sos_deathcause, "I110|I130|I132|I255|I420|I423|I42[5-9]|I43|I50|J81|K761|R57") ~ "Yes",
      TRUE ~ "No"
    )
  )

metaout$Variable <- stringr::str_replace_all(metaout$Variable, "sos_out_hosp", "sos_out_deathhosp")
metaout <- rbind(metaout, c("sos_out_deathhf", "ICD:I110,I130,I132,I255,I420,I423,I425-9,I43,I50,J81,K761,R57", "CDR", "ULORSAK", "1-"))
