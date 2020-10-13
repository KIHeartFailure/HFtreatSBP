
# Inclusion/exclusion criteria --------------------------------------------------------

flow <- c("Number of posts (cases) in SHFDB3", nrow(pdata), NA)

pdata <- pdata %>%
  mutate(koll = case_when(
    is.na(shf_ef) ~ NA_character_,
    shf_ef %in% c("<30", "30-39") ~ "Yes",
    TRUE ~ "No"
  ))
flowtabFunc("HFrEF(<=39%)")

pdata <- pdata %>%
  mutate(koll = ifelse(!is.na(shf_bpsys), "Yes", NA))
flowtabFunc("No missing systolic BP")

pdata <- pdata %>%
  mutate(koll = case_when(
    shf_arni != "Yes" | is.na(shf_arni) ~ "Yes",
    TRUE ~ "No"
  ))
flowtabFunc("Not recieving ARNI (missing is assumed not recieving)")

pdata <- pdata %>%
  mutate(koll = ifelse(!is.na(shf_arb) & !is.na(shf_acei) & !is.na(shf_bbl) & !is.na(shf_mra), "Yes", NA))
flowtabFunc("No missing acei/arb/bbl/mra")

pdata <- pdata %>%
  mutate(koll = case_when(
    shf_arb == "Yes" & (is.na(shf_arbsub) | is.na(shf_arbdose)) ~ NA_character_,
    shf_acei == "Yes" & (is.na(shf_aceisub) | is.na(shf_aceidose)) ~ NA_character_,
    shf_bbl == "Yes" & (is.na(shf_bblsub) | is.na(shf_bbldose)) ~ NA_character_,
    TRUE ~ "Yes"
  ))
flowtabFunc("Not missing substance/dose if recieving acei/arb/bbl")

pdata <- pdata %>%
  mutate(koll = ifelse(shf_arb == "Yes" & shf_acei == "Yes", "No", "Yes"))
flowtabFunc("Not recieving both arb and acei")

pdata <- pdata %>%
  mutate(koll = ifelse(shf_bbl == "Yes" & shf_bblsub %in%
    c("Bisoprolol", "Carvedilol", "Metoprolol") | shf_bbl == "No",
  "Yes", "No"
  ))
flowtabFunc("Not recieving other bbl than Bisoprolol, Carvedilol, Metoprolol")

pdata <- pdata %>%
  group_by(LopNr) %>%
  arrange(shf_indexdtm) %>%
  slice(c(n() - 1, n()))

pdata2 <- pdata %>%
  slice(n() - 1) %>%
  ungroup()

pdata <- pdata %>%
  slice(n()) %>%
  ungroup()

pdata <- left_join(
  pdata,
  pdata2 %>% select(LopNr, shf_bpsys, contains("supremum"), shf_indexdtm, sos_out_hosphf, sos_outtime_hosphf),
  by = "LopNr",
  suffix = c("", "_pre")
)

flow <- rbind(flow, c("Last registration / patient", nrow(pdata), NA))

flow <- rbind(flow, c("Whereof have a prior visit fullfilling the above (objective C)", nrow(pdata2), NA))

pdata_sens <- pdata %>%
  filter(
    shf_indexyear >= 2014,
    (shf_mra == "Yes" & !is.na(shf_mradose) | shf_mra == "No")
  )

flow <- rbind(
  flow,
  c(
    "Whereof indexyear >= 2014 and non-missing dose if recieving mra (consistency analysis)",
    nrow(pdata_sens), NA
  )
)

colnames(flow) <- c("Criteria", "Included", "Excluded (wherof excluded due to missing)")
