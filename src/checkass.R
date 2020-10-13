
ProjectTemplate::reload.project()

dataass <- mice::complete(impdata, 4)


# Cox regression ----------------------------------------------------------

modvarstmp <- modvars[!modvars %in% c("shf_bpsys", "shf_ras", "shf_bbl", "shf_mra")]

mod <- coxph(formula(paste0(
  "Surv(sos_outtime_hosphf, sos_out_deathcvhosphf == 'Yes') ~ supremumdose + shf_bpsys +",
  paste(modvarstmp, collapse = " + ")
)), data = dataass)


# Checking for non-prop hazards -------------------------------------------

print(testpat <- cox.zph(mod))
(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

# check spec for supremumdose
survminer::ggcoxzph(testpat[1])
plot(testpat[1])

# check spec for sysbp
survminer::ggcoxzph(testpat[2])
plot(testpat[2])

# check spec for age
survminer::ggcoxzph(testpat[4])
plot(testpat[4])

# check spec for location
survminer::ggcoxzph(testpat[9])
plot(testpat[9])

# Checking for linearity ---------------------------------------------------

ggcoxfunctional(Surv(sos_outtime_hosphf, sos_out_deathcvhosphf == "Yes") ~ shf_age +
  shf_bpsys +
  shf_heartrate +
  shf_potassium +
  shf_gfrckdepi +
  shf_ntpropbnp,
data = dataass
)
