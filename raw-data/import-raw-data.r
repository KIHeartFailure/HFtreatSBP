
ProjectTemplate::reload.project()

# Patient registry from SHFDB3 v 3.1.3, prepared in 08-prep_sosdata.R -----

load(file = "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/data/patreg.RData")

patreg <- patreg %>% 
  filter(sos_source == "sv")

# Store as RData in /data folder ------------------------------------------

save(file = "./data/patreg.RData", list = c("patreg"))