a <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20241112172709_LVL2_RECAP/entities/global_catch_ird_level2_rf1/Markdown/Level0_Firms/data.qs")
a <- a %>% slice_sample(n = 1000)
qs::qsave(x = a, file = "~/firms-gta/geoflow-tunaatlas/jobs/20241112172709_LVL2_RECAP/entities/global_catch_ird_level2_rf1/Markdown/Level0_Firms/data.qs")



a <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20241112172709_LVL2_RECAP/entities/global_catch_ird_level2_rf1/Markdown/Level2_RF1_basic/data.qs")
a <- a %>% slice_sample(n = 1000)
qs::qsave(x = a, file = "~/firms-gta/geoflow-tunaatlas/jobs/20241112172709_LVL2_RECAP/entities/global_catch_ird_level2_rf1/Markdown/Level2_RF1_basic/data.qs")



a <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20241112172709_LVL2_RECAP/entities/global_catch_ird_level2_rf1/Markdown/Level2_RF1_full/data.qs")
a <- a %>% slice_sample(n = 1000)
qs::qsave(x = a, file = "~/firms-gta/geoflow-tunaatlas/jobs/20241112172709_LVL2_RECAP/entities/global_catch_ird_level2_rf1/Markdown/Level2_RF1_full/data.qs")


path_test <- "~/firms-gta/geoflow-tunaatlas/jobs_test/20241112172709_LVL2_RECAP"

source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/Summarising_step.R")
setwd("~/firms-gta/geoflow-tunaatlas")
Summarising_step(main_dir = path_test, connectionDB = con, config  = config, sizepdf = "middle",savestep = TRUE, usesave = FALSE, 
                 source_authoritylist = c("all", "WCPFC", "IATTC", "ICCAT", "CCSBT", "IOTC" ))
