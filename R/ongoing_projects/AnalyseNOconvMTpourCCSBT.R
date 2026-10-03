
# Analyse NO conv MT pour CCSBT 2026  -------------------------------------

data_overlap <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20260302113731level_2_catch_2025/entities/global_catch_ird_level2_1950_2023/Markdown/overlap_iotc_wcpfc/ancient.qs")
head(data_overlap)
summary(data_overlap$measurement_unit)

initial_data_before_conv <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20260302113731level_2_catch_2025/entities/global_catch_ird_level2_1950_2023/Markdown/Removing_data_with_no_nominal/ancient.qs")
head(initial_data_before_conv)
summary(initial_data_before_conv$measurement_unit)
initial_data_before_conv_enriched <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20260302113731level_2_catch_2025/entities/global_catch_ird_level2_1950_2023/Markdown/Removing_data_with_no_nominal/data.qs")
head(initial_data_before_conv_enriched)
summary(initial_data_before_conv_enriched$measurement_unit)

conv_no_nominal <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20260302113731level_2_catch_2025/entities/global_catch_ird_level2_1950_2023/Markdown/Conv_NO_nominal1/ancient.qs")
head(conv_no_nominal)
summary(initial_data_before_conv$measurement_unit)

conv_no_nominal_enriched <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20260302113731level_2_catch_2025/entities/global_catch_ird_level2_1950_2023/Markdown/Conv_NO_nominal1/data.qs")
head(conv_no_nominal_enriched)
summary(initial_data_before_conv$measurement_unit)


lvl2 <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20250620061443level_2_catch_2025/entities/global_catch_ird_level2_1950_2023/Markdown/RF_pass_8/data.qs")
lvl0 <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20250620061443level_2_catch_2025/entities/global_catch_ird_level2_1950_2023/Markdown/rawdata/data.qs")

nrow(lvl0 %>% dplyr::filter(fishing_fleet=="JPN"))
nrow(lvl2 %>% dplyr::filter(fishing_fleet=="JPN"))
