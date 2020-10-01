devtools::build_vignettes()
Report_year=2019
file.copy(file.path("doc", "Delta_Smelt_conditions_report.html"), file.path("docs", paste0("Delta_Smelt_conditions_report_",Report_year, ".html")), overwrite = TRUE)

file.copy(file.path("doc", "Delta_Smelt_conditions_report_full.html"), file.path("docs", paste0("Delta_Smelt_conditions_report_full_",Report_year, ".html")), overwrite = TRUE)
