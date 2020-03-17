devtools::build_vignettes()
file.copy(file.path("doc", "Delta_Smelt_conditions_report.html"), file.path("docs", "Delta_Smelt_conditions_report.html"))
