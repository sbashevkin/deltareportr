devtools::build_vignettes()
Report_year=2019
file.copy(file.path("doc", "Delta_Smelt_conditions_report.html"), file.path("docs", paste0("Delta_Smelt_conditions_report_",Report_year, ".html")))
