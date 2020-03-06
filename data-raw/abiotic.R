## code to prepare `abiotic` dataset goes here

Fieldfiles <- list.files(path = "data-raw/Water quality", full.names = T, pattern="Field")

Labfiles <- list.files(path = "data-raw/Water quality", full.names = T, pattern="Lab")

wq_field<-sapply(Fieldfiles, function(x) read_excel(x, guess_max = 5e4))%>%
  bind_rows()

wq_lab<-sapply(Labfiles, function(x) read_excel(x, guess_max = 5e4))%>%
  bind_rows()

wq_2000<-read_excel("data-raw/EMP WQ Combined_2000-2018.xlsx", na=c("N/A", "<R.L.", "Too dark"), col_types = c(rep("text", 3), "date", rep("text", 37)))

usethis::use_data(wq_field, wq_lab, wq_2000, overwrite = TRUE)
