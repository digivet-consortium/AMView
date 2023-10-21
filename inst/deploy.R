scripts <- list.files("R", pattern = "[^(data)].R$", full.names = TRUE)

file.copy(scripts, "inst/shinyapps-io/www", overwrite = TRUE)

setwd("inst/shinyapps-io")

rsconnect::deployApp(appName = "AMView", forceUpdate = TRUE)
