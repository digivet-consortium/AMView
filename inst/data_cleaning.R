library(data.table)

atc <- unique(fread(system.file("extdata/atc.csv", package = "AmView"))[nchar(handelsvarakod) > 0])

subgroups <- fread("inst/extdata/atc_subgroups.csv")

dt_historic1 <- fread("~/shares/s_disk/Digivet_AMU/DSD from portalen/djursjuk_cattle_historic1.csv")

dt_historic1 <- merge(dt_historic1, atc, by.x = "Handelsvarakod", by.y = "handelsvarakod", all.x = T)

dt_historic1 <- dt_historic1[!is.na(atc_kod)]

dt_historic1 <- merge(dt_historic1, )
