### Info----
# The goal of this script is to create the final merged file which combines all the data and which will be
# used in the in the future for any purpose.
# The final output of this script is standardized and ready for use
# With this script, we will complete the import section.

### Import data----
years <- seq (from = start_year, to = end_year)

# We run a for loop, first the loop will find all files for one year and combine it into a single data frame called VetReg
# Then it adds the records for any subsequent years.
for (i in years) {
  file <- list.files("transformed", pattern = str_glue("VetReg_{as.character(i)}"))
  for (j in file) {
    records <- read.csv(str_glue("transformed/{j}"), encoding = "Latin-1")
  }
  if (!exists("VetReg")) {
    VetReg <- records
  }
  else {
    VetReg <- rbind(VetReg,records)
  }
}
rm(records)

### Make the final VetReg data frame compatible for merging----
# First we are going to add leading zeros in front of varenummer so that we can merge it with the FEST data
VetReg <- VetReg %>%
  mutate(
    varenummer = str_pad(as.character(varenummer), 6, pad = "0")
  )
# If there are any NA in the varenummer, we replace it with a blank so that we can join without any issues
# We also remove the columns that are not needed
VetReg <- VetReg %>%
  mutate(varenummer = replace(varenummer, is.na(varenummer), "")) %>%
  select(-c(rkid,rekvirent_hprnr,utlevertlm_idstring))
# We also remove NAs from the following columns and replace it with a blank. This is not necessary but
# helps avoid any problems with merging in the future.
VetReg <- VetReg %>%
  mutate(
    reportid = replace(reportid, is.na(reportid), "")
  ) %>%
  mutate(
    reseptid = replace(reseptid, is.na(reseptid), "")
  ) %>%
  mutate(
    merke = replace(merke, is.na(merke), "")
  )

### Merging VetReg with FEST----
# We finally merge VetReg with FEST, we use inner join to get all the data in the VetReg that shows up in FEST
VetReg_merged <- VetReg %>%
  inner_join(Varenr_Virkestoff_unique, by = "varenummer", relationship = "many-to-many")
# Okay to overlook the warning message on unexpected many-to-many relationship (it is expected)

# The ATC codes are almost always consistent, but in case of any discrepancies, we trust the code in FEST
# So we remove atckode and varenavn column that we got from VetReg and other columns not needed from the final merged dataframe
# We also standardize the varenavn column.
VetReg_merged <- VetReg_merged %>%
  select(-c(atckode,varenavn.x,problemer,feilkoder,lm_varenavn)) %>%
  rename(varenavn = varenavn.y)

### Creating a record of data in VetReg where the varenummer does not show up in FEST----
# We use antijoin to get the records where the varenummer in VetReg does not exist in FEST, for this purpose, we use anti-join
Records_Ikkei_Fest <- VetReg %>%
  anti_join(antall_Varenr_Virkestoff, by = c("varenummer"))

# Now we have to add columns to this dataframe so that the column names and numbers are same as VetReg merge
# For this we need to add all the columns of FEST to this data frame
df = data.frame(matrix(NA,nrow=dim(Records_Ikkei_Fest)[1],ncol=19))
colnames(df) <- c (names(Varenr_Virkestoff_unique))
# We remove the varenummer column since it will repeat
df<- df %>%
  select(-c(varenummer))

# varenavn column will also repeat but we are not removing that column because we will add additional data to it
# Instead we are changing the name of the varenavn column in the Records_Ikkei_Fest dataframe
# After that we bind the columns
Records_Ikkei_Fest <- Records_Ikkei_Fest %>%
  rename(varenavn_change = varenavn) %>%
  cbind(df)

# We change the varenavn column according to the following criteria:
# If the atckode starts with an H or Q, we write "ATC-kode som varenummer"
# If the varenummer starts with a 6, we write "Lokalt varenummer"
# If the varenummer starts with any other digit, we write "Ukjent varenummer"
Records_Ikkei_Fest <- Records_Ikkei_Fest %>%
  mutate(
    varenavn = case_when(
      substr(atckode,1,1) == "H" ~ "ATC-kode som varenummer",
      substr(atckode,1,1) == "Q" ~ "ATC-kode som varenummer",
      substr(varenummer,1,1) == 6 ~ "Lokalt varenummer",
      substr(varenummer,1,1) != 6 ~ "Ukjent varenummer",
    )
  )
# Now, if there is already an entry for the varenavn in the original dataframe, we use that, otherwise 
# We use the description that we gave according to the previous criteria.
Records_Ikkei_Fest <- Records_Ikkei_Fest %>%
  mutate(
    varenavn = ifelse (is.na(varenavn_change), varenavn, varenavn_change)
  )

# Remove the columns that are unnecessary so that we have the columns same as the VetReg merged
Records_Ikkei_Fest <- Records_Ikkei_Fest %>%
  select(-c(varenavn_change, atckode,problemer,feilkoder,lm_varenavn))

# As a last modification, add datakilde as Apotek m.m where utleveringstype is "Melding om dyrehelsepersonells bruk av legemidler"
Records_Ikkei_Fest <- Records_Ikkei_Fest %>%
  mutate(
    datakilde = ifelse(utleveringstype != "Melding om dyrehelsepersonells bruk av legemidler", "Apotek m.m.", NA)
  )

### Merge the VetReg records found in FEST and the VetReg records that are not found in FEST----
VetReg_merged <- rbind(VetReg_merged, Records_Ikkei_Fest)

# Change the position of the column reportid so that it is next to the atc_code
VetReg_merged <- VetReg_merged %>%
  relocate(reportid, .after = atc_kode)
# Remove the dataframes that are no longer needed
rm (antall_Varenr_Virkestoff,df,Varenr_Virkestoff_unique,VetReg)

# Saving these two dataframes: VetReg_merged and Records_Ikkei_Fest as rds files.
save(VetReg_merged,file = merged_file)
# Enter the name and path of the rds file you want to save the records not in VetReg. As convention name the rds as "Records_Ikkie_Fest_XX",
# Where XX is the last two digits of the year
new_file_1 <- "Records_Ikkei_Fest_23.rds"
save(Records_Ikkei_Fest,file = check_file_1)
