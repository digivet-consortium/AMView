### Information----
# The VetReg data that has been standardized will be used here. The purpose of this script is to get the antibiotic
# specific records from VetReg and standardise it.

### Input data to work on----
# Read support registers:

# Antibiotics ATC codes
AB_atc <- import("data/ATC_codes_antimicrobials.csv") %>%
  clean_names()

# Antibiotics class
# Here we use encoding = "Latin-1" to read Norwegian letters
# Using header as FALSE because there are two columns with merged header here so we will assign header row later
# Also we only need the first 6 columns so we use [1:6]
AB_class <- import("data/ABvirkestoff tilegnet ABklasser.csv", encoding = "Latin-1", header = F)[,1:6]
AB_class <- AB_class %>%
  row_to_names(row_number = 1) %>%
  clean_names()
AB_class <- AB_class %>%
  rename(ab_klasse = a_bklasse,
         ab_rapport = a_brapport,
         ab_klasse_2 = abklasse_2,
         ab_klasse_engelsk = abklasse_engelsk)    #again, some manual column name cleaning is required
# Select unique combinations of (virkestoff_navn, ab_klasse, ab_rapport and remove the rows where virkestoff_navn is not given
AB_class <- AB_class %>%
  distinct(virkestoff_navn, ab_klasse, ab_rapport) %>%
  filter(virkestoff_navn !="")

# Antibiotics conversion factor
AB_conversion <- import("data/AB conversion factors.csv", encoding = "Latin-1") %>%
  clean_names()
# Only select the relevant columns and rename those columns to make the column names standard across all support registers
AB_conversion <- AB_conversion %>%
  select(virkestoff_navn = derivat_virkestoff_i_iu_norsk,
         active_moiety = aktivt_virkestoffstoff_engelsk,
         iu_to_mg = iu_to_mg_conversion_factors,
         product_to_active = conversion_factors_of_derivatives)
# The active moiety for Bacitracin is Bacitracin, we add this information:
AB_conversion <- AB_conversion %>%
  dplyr::mutate(active_moiety = replace(active_moiety, virkestoff_navn == "Bacitracin", "Bacitracin"))
# There are commas that need to be replaced with decimals in AB_conversion
AB_conversion <- AB_conversion %>%
  mutate(
    across(
      .cols = c(iu_to_mg, product_to_active),
      \(x) as.numeric(gsub(",",".",x))
    )
  )
# Some more cleaning on AB_conversion: remove rows where there is no data, use 1 instead of NA in column product_to_active
AB_conversion <- AB_conversion %>%
  filter(!is.na(iu_to_mg) & virkestoff_navn !="")
AB_conversion <- AB_conversion %>%
  mutate(product_to_active = case_when(is.na(product_to_active) ~ 1,
                                       TRUE ~ product_to_active)) %>%
  distinct()

# Antibiotics short names
form_short_name <- import("data/Legemiddelform_norsk_engelsk.csv", encoding = "Latin-1") %>%
  clean_names()
# Select unique combinations of legemiddelform_kort_dn, legemiddelform_vi and remove rows where legemiddelform_kort_dn is not given
form_short_name <- form_short_name %>%
  distinct(legemiddelform_kort_dn, legemiddelform_vi) %>%
  filter(legemiddelform_kort_dn !="")

### Load VetReg data----

# Read the VetReg files
years <- seq (from = start_year, to = end_year)

# We run a for loop, first the loop will find all rds object in which the formatted VetReg is stored
# for one year and store it in a dataframe called VetReg
# Then it adds the records for any subsequent years.
for (i in years) {
  file <- list.files("transformed", pattern = str_glue("merged_",as.character({i})))
  for (j in file) {
    load(str_glue("transformed/{j}"))
    if (!exists("VetReg")) {
      VetReg <- VetReg_merged
    } else {
      VetReg <- rbind(VetReg,VetReg_merged)
    }
    rm(VetReg_merged)
  }
}

### Deriving antibiotic specific data and cleaning VetReg data----

VetReg <- VetReg %>%
  mutate(
  across(                                                        
    .cols = c(planavsluttbehandling, registrertdato, utlevertdato),                      # the columns that need to be modified
    .fns = \(x) lubridate::ymd(x)                                                        # We use lubridate::ymd() to change character to Date class
  )
)
# Select the columns where the delivery date is in the year required
VetReg <- VetReg %>%
  filter(format(utlevertdato, "%Y") == "2023")

# Select antibiotic specific data. Select the rows where the ATC code in VetReg is in AB_atc support register
antibiotics <- VetReg %>%
  filter(substr(atc_kode,1,8) %in% AB_atc$atc_code | substr(atc_kode,1,7) %in% AB_atc$atc_code |
           substr(atc_kode,1,6) %in% AB_atc$atc_code | substr(atc_kode,1,5) %in% AB_atc$atc_code | 
           substr(atc_kode,1,4) %in% AB_atc$atc_code | substr(atc_kode,1,3) %in% AB_atc$atc_code)

# Join the antibiotic dataframe with form_short_name (by legemiddelform_kort_dn)
# and AB_class support register (by virkestoff_navn)
antibiotics <- left_join(antibiotics, form_short_name, by = "legemiddelform_kort_dn")
antibiotics <- left_join(antibiotics, AB_class, by = "virkestoff_navn")

# After manually checking against SPC, change the active substance name from Benzylpenicillin to Benzylpenicillinprokain for the following varenummers
antibiotics <- antibiotics %>%
  dplyr::mutate (
    virkestoff_navn = replace(
      virkestoff_navn, (varenummer == "001994" | varenummer == "019403" | varenummer == "027429" |
                          varenummer == "027442" | varenummer == "076460" | varenummer == "144907" |
                          varenummer == "402735" | varenummer == "454918" | varenummer == "511675" |
                          varenummer == "529597" | varenummer == "560289") 
      & virkestoff_navn == "Benzylpenicillin", "Benzylpenicillinprokain"
    )
  )

# After manually checking against SPC, change the active substance name from Benzylpenicillin to Benzylpenicillinbenzatin for the following varenummers
antibiotics <- antibiotics %>%
  dplyr::mutate (
    virkestoff_navn = replace(
      virkestoff_navn, (varenummer == "264994") 
      & virkestoff_navn == "Benzylpenicillin", "Benzylpenicillinbenzatin"
    )
  )

# After manually checking against SPC, change the active substance name from Benzylpenicillin to Penicillinbenethamin for the following varenummers
antibiotics <- antibiotics %>%
  dplyr::mutate (
    virkestoff_navn = replace(
      virkestoff_navn, (varenummer == "205876" | varenummer == "279687") 
      & virkestoff_navn == "Benzylpenicillin", "Penicillinbenethamin"
    )
  )

# Join the antibiotics dataframe with AB_conversion support register
# Virkestoff_navn is the name of the column for which the strength is given 
# This column is a mixture of derivative and active moiety
antibiotics <- left_join(antibiotics, AB_conversion[,c("virkestoff_navn","active_moiety")],  by = "virkestoff_navn")
antibiotics <- antibiotics %>%
  relocate(active_moiety, .after = virkestoff_navn)
antibiotics <- antibiotics %>%
  mutate(active_moiety = case_when(
    is.na(active_moiety) ~ virkestoff_navn,
    TRUE ~ active_moiety
  ))

antibiotics <- left_join(antibiotics, AB_conversion[,c("virkestoff_navn","iu_to_mg")],  by = "virkestoff_navn")
antibiotics <- left_join(antibiotics, AB_conversion[,c("virkestoff_navn","product_to_active")], by = "virkestoff_navn")
antibiotics <- antibiotics %>%
  mutate(
    product_to_active = replace(product_to_active, is.na(product_to_active), 1)
  )

### Formatting----

#removing all variables from the environment except antibiotics
rm (list = setdiff(ls(),"antibiotics"))

# Change the delivery type where the vet has probably used the wrong form of service
antibiotics <- antibiotics %>%
  mutate(
    utleveringstype = replace(
      utleveringstype, utleveringstype == "Utlevering til dyrehold fra apotek m.m." &
        kilde == "Skjematjeneste" & 
        aktivitet != "Akvakultur fisk" &
        legemiddelform_vi != "Premiks", "Melding om dyrehelsepersonells bruk av legemidler"
    )
  )

#I like using replace_tag, this will be used in the next step
antibiotics <- antibiotics %>%
  mutate(
    replace_tag = NA
  )

# Correct the strength and strength denominator where package is in kg or L
# The following changes need to be made:
# When strength is in mg and strength denominator is in g and package in in kg, change strength to g and strength denominator to kg
# When strength is in mg and strength denominator is in ml and package in in L, change strength to g and strength denominator to L
# When strength denominator volume is not given and strength denominator unit is kg and package in in kg, change strength denominator volume to 1
# When strength denominator volume and unit are not given and package is in ml or not given and the form is injection, change strength denominatior volume to 1 and unit to ml
antibiotics <- antibiotics %>%
  mutate(
    replace_tag = 
      replace(replace_tag, styrke_u == "mg" & styrke_nevner_u == "g" & lmp_enhet_pakning_v == "kg", 1),
    replace_tag =
      replace(replace_tag, styrke_u == "mg" & styrke_nevner_u == "ml" & lmp_enhet_pakning_v == "L", 2),
    replace_tag =
      replace(replace_tag, is.na(styrke_nevner_v) & styrke_nevner_u == "kg" & lmp_enhet_pakning_v == "kg", 3),
    replace_tag =
      replace(replace_tag,is.na(styrke_nevner_v) & is.na(styrke_nevner_u) & lmp_enhet_pakning_v %in% c("","ml") & legemiddelform_vi == "Injeksjon", 4)
  ) 

antibiotics <- antibiotics %>%
  mutate(
    styrke_u = replace(styrke_u, replace_tag == 1, "g"),
    styrke_u = replace(styrke_u, replace_tag == 2, "g"),
    styrke_nevner_u = replace(styrke_nevner_u, replace_tag == 1, "kg"),
    styrke_nevner_u = replace(styrke_nevner_u, replace_tag == 2, "L"),
    styrke_nevner_v = replace(styrke_nevner_v, replace_tag == 3, 1),
    styrke_nevner_u = replace(styrke_nevner_u, replace_tag == 4, "ml"),
    styrke_nevner_v = replace(styrke_nevner_v, replace_tag == 4, 1)
  ) %>%
  mutate(
    replace_tag = NULL
  )
# The path and name of the final rds that will be exported (This name is generally antibiotics_XXXX.rds, where XXXX is the year)
new_file <- ("transformed/antibiotics_2023.rds")
export(antibiotics, new_file)
