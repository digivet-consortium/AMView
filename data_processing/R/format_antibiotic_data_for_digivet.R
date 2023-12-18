options(scipen = 999)
### Information----
# The aim of this script is to take the antibiotic specific data as input and result in a dataframe with columns and information
# in the format required by DigiVet

### Modify columns for DigiVet----

# There are some columns that hold data for antibiotic records that can be directly used for DigiVet 
# (some columns require certain translations). We change the column names of such columns to the column
# names these would correspond in DigiVet.
antibiotics_report <- antibiotics %>%
  rename(EntryID = reportid,                                  # EntryID is the same as reportid
         TypeOfDataSource = utleveringstype,                  # Type of DataSource is utleveringstype (translation required)
         DateTransaction = registrertdato,                    # DateTransaction is registrertdatoc(date of registration)
         DateTreatment = utlevertdato,                        # DateTreatment is same as utlevertdato
         DateOther = planavsluttbehandling,                   # DateOther is set as planavsluttbehandling (end date of treatment)
         NumberOfAnimals = antalldyr,                         # NumberOfAnimals is same as antalldyr (some modifications required)
         Diagnosis = diagnose,                                # Diagnosis is same as diagnose
         MedicalProductName = varenavn,                       # MedicalProductName is same as varenavn
         MedicalProductCode = varenummer,                     # MedicalProductCode is same as varenummer
         ATCcode = atc_kode,                                  # ATCcode is same as atc_kode
         ActiveIngredient = active_moiety)                    # ActiveIngredient is same as active moiety

# There are new columns required for DigiVet which would contain information that we will get from other columns
# or support registers or a combination of the two. We add these columns for now and initialize these as NA, except country which is "NO"
antibiotics_report <- antibiotics_report %>%
  mutate(IncludeAsUse = NA,
         DateOtherComment = NA,
         Country = "NO",
         NUTS2 = NA,
         GeoComment = NA,
         HerdID = NA,
         Gender = NA,
         AgeCategory = NA,
         TreatmentPurpose = NA,
         TreatedUnit = NA_character_)

# Bring all the columns to be delivered for DigiVet to the left of the dataframe
antibiotics_report <- antibiotics_report %>%
  relocate(EntryID, TypeOfDataSource, IncludeAsUse, DateTransaction, DateTreatment, DateOther,
           DateOtherComment, Country, NUTS2, GeoComment, HerdID, Gender, AgeCategory, TreatmentPurpose,
           Diagnosis, TreatedUnit, NumberOfAnimals, MedicalProductName, MedicalProductCode, 
           ATCcode, ActiveIngredient)

### Modify records to comply with DigiVet----

# The TypeOfDataSource column needs to be translated from Norwegian to valid entries accepted by DigiVet
antibiotics_report <- antibiotics_report %>%
  mutate(TypeOfDataSource = case_when(
    TypeOfDataSource == "Utlevering til dyrehelsepersonell fra apotek m.m." ~ "Pharmacy sale (requisition)",
    TypeOfDataSource == "Utlevering til dyrehold fra apotek m.m." ~ "Pharmacy sale (prescription)",
    TypeOfDataSource == "Melding om dyrehelsepersonells bruk av legemidler" ~ "Journal entry (use by veterinarian)",
    TRUE ~ TypeOfDataSource
  ))

# IncludeAsUse is set to FALSE for "Pharmacy sale (requisition)"
antibiotics_report <- antibiotics_report %>%
  mutate(IncludeAsUse = ifelse(TypeOfDataSource == "Pharmacy sale (requisition)", "FALSE", "TRUE"))

# We set the DateOther to "End date of treatment", so we add this information to DateOtherComment column
antibiotics_report <- antibiotics_report %>%
  mutate(DateOtherComment = ifelse(!is.na(DateOther), "End date of treatment", NA))

# Read support register that translates post codes to NUTS3 codes
nuts3_postnr <- read.csv2("data/nuts_3_postnr.csv", header = T)

# Add leading zeros to post codes from support register
nuts3_postnr <- nuts3_postnr %>%
  mutate(postnr = str_pad(as.character(postnr),4,pad = 0))

# Read support register that translates kommune codes to NUTS3 codes
nuts3_komnr <- read.csv2("data/nuts_3_kommnr.csv", header = T)

# Add leading zeros to kommune codes from support register
nuts3_komnr <- nuts3_komnr %>%
  mutate(komnr = str_pad(as.character(komnr),4,pad = 0))

# Add leading zeros to post codes in antibiotic data and
# The first 4 digits of column "mottakers_produsentnr" (when given) specifies the kommune code, so we get these and store it in
# a new "komnr" column
antibiotics_report <- antibiotics_report %>%
  mutate(mottakerpostnr = str_pad(as.character(mottakerpostnr),4,pad = "0"),
         komnr = str_sub(as.character(mottakers_produsentnr), 1, 4))

# rename mottakerpostnr to postnr
antibiotics_report <- antibiotics_report %>%
  rename(postnr = mottakerpostnr)

# Join antibiotic data with nuts2_postnr support register by postnr
antibiotics_report <- left_join(antibiotics_report, nuts3_postnr, by = "postnr")
# Join antibiotic data with nuts2_komnr support register by komnr
antibiotics_report <- left_join(antibiotics_report, nuts3_komnr, by = "komnr")

# When we have a "mottakers_produsentnr" given for a record (which means we have a komnr), we use this to
# get the NUTS3 code for the herd, when we don't have this but have the owner address and postnr, we use this to get the NUTS3 code.
# The same is specified in the GeoComment column.
antibiotics_report <- antibiotics_report %>%
  mutate(NUTS3 = case_when(
    !is.na(NUTS3_komnr) ~ NUTS3_komnr,
    is.na(NUTS3_komnr) & !is.na(NUTS3_postnr) ~ NUTS3_postnr,
    TRUE ~ NA_character_
  )) %>%
  mutate(GeoComment = case_when(
    !is.na(NUTS3_komnr) ~ "Location of herd",
    is.na(NUTS3_komnr) & !is.na(NUTS3_postnr) ~ "Location of owner",
    TRUE ~ NA_character_
  )) %>%
  relocate(NUTS3, .after = NUTS2)

# NUTS2 codes are obtained from the first four digits of NUTS3 codes.
antibiotics_report <- antibiotics_report %>%
  mutate(NUTS2 = str_sub(NUTS3, 1, 4))

# The first 8 digits of "mottakers_produsentnr" column (if given) correspond to the HerdID
antibiotics_report <- antibiotics_report %>%
  mutate(HerdID = case_when(
    !is.na(mottakers_produsentnr) ~ str_sub(as.character(mottakers_produsentnr),1,8),
    TRUE ~ HerdID
  ))

# Read the support register to assign animal types and production type to records according to DigiVet specifications
animal_types <- read.csv2("data/animal_types.csv", header = T, encoding = "latin1")

# Join the support register to antibiotic data by column "dyrekategori"
antibiotics_report <- left_join(antibiotics_report,animal_types, by = "dyrekategori")

# Relocate the added AnimalType and ProductionType column to their proper position
antibiotics_report <- antibiotics_report %>%
  relocate(AnimalType, ProductionType, .after = HerdID)

# Translate the TreatmentPurpose column according to DigiVet protocol
antibiotics_report <- antibiotics_report %>%
  mutate(TreatmentPurpose = case_when(
    beskrivelse == "S: Sykdom" ~ "Therapeutic",
    beskrivelse == "F: Forebyggende behandling inkl. vaksinering" ~ "Prophylactic",
    beskrivelse == "A: Andre" ~ "Undefined",
    TRUE ~ TreatmentPurpose
  ))

# The first three letters of the Diagnosis column give the Diagnosis codes which will be used to assign Indication,
# save these codes in Diagnosis_code column
antibiotics_report <- antibiotics_report %>%
  mutate(Diagnosis_code = str_sub(Diagnosis, 1, 3))

# Read the support register to translate Diagnosis codes to Indication
indication <- read.csv2("data/indication.csv", header = T)

# Join the support register to antibiotic data
antibiotics_report <- left_join(antibiotics_report, indication, by = "Diagnosis_code")

# Relocate the added Indication column to its proper position
antibiotics_report <- antibiotics_report %>%
  relocate(Indication, .after = TreatmentPurpose)

# Read the support register that will translate the Administration Methods from Norwegian to English according
# to DigiVet protocol
administration <- read.csv2("data/administration_methods.csv", header = T, encoding = "latin1")

# Join this support register with antibiotic data
antibiotics_report <- left_join(antibiotics_report, administration, by = "legemiddelform_vi")

# Relocate the added AdministrationMethod column to its proper position
antibiotics_report <- antibiotics_report %>%
  relocate(AdministrationMethod, .after = Diagnosis)

# Change the class of NumberOfAnimals column to character
antibiotics_report <- antibiotics_report %>%
  mutate(NumberOfAnimals = as.character(NumberOfAnimals))

# Modify the NumberOfAnimal column as follows:
# When it is NA, change it to "not applicable". This is the case for "Pharmacy sale (requisition)" entries.
# When it is 0 but "merke" or identification of the animal is given, change it to 1.
# When it is 0 and "merke" is not given then change it to "Not Given".
# When it is 1 or more than 1, we do not make any changes.
antibiotics_report <- antibiotics_report %>%
  mutate(NumberOfAnimals = case_when(
    is.na(NumberOfAnimals) ~ "Not Applicable",
    merke != "" & NumberOfAnimals == 0 ~ "1",
    merke == "" & NumberOfAnimals == 0 ~ "Not Given",
    TRUE ~ NumberOfAnimals
  ))

# Modify the TreatedUnit column as follows:
# When NumberOfAnimals in "not Applicable" or "Not Given", write "-".
# When NumberOfAnimals is 1, write "Individual",
# When NumberOfAnimals is > 1, write "Group".
# A warning is expected
antibiotics_report <- antibiotics_report %>%
  mutate(TreatedUnit = case_when(
    NumberOfAnimals == "Not Applicable" ~ "-",
    NumberOfAnimals == "Not Given" ~ "-",
    NumberOfAnimals == "1" ~ "Individual",
    as.numeric(NumberOfAnimals) > 1 ~ "Group",
    TRUE ~ TreatedUnit
  ))

### Calculate amount of active substance in Kg----

# Change the class of lmp_mengde, lmp_antall and iu_to_mg to numeric
antibiotics_report <- antibiotics_report %>%
  mutate(
    lmp_mengde = as.numeric(lmp_mengde),
    lmp_antall = as.numeric(lmp_antall),
    iu_to_mg = as.numeric(iu_to_mg)
  )

# Change NA in lmp_antall to 1 to ease multiplication
antibiotics_report <- antibiotics_report %>%
  mutate(
    lmp_antall = replace(lmp_antall, is.na(lmp_antall), 1)
  )

# We assign calculation functions in a new column called calculate accordiong to the following criteria:
# Pharmacies always report in the number of packages sold in VetReg so all pharmacy data is assigned function 1
# Hence, for calculation function 1, the amount of active sunstance will be antall_pakninger * lmp_antall * lmp_mengde * styrke_v in the unit styrke_v.
# When vets report in the same unit as the unit of the package, assign calculation function 2
# For this calculation function, the amount of active substance will be styrke_v * levert_mengde in the unit styrke_v.
# When vets report in the "units of packages" (stk,spr) used, assign calculation function 3
# For this calculation function, the amount of active substance will be styrke_v * levert_mengde * lmp_mengde in the unit styrke_v.
# Also there are rare cases when the lmp_mengde is the same as the styrke_v as the package specifications already contain the strength.
# here we assign calculation function 4 and the amount of active substance will be levert_mengde in the unit styrke_v.

antibiotics_report <- antibiotics_report %>%
  mutate(calculate = case_when(
    TypeOfDataSource != "Journal entry (use by veterinarian)" ~ 1,
    TypeOfDataSource == "Journal entry (use by veterinarian)" & enhet_mengde == lmp_enhet_pakning_v & styrke_v == lmp_mengde & styrke_u == lmp_enhet_pakning_v ~ 4,
    TypeOfDataSource == "Journal entry (use by veterinarian)" & enhet_mengde == lmp_enhet_pakning_v ~ 2,
    TypeOfDataSource == "Journal entry (use by veterinarian)" & enhet_mengde == "stk" & lmp_enhet_pakning_v == "g" ~ 3,
    TypeOfDataSource == "Journal entry (use by veterinarian)" & enhet_mengde == "spr" & lmp_enhet_pakning_v == "g"~ 3,
    TypeOfDataSource == "Journal entry (use by veterinarian)" & enhet_mengde == "stk" & lmp_enhet_pakning_v == "ml"~ 3,
    TypeOfDataSource == "Journal entry (use by veterinarian)" & enhet_mengde == "spr" & lmp_enhet_pakning_v == "ml"~ 3,
    TypeOfDataSource == "Journal entry (use by veterinarian)" & enhet_mengde == "spr" & lmp_enhet_pakning_v == "sprøyte"~ 2,
    TypeOfDataSource == "Journal entry (use by veterinarian)" & enhet_mengde == "stk" & lmp_enhet_pakning_v == "sprøyte"~ 2,
    TRUE ~ NA
  )) %>%
  relocate(calculate, .after = ActiveIngredient)

# Perform the calculations based on calculation functions assigned
antibiotics_report <- antibiotics_report %>%
  mutate(
    active_subs_amount = case_when(
      calculate == 1 ~ antall_pakninger * lmp_antall * lmp_mengde * styrke_v,
      calculate == 2 ~ styrke_v * levert_mengde,
      calculate == 3 ~ styrke_v * levert_mengde * lmp_mengde,
      calculate == 4 ~ levert_mengde,
      TRUE ~ 0
    )
  ) %>%
  relocate(active_subs_amount, .after = calculate)

# Use the conversion factor to get the actual amount of active substance and then derive the amount in kilograms
antibiotics_report <- antibiotics_report %>%
  mutate(
    ActiveSubstanceKg = case_when(
      styrke_u == "mg" ~ active_subs_amount * product_to_active / 1000000,
      styrke_u == "g" ~ active_subs_amount * product_to_active / 1000,
      styrke_u == "kg" ~ active_subs_amount * product_to_active,
      styrke_u == "IU" ~ active_subs_amount * iu_to_mg / 1000000,
      styrke_u == "IE" ~ active_subs_amount * iu_to_mg / 1000000,
      styrke_u == "mill IE" ~ active_subs_amount * iu_to_mg,
    )
  ) %>%
  relocate(ActiveSubstanceKg, .after = ActiveIngredient)

# Select the relevant columns for reporting
antibiotics_report <- antibiotics_report %>%
  select(1:27)

# Load anonymization function
sha256_hash <- function(data) {
  openssl::sha256(data)
}

# Anonymize HerdID
antibiotics_report <- antibiotics_report %>%
  dplyr::mutate(
    HerdID= sha256_hash(as.character(HerdID))
  )
