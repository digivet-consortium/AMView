#' Get ATC codes datasest
#' @noRd
get_atc <- function(include_product = FALSE) {
    dt <- data.table::fread(system.file("extdata/atc.csv", package = "AMView"))

    if (isFALSE(include_product))
        dt <- dt[, -"handelsvarakod"]

    unique(dt)
}

#' Get AMU dataset
#' @noRd
get_amu <- function() {
    dt <- data.table::fread(
        system.file("extdata/amu.csv", package = "AMView"), encoding = "UTF-8"
    )
    atc <- get_atc()

    dt <- data.table::merge.data.table(
        dt,
        atc,
        by.x = "ATCcode",
        by.y = "atc_code",
        all.x = TRUE
    )

    dt
}

#' The variables that go into the common data structure.
#'
#' The names of the list elements are the variable names, and the values are
#' either the legal choices or an empty vector of the correct data type.
#' @param varnames if "all" (default), returns all values. Otherwise returns
#' a subset whose names corresponds to the values given.
#' @return a list as described above.
am_variables <- function(varnames = "all") {
    vars <- list(
        EntryID = integer(),
        TypeOfEntry = c(
            "Pharmacy sale (prescription)",
            "Pharmacy sale (requisition)",
            "Pharmacy sale (other/unknown)",
            "Journal entry (use by veterinarian)",
            "Journal entry (use by animal owner)",
            "Journal entry (prescription by veterinarian)",
            "Journal entry (other)",
            "Other"
        ),
        IncludeAsUse = TRUE,
        DateTransaction = as.Date(NULL),
        DateTreatment = as.Date(NULL),
        DateOther = as.Date(NULL),
        DateOtherComment = character(),
        Country = "SE",
        NUTS2 = paste0("SE", c(11, 12, 21, 22, 23, 31, 32, 33)),
        NUTS3 = paste0(
            "SE",
            c(110, 121, 122, 123, 124, 125, 211, 212, 213, 214, 221, 224, 231,
                232, 311, 312, 313, 321, 322, 331, 332)
        ),
        GeoComment = character(),
        HerdID = character(),
        AnimalType = c("Beef cattle", "Dairy cattle", "Other cattle"),
        ProductionType = c("Breeders", "Meat/Fattening", "Milk"),
        Gender = c("Male", "Female", "Unknown"),
        AgeCategory = c("<6 months", "6-24 months", ">24 months"),
        TreatmentPurpose = c("Metaphylactic", "Treatment"),
        Indication = paste("Indication", 1:10),
        Diagnosis = c(
            "Obstetrics and reproductive disorder",
            "Lameness",
            "Mastitis",
            "Other infection",
            "Other"
        ),
        AdministrationMethod = c(
            "Injectable products", "Intramammary products", "Oral solutions",
            "Oral pastes", "Oral powders", "Premixes", "Tablets and similar",
            "Intrauterine products", "Topical dermatological products",
            "Topical ophthalmological products", "Topical otology products",
            "Topical nasal products", "Other"
        ),
        TreatedUnit = c("Individual", "Group", "Herd"),
        NumberOfAnimals = integer(),
        MedicalProductName = character(),
        MedicalProductCode = get_atc()$handelsvarakod,
        ATCcode = get_atc()$atc_kod,
        ActiveIngredient = get_atc()$substance,
        ActiveSubstanceKg = numeric()
    )

    if (varnames == "all")
        return(vars)

    vars[[varnames]]
}

#' Generate some random AMU dummy data adhering to the common data structure.
#' Limited to Sweden and cattle and. Diagnoses are fake (simply a random number)
#' but ATC codes and medical product IDs are real and fetched from the
#' "atc.csv" file in this package. The EntryID is merely an index of the row
#' number. NOTE! This function takes quite some time to run for large values of
#' \code{n_rows}.
#' @param n_rows the number of rows in the dataset
#' @param start_date The earliest possible date of transaction
#' @param end_date The latest possible date of transaction.
#' @return a data.frame with the dummy data
#' @export
amu_dummy_data <- function(n_rows, start_date, end_date) {
    atc <- get_atc(include_product = TRUE)

    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    stopifnot(is.numeric(n_rows), n_rows >= 1, n_rows %% 1 == 0)
    stopifnot(end_date > start_date)

    vars <- am_variables()

    #nolint start
    EntryID  <- seq_len(n_rows)
    TypeOfEntry <- sample(vars$TypeOfEntry, n_rows, TRUE)
    IncludeAsUse <- rep(vars$IncludeAsUse, n_rows)
    DateTransaction <- sample(seq(start_date, end_date, "day"), n_rows, TRUE)
    DateTreatment <- as.Date(sapply(seq_len(n_rows), function(i) {
        sample(seq(DateTransaction[i], DateTransaction[i] + 3, "day"), 1, TRUE)
    }))
    DateOther <- as.Date(rep(NA, n_rows))
    DateOtherComment <- rep(NA_character_, n_rows)
    Country <- sample(vars$Country, n_rows, TRUE)
    NUTS2 <- sample(vars$NUTS2, n_rows, TRUE)
    NUTS3 <- sapply(seq_len(n_rows), function(i) {
        sample(vars$NUTS3[startsWith(vars$NUTS3, NUTS2[i])], 1, TRUE)
    })
    GeoComment <- rep("Location in Sweden", n_rows)
    HerdID <- paste0(Country, "_", sample(1:25, n_rows, TRUE))
    AnimalType <- sample(vars$AnimalType, n_rows, TRUE)
    ProductionType <- sample(vars$ProductionType, n_rows, TRUE)
    Gender <- sample(vars$Gender, n_rows, TRUE)
    AgeCategory <- sample(vars$AgeCategory, n_rows, TRUE)
    TreatmentPurpose <- sample(vars$TreatmentPurpose, n_rows, TRUE)
    Indication <- sample(vars$Indication, n_rows, TRUE)
    Diagnosis <- sample(vars$Diagnosis, n_rows, TRUE)
    AdministrationMethod <- sample(vars$AdministrationMethod, n_rows, TRUE)
    TreatedUnit <- sample(vars$TreatedUnit, n_rows, TRUE)
    NumberOfAnimals <- sapply(seq_len(n_rows), function(i) {
        if (TreatedUnit[i] == "Individual")
            return(1)
        sample(2:250, 1, TRUE)
    })
    product_index <- sample(seq_len(nrow(atc)), n_rows, TRUE)
    MedicalProductName <- rep(NA_character_, n_rows)
    MedicalProductCode <- atc$handelsvarakod[product_index]
    ATCcode <- atc$atc_code[product_index]
    ActiveIngredient <- atc$substance[product_index]
    ActiveSubstanceKg <- sample(seq(0.5, 10, 0.5), n_rows, TRUE)

    data <- data.frame(
        EntryID,
        TypeOfEntry,
        IncludeAsUse,
        DateTransaction,
        DateTreatment,
        DateOther,
        DateOtherComment,
        Country,
        NUTS2,
        NUTS3,
        GeoComment,
        HerdID,
        AnimalType,
        ProductionType,
        Gender,
        AgeCategory,
        TreatmentPurpose,
        Indication,
        Diagnosis,
        AdministrationMethod,
        TreatedUnit,
        NumberOfAnimals,
        MedicalProductName,
        MedicalProductCode,
        ATCcode,
        ActiveIngredient,
        ActiveSubstanceKg
    )
    #nolint end

    colnames(data) <- names(vars)

    data <- data[order(data$DateTransaction, data$EntryID), ]

    data$EntryID <- rownames(data) <- seq_len(nrow(data))

    data
}

#' @noRd
get_spatial_data <- function(country_codes) {
    countries <- eurostat::get_eurostat_geospatial(
        output_class = "sf",
        nuts_level = "3",
        make_valid = FALSE
    )

    stopifnot(all(country_codes %in% countries$CNTR_CODE))

    countries[countries$CNTR_CODE %in% country_codes, ]
}
