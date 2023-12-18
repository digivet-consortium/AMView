## Info----
# The goal of this script is to import the VetReg-data that we receive and clean/transform it to a standard format that we can use further down the line.
# The standard format origins from the previously automatically received VetReg-data.

# Here the input file is the csv that we receive. This is our start point. 


### Data management----
# Import the csv file
VetReg_new <- read.csv2(file, 
                        header = TRUE, 
                        encoding = "UTF-8", 
                        colClasses = c("mottakerpostnr" = "character", 
                                       "mottakers_produsentnr" = "character")) %>%       
    janitor::clean_names()

# Columns that we know are different in class, we can mutate them
VetReg_new <- VetReg_new %>%
  mutate(
    across(                                                        # across function helps to apply a function across multiple columns
      .cols = c(varenummer),                                       # the columns that need to be modified
      .fns = \(x) as.numeric(x)                                    # We use as.numeric() to change character to numeric
    )
  ) %>%
  mutate(
    across(                                                        
      .cols = c(planavsluttbehandling, registrertdato, utlevertdato),                      # the columns that need to be modified
      .fns = \(x) as.Date(x, format = "%d/%m/%Y")                                                               # We use as.Date() to change character to Date class
    )
  ) %>%
  mutate(
    across(
      .cols = c(antall_pakninger,levert_mengde,snittvekt),
      .fns = \(x) as.numeric(str_replace_all(x,",","."))
    )
  )

export(VetReg_new, new_file)
