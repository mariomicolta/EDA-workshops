# Load Dataset splitted by (;) and map the empty values ("") to NA
insurance_dataset <-
  read.csv2("./base_clientes.csv", na.strings = c("", "NA"))
# Head of first six elements
head(insurance_dataset)
# Print the structure of the data
str(insurance_dataset)
# Print summary of the data
summary(insurance_dataset)

# ANOMALIES

# WRONG VARIABLE FORMAT
# Print the variable type for each feature because looks like their types does not correspond
cat("The variable type for date is:",
    class(insurance_dataset$date),
    "\n")
cat("The variable type for sex is:",
    class(insurance_dataset$sex),
    "\n")
cat("The variable type for age is:",
    class(insurance_dataset$age),
    "\n")
cat("The variable type for vehicle is:",
    class(insurance_dataset$vehicle),
    "\n")
# Now, let's try to cast some features because they are not using the right format

# First, convert date as a Date format instead of a factor
insurance_dataset$date <-
  as.Date(insurance_dataset$date, format = "%m/%d/%Y")
# Second, convert sex to a factor instead of a integer because it is a qualitative value
insurance_dataset$sex <-
  factor(
    insurance_dataset$sex,
    levels = c("1", "2"),
    labels = c("Female", "Male")
  )
# Third, convert vehicle to boolean because it is a yes or not value
insurance_dataset$vehicle <- as.logical(factor(
  insurance_dataset$vehicle,
  levels = c("YES", "NO"),
  labels = c("True", "False")
))
# Finally, convert age as a cuantitative value instead of a qualitative. For that, cast to an integer value
insurance_dataset[insurance_dataset$custid == 17946,]
suppressWarnings(insurance_dataset$age <-
                   as.integer(as.character(insurance_dataset$age)))
insurance_dataset[insurance_dataset$custid == 17946,]
# Now, when we print the structure again we will see that features are using the right format
str(insurance_dataset)

# DUPLICATED VALUES
# Now it's time to find some duplicated values
# First let's start by searching for duplicated values
table(duplicated(insurance_dataset$custid))

# Due to the id should not be duplicated we delete the copies
insurance_dataset <- unique(insurance_dataset)
# There should not be duplicated data
table(duplicated(insurance_dataset$custid))

# MISSING VALUES
summary(insurance_dataset)
# There are NA's values for sex, is.employed, annual_incomeUSD, marital.stat, health.ins, housing.type,
# vehicle, num.vehicles, age and state.of.res

# First, let's search the only missing value for sex
missing <- insurance_dataset[which(is.na(insurance_dataset$sex)), ]
missing
# Looks like that row has a lot of missing values for the cols, thats why that row is deleted
library(tidyr)
insurance_dataset <- insurance_dataset %>% drop_na(sex)
summary(insurance_dataset)
# Now there are just 4 variables with NA values: is.employed, housing.type, age and num.vehicles

# Let's try to find any relationship between NA num vehicles and vehicles:
summary(insurance_dataset[(insurance_dataset$vehicle == FALSE &
                             insurance_dataset$num.vehicles == 0), c("vehicle", "num.vehicles")])
# Override the NA to cero because if there is a NA value, then there is a "NO"
# value for the vehicle feature
insurance_dataset$num.vehicles <-
  ifelse(is.na(insurance_dataset$num.vehicles),
         0,
         insurance_dataset$num.vehicles)
summary(insurance_dataset)
# For NAs for age, let's save these values as 0.
insurance_dataset$age <-
  ifelse(is.na(insurance_dataset$age), 0, insurance_dataset$age)

# There are just two variables with NA's: is.employed with 328 (33% of the data) and
# housing.type with 56 (6% of the data). Due to housing.type has a narrow percent of
# the data it is safe to drop these customers, but not for is.employed because
# it is the third part of the data.
insurance_dataset <- insurance_dataset %>% drop_na(housing.type)
summary(insurance_dataset)
# For is.employed let's replace the NA's for a new value that encodes for someone who is
# not in active in the actual workforce
insurance_dataset$is.employed <-
  ifelse(
    is.na(insurance_dataset$is.employed),
    "Other",
    ifelse(
      insurance_dataset$is.employed == TRUE,
      "Employee",
      "Not Employee"
    )
  )
insurance_dataset$is.employed <-
  factor(insurance_dataset$is.employed)
summary(insurance_dataset)

# OUTLIERS
# let's create some box plots to find outliers
# First, change the annal_income unit dividing by 1000
insurance_dataset$annual_incomeUSD <-
  insurance_dataset$annual_incomeUSD / 1000

# When the plot is generated, the outliers are more visible
outliers_results <- boxplot(insurance_dataset$annual_incomeUSD)
cat("There are", length(outliers_results$out), "outliers")
# Then let's use simple imputation to replace the annual income outliers for the annual income median
insurance_dataset$annual_incomeUSD <-
  ifelse(
    insurance_dataset$annual_incomeUSD %in% outliers_results$out,
    median(insurance_dataset$annual_incomeUSD),
    insurance_dataset$annual_incomeUSD
  )

# Then the plot has less outliers now
outliers_results <- boxplot(insurance_dataset$annual_incomeUSD)
cat("There are", length(outliers_results$out), "outliers")

# The second value with outliers is age
outliers_results <- boxplot(insurance_dataset$age)
cat("There are", length(outliers_results$out), "outliers")
# then impute values using the median too
insurance_dataset$age <-
  ifelse(
    insurance_dataset$age %in% outliers_results$out,
    median(insurance_dataset$age),
    insurance_dataset$age
  )
# Then the plot has not outliers now
outliers_results <- boxplot(insurance_dataset$age)

# INVALID VALUES
# First, let's impute with the median those data with income <= 0
insurance_dataset$annual_incomeUSD <-
  ifelse(
    insurance_dataset$annual_incomeUSD <= 0,
    median(insurance_dataset$annual_incomeUSD),
    insurance_dataset$annual_incomeUSD
  )
summary(insurance_dataset)

# Second, delete the row with inconsistency because
# it has no vehicule but with 4 num vehicles at the same time
insurance_dataset[insurance_dataset$vehicle == FALSE &
                    insurance_dataset$num.vehicles > 0,]
insurance_dataset <-
  insurance_dataset[!(insurance_dataset$vehicle == FALSE &
                        insurance_dataset$num.vehicles > 0),]

# Third, trim state.of.res values because there are repited values
# that start or finishwith a whitespace. For example, New york
str(insurance_dataset$state.of.res)
insurance_dataset$state.of.res <-
  factor(trimws(insurance_dataset$state.of.res))
summary(insurance_dataset)

# Finally there are some typo for the states, like "Norh Caroli"
# So let's fix them one by one
# Fixing North Carolina
library(plyr)
insurance_dataset$state.of.res <-
  revalue(insurance_dataset$state.of.res,
          c("North Caroli" = "North Carolina"))

# Fixing South Carolina
insurance_dataset$state.of.res <-
  revalue(insurance_dataset$state.of.res,
          c("South Caroli" = "South Carolina"))

# Fixing Arizona
insurance_dataset$state.of.res <-
  revalue(insurance_dataset$state.of.res, c("Arizo" = "Arizona"))

# Fixing Indiana
insurance_dataset$state.of.res <-
  revalue(insurance_dataset$state.of.res, c("India" = "Indiana"))

# Fixing New York
insurance_dataset$state.of.res <-
  revalue(insurance_dataset$state.of.res, c("NewYork" = "New York"))

# Fixing Louisiana
insurance_dataset$state.of.res <-
  revalue(insurance_dataset$state.of.res, c("Louisia" = "Louisiana"))

# Fixing Montana
insurance_dataset$state.of.res <-
  revalue(insurance_dataset$state.of.res, c("Monta" = "Montana"))


# PREPROCESSING STEP
# Delete date column because it does not have a big range between dates
# also does not provide us enough value
insurance_dataset <-
  insurance_dataset[, names(insurance_dataset) != "date"]

# The same for custid because it is unique and for the vehicle col because can be inferred with num.vehicles
insurance_dataset <-
  insurance_dataset[, !names(insurance_dataset) %in% c("custid", "vehicle")]

# Add is.house.owner column if it is Homeowner free and clear or Homeowner with mortgage/loan
insurance_dataset$is.house.owner <-
  ifelse(
    insurance_dataset$housing.type %in% c("Homeowner free and clear", "Homeowner with mortgage/loan"),
    T,
    F
  )
summary(insurance_dataset)
# Finally, add bureau.cardinal.point that groups some states
northeast <-
  c(
    "Connecticut",
    "Maine",
    "Massachusetts",
    "New Hampshire",
    "Rhode Island",
    "Vermont",
    "New Jersey",
    "New York",
    "Pennsylvania"
  )
midwest <-
  c(
    "Illinois",
    "Indiana",
    "Michigan",
    "Ohio",
    "Wisconsin",
    "Iowa",
    "Kansas",
    "Minnesota",
    "Missouri",
    "Nebraska",
    "North Dakota",
    "South Dakota"
  )
south <-
  c(
    "Delaware",
    "Florida",
    "Georgia",
    "Maryland",
    "North Carolina",
    "South Carolina",
    "Virginia",
    "West Virginia",
    "Alabama",
    "Kentucky",
    "Mississippi",
    "Tennessee",
    "Arkansas",
    "Louisiana",
    "Oklahoma",
    "Texas"
  )
west <-
  c(
    "Arizona",
    "Colorado",
    "Idaho",
    "Montana",
    "Nevada",
    "New Mexico",
    "Utah",
    "Wyoming",
    "Alaska",
    "California",
    "Hawaii",
    "Oregon",
    "Washington"
  )
insurance_dataset$bureau.cardinal.point = ifelse(
  insurance_dataset$state.of.res %in% northeast,
  "Northeast",
  ifelse(
    insurance_dataset$state.of.res %in% midwest,
    "Midwest",
    ifelse(
      insurance_dataset$state.of.res %in% south,
      "South",
      ifelse(insurance_dataset$state.of.res %in% west,
             "West",
             "NA")
    )
  )
)
insurance_dataset$bureau.cardinal.point = factor(insurance_dataset$bureau.cardinal.point)

# Delete state.of.res because bureau.cardinal.point creates a set
# for these values
insurance_dataset <-
  insurance_dataset[, names(insurance_dataset) != "state.of.res"]

# Now, the work is done by reordering the column names
insurance_dataset <-
  insurance_dataset[, c(
    "sex",
    "age",
    "marital.stat",
    "housing.type",
    "is.house.owner",
    "bureau.cardinal.point",
    "is.employed",
    "annual_incomeUSD",
    "num.vehicles",
    "health.ins"
  )]

# Thes are the results!
str(insurance_dataset)
summary(insurance_dataset)
head(insurance_dataset)