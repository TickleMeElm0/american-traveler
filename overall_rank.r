# Load necessary libraries
library(dplyr)
library(readxl)

# Load data
nursedatacan <- read_excel("/Users/diegoperez/Desktop/Data.xlsx", sheet = 1)
nursedatapos <- read_excel("/Users/diegoperez/Desktop/Data.xlsx", sheet = 2)

# Define NurseSubset in a wider scope
NurseSubset <- NULL

# Define function to filter nurse data
NurseFunction <- function(nurse_num) {
  # Extract information for the given nurse number
  nurse <- nursedatacan[nurse_num,]
  ProfText <- nurse$ProfText
  SpecText <- nurse$SpecText
  ST_Pref <- nurse$ST_Pref
  
  # Filter nurse data based on professional text, specialization text, and state preference
  NurseFilter <- filter(nursedatapos, Prof == ProfText, GroupSpec == SpecText, ST == ST_Pref)
  
  # Check if no records found for the given specialization and state preference
  if (nrow(NurseFilter) == 0) {
    filST_Pref <- filter(nursedatapos, Prof == ProfText, ST == ST_Pref)
    if (nrow(filST_Pref) == 0) {
      filProf <- filter(nursedatapos, Prof == ProfText)
      NurseSubset <<- filProf  # Assign the filtered data to NurseSubset using global assignment operator
    } else {  
      NurseSubset <<- filST_Pref  # Assign the filtered data to NurseSubset using global assignment operator
    }
  } else {
    NurseSubset <<- NurseFilter  # Assign the filtered data to NurseSubset using global assignment operator
  }
}

# User input for candidate number
Nurse_num <- as.integer(readline("Enter the candidate number: "))

# Call NurseFunction with the given candidate number
NurseFunction(Nurse_num)

# Now you can use NurseSubset outside of NurseFunction
# Add a new column to your subset data frame containing the ranks
NurseSubset$rank <- rank(NurseSubset[, ncol(NurseSubset)], ties.method = "min")

# Read the value of the first row and the second-to-last column
max_value <- max(NurseSubset$EstWklyGross)
max_value <- as.numeric(as.character(max_value))

# Function to iterate over each value in a column and divide a constant x by each value
iterate_and_divide <- function(data, column_index, x) {
    # Iterate over each value in the specified column and divide x by each value
  result <- vector("numeric", length = length(data[[column_index]]))
  for (i in seq_along(data[[column_index]])) {
    result[i] <- data[[column_index]][i] / x
  }
  
  # Return the result
  return(result)
}

# Suppose NurseSubset is your data frame and you want to iterate over the 'your_column_name' column:
column_index <- "EstWklyGross"
x <- max_value
# Perform the iteration and division
result <- iterate_and_divide(NurseSubset, column_index, x)

# Create a new data frame with all existing columns from NurseSubset and the new column
NurseSubset_with_new_column <- cbind(NurseSubset, percent_rank = result)

# Create a new dataset with only the last two columns
simple_dataset <- select(NurseSubset_with_new_column, -posid, -City, -ST, -Prof, -GroupSpec, -Shift, -'#Wks', -WklyHrs, -EstWklyGross)

# OVERALLSCORE function
# Define the percentages and their weights
percent_salary <- 0.51
percent_distance <- 1

# Get user input for weights
weight_salary <- as.numeric(readline("Enter the weight for salary (in decimal form): "))
weight_distance <- as.numeric(readline("Enter the weight for distance (in decimal form): "))

# Calculate the overall percentage
overall_percentage <- (percent_salary * weight_salary) + (percent_distance * weight_distance)

print(overall_percentage)