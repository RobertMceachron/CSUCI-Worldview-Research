library(readxl) # For Reading XLSX
library(lavaan) # For FA
library(semPlot) # For FA Plots
library(tidyverse) # For Plots
library(haven) # For SPSS Output

dataraw1 <- read_xlsx("data/WV_V1.xlsx") # Inputs 1st Survey
dataraw2 <- read_xlsx("data/WV_V2.xlsx") # Inputs 2nd Survey


########################
#    Data Cleaning     #
########################

# Removes Extra Rows & Combines Data Sets
  data1trans <- dataraw1[,-c(1,13)]
  data2trans <- dataraw2
  data <- rbind(data1trans, data2trans)

# Removes Pesky Qualtrics Labels >:(
  data <- data[-1,]

# Remove Outliers
  
# Convert to Numeric for analysis 
  #data1$SC1 <- as.numeric(data1$SC19)  
  
  
# Outputs SPSS File for rest of Group
  write_sav(data, "data/Combined_Data.sav")
  
########################
#     Demo Analysis    #
########################

  # Grabs Mean Age
  data$Q2_Age <- as.numeric(data$Q2_Age)
  mean(data$Q2_Age)
  
  # Calculate proportion of Male/Female
  
  
  # Run analysis on scores for the scales
  
  
  
  # Factor analysis on the 6 scored scales 
  # two investment 




###############################
#   Confirm Factor Analysis   #
###############################

  # Interested in Principal Component Analysis?
  # Run Confirmatory Factor Analysis on ? Whatever 10B is
      # Add a scree plot
      # Agency
      # Mutability
      # R2A
      # R2g
      # LOR
      # Metaphysics
      # Self Investment mutability
      # Self Investment LOR
  # Run another one with the 8 but with also the investment scales







########################
#   FACTOR ANALYSIS    #
########################


# Creates the model
  model <- 'Worldview =~ SC1 + SC2 + SC3 + SC4 + SC5 + SC6 + SC12 + SC13'
  cfa_result <- cfa(model, data = data)
  summary(cfa_result)
  semPaths(cfa_result)
  semPaths(cfa_result, "std")

