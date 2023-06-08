#Exercise Ben Spycher - Thursday

###
# Framingham heart study data
framData <- riskCommunicator::framingham

# We keep only the baseline examination data
framData_base <- subset(framData, TIME == 0)

# Numbers of rows and columns / show first rows and variables
head(framData_base)

###
# German Breast Cancer Study
breastCancer <- tibble(TH.data::GBSG2) 
# tibble wouldn't be necessary, GBSG2 is a dataframe in the package
# for consistency let's turn it into a tibble (tidyverse data frame)

# Numbers of rows and columns / show first rows and variables
head(breastCancer)


