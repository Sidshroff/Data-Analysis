str(RequiredData_F1)
# check the current working directory and change it to the directory where data files are stored
getwd()
setwd("C:/Users/Siddhi/Desktop/Personal/ASU/Applied Project/Data Analysis/Data Pre_Processing Final/Mgmc")

remove(RequiredData_F1)
RequiredData_F1
# Load the csv files in R and putting NA for blank values
RequiredData_F1 <- read.csv("Mgmc_AllData_RequiredColumns.csv", na.strings=c("","NA"))


# Check if the data is populated properly by checking first few rows
head(RequiredData_F1)

# Exploring the data
## Checking the class of the data
class(RequiredData_F1)

## Checking deimensions of the imported data
dim(RequiredData_F1)

##Checking the column names
names(RequiredData_F1)

## Checking the structure of the data

## checking the structure of data through dplyr package and glimpse function
install.packages("dplyr")
library("dplyr")
glimpse(RequiredData_F1)

## Getting the summary of data
summary(RequiredData_F1)

## Checking first few rows of data 
head(RequiredData_F1, n= 10)

## Checking the distribution of numerical data
hist(RequiredData_F1$Charges)

## Creating a scatterplot to see the relation between two variables
plot(RequiredData_F1$Service.Line, RequiredData_F1$Charges)

# Check for missing values for all data columns
## Converting all the missing values in data frame to NA
colSums(is.na(RequiredData_F1))

## Check for unique values in the column P_Account_Number
length(unique(RequiredData_F1$P_Account_Number))

# Extract month from admit and discharge date and DOB column if both dates are in the same month
## transform the dates column in a format which is easier to work with in R in format year-month-date

RequiredData_F1$Admit1 <- as.Date(RequiredData_F1$Admit, format = "%m/%d/%Y")
RequiredData_F1$Discharge1 <- as.Date(RequiredData_F1$Discharge, format = "%m/%d/%Y")
RequiredData_F1$DOB <- as.Date(RequiredData_F1$DOB, format = "%m/%d/%Y")

## Extract month and year from admit1 and discharge1 using the lubridate package
install.packages("lubridate")
library("lubridate")
RequiredData_F1$Month_Admit1 <- month(RequiredData_F1$Admit1)
RequiredData_F1$Month_Discharge1 <- month(RequiredData_F1$Discharge1)
RequiredData_F1$Year_Admit1 <- year(RequiredData_F1$Admit1)
RequiredData_F1$Year_Discharge1 <- year(RequiredData_F1$Discharge1)

## compare month and year for admit and discharge date to see they are in same month and same year
identical(RequiredData_F1$Month_Admit1, RequiredData_F1$Month_Discharge1 )
identical(RequiredData_F1$Year_Admit1, RequiredData_F1$Year_Discharge1 )

## since all the months and year is not equal, we want to extract the subset of rows where its not same 
## respectively for month and year to a seperate file
sum(RequiredData_F1$Month_Admit1!=RequiredData_F1$Month_Discharge1 &
      RequiredData_F1$Year_Admit1!=RequiredData_F1$Year_Discharge1)
sum(RequiredData_F1$Year_Admit1!=RequiredData_F1$Year_Discharge1)
MonthNotEqual <- subset(RequiredData_F1, Month_Admit1 != Month_Discharge1)
YearNotEqual <- subset(RequiredData_F1, Year_Admit1 != Year_Discharge1)

# Update zip codes of cities where zip code is missing but city is given
## Check for which cities are zip codes missing
print(subset(RequiredData_F1, !is.na(City) & is.na(Zip.Code)))

## check if age is missing if DOB is given
print(subset(RequiredData_F1, !is.na(DOB) & is.na(Age)))

# Update missing values
## Update the zip codes for rows- Nevada City :95959, Las Vegas: 89101,where cities are present but zip code is missing
RequiredData_F1$Zip.Code[RequiredData_F1$City == "LAS VEGAS"] <- 89101
RequiredData_F1$Zip.Code[RequiredData_F1$City == "NEVADA CITY"] <- 95959

## Update the age where age is missing and DOB is present
RequiredData_F1$Age[is.na(RequiredData_F1$Age) & !is.na(RequiredData_F1$DOB)] <- as.numeric(round((RequiredData_F1$Discharge1-RequiredData_F1$DOB)/365), digits = 0)


## subset and export data for all columns seperately relevance for analysis and has missing values
RecordNo_Missing <- subset(RequiredData_F1, is.na(M_Record_Number))
Age_Missing <- subset(RequiredData_F1, is.na(Age))
ZipCode_Missing <- subset(RequiredData_F1, is.na(City) & is.na(Zip.Code))
head(RecordNo_Missing)
help(years)

## export all the value we are not using, for missing values and other purposes
install.packages("xlsx")
library(xlsx)
write.csv(MonthNotEqual, "MonthNotEqual.csv")
write.csv(YearNotEqual, "YearNotEqual.csv")
write.csv(RecordNo_Missing, "RecordNumber_Missing.csv")
write.csv(Age_Missing, "Age_Missing.csv")
write.csv(ZipCode_Missing, "ZipCode_Missing.csv")

# Removing duplicates for the data not being used in the analysis
## Load the files containing all the unused data
DataNotUsed_All <- read.csv("Master_Unused_WithDuplicates.csv")
UniqueNotUsedData <- DataNotUsed_All[!duplicated(DataNotUsed_All$P_Account_Number),]


##Exporting the file with unique account number of unused data
write.csv(UniqueNotUsedData, "DataNotUsed_Complete.csv")

## Extracting and exporting the file for all the data to be used
CompleDataUsed <- RequiredData_F1[!(RequiredData_F1$P_Account_Number %in% UniqueNotUsedData$P_Account_Number),]
write.csv(CompleDataUsed, "DataUsed_Complete.csv")
dim(CompleDataUsed)
summary(CompleDataUsed)


