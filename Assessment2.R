#Assessment 2.7 clean data for edX course PH527X. 
#Script author Jasmine Spence. This is the first code I've ever written. 
#Please send any feedback to jasminenicolespence@gmail.com

#Load libraries before running the script:
#library(stringr)
#library(dplyr)
#library(gtools)
#library(tidyverse)

dataset = read.csv("dataset.csv", na.strings=c("", "NA", "NULL", "?", "unknown", "Treatment B"))

#Create a numeric binary variable for column "Complications" by making 
#‘No’ = 0 and ‘Yes’ = 1.

dataset$Complications.Encoded [dataset$Complications. == "no"] = "0"
dataset$Complications.Encoded [dataset$Complications. == "NO"] = "0"
dataset$Complications.Encoded [dataset$Complications. == "No"] = "0"
dataset$Complications.Encoded [dataset$Complications. == "n"] = "0"
dataset$Complications.Encoded [dataset$Complications. == "N"] = "0"

dataset$Complications.Encoded [dataset$Complications. == "yes"] = "1"
dataset$Complications.Encoded [dataset$Complications. == "YES"] = "1"
dataset$Complications.Encoded [dataset$Complications. == "Yes"] = "1"
dataset$Complications.Encoded [dataset$Complications. == "y"] = "1"
dataset$Complications.Encoded [dataset$Complications. == "Y"] = "1"

#Rather than separating individuals by the treatment received, create a 
#patient ID variable.

id <- rownames(dataset)
dataset <- cbind(id=id, dataset)

#Create new binary variable Treatment A: "Treatment A" = 1, "Treatment B" = 0
dataset$TreatmentA[dataset$id<=3]<-"1"
dataset$TreatmentA[dataset$id>4]<-"0"

#Create new binary variable Treatment B: "Treatment A" = 0, "Treatment B" = 1
dataset$TreatmentB[dataset$id<=3]<-"0"
dataset$TreatmentB[dataset$id>4]<-"1"

#Create new binary variable Male: "Male" = 1, "Female" = 0
dataset$Male [dataset$Patient.gender == "Male"] = "1"
dataset$Male [dataset$Patient.gender == "male"] = "1"
dataset$Male [dataset$Patient.gender == "M"] = "1"
dataset$Male [dataset$Patient.gender == "m"] = "1"

dataset$Male [dataset$Patient.gender == "Female"] = "0"
dataset$Male [dataset$Patient.gender == "female"] = "0"
dataset$Male [dataset$Patient.gender == "F"] = "0"
dataset$Male [dataset$Patient.gender == "f"] = "0"

#Create new binary variable Female: "Male" = 0, "Female" = 1
dataset$Female [dataset$Patient.gender == "Female"] = "1"
dataset$Female [dataset$Patient.gender == "female"] = "1"
dataset$Female [dataset$Patient.gender == "F"] = "1"
dataset$Female [dataset$Patient.gender == "f"] = "1"

dataset$Female [dataset$Patient.gender == "Male"] = "0"
dataset$Female [dataset$Patient.gender == "male"] = "0"
dataset$Female [dataset$Patient.gender == "M"] = "0"
dataset$Female [dataset$Patient.gender == "m"] = "0"

#Convert tumor stage entries to integers.

dataset[dataset == "II"] <- "2"
dataset[dataset == "III"] <- "3"
dataset[dataset == "IV"] <- "4"

#Make the date of enrollment consistent. 

dataset[dataset == "1/15/99"] <- "1/15/1999"
dataset[dataset == "2/1/1999"] <- "2/1/1999"
dataset[dataset == "2/13/99"] <- "2/13/1999"
dataset[dataset == "Jan 2000"] <- "1/1/2000"
dataset[dataset == "3-1999"] <- "3/1/1999"
dataset[dataset == "4/1999"] <- "4/1/1999"
dataset$Enrol.date.my <- format(as.Date(dataset$Date.enrolled, format = "%m/%d/%Y"), "%m-%Y")

#Convert all dates to number of months since the start of the study.
vec <- c(0,1,1,NA,12,2,NA,3)
dataset$Enrol.m.sincestrt <- vec

#Convert height to either inches or centimeters for consistency.
dataset$cm <- str_extract(dataset$Height, "^([0-9]{3})")
dataset$FeetInches <- str_extract(dataset$Height,"^(?!$|.*\'[^\x22]+$)(?:([0-9]{1,2})\')?(?:([0-9]{1,2})\x22?)?$")
dataset$Inches <- str_extract(dataset$FeetInches,("^([4-6])'?$"))

vec <- c(61,68,47,NA,66,60,68,NA)
dataset$Inches <- vec

#Create a consistent indicator of missing information to replace 
#the ‘?’, ‘unknown’ and ‘NA’ cells. 
#Designate the number -9 as an indicator of missing data.
dataset[is.na(dataset)] <- "-9"

#Create updated data frame and remove observation row 4, previously the "Treatment B" header.
converted_dataset <- subset(dataset, id %in% c(1, 2, 3, 5, 6, 7, 8))

#In updated data frame remove columns of unprocessed data.
converted_dataset <- converted_dataset [,-c(2,4,5,7,8,14,16,17)]

#Export Converted Dataset
write.csv(converted_dataset, "converted_dataset.csv")
