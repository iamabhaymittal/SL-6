getwd()


#loading dataset
data <- read.csv("/home/pict/Desktop/Sl-VI DataSets/BreastCancer/BreastCancerWc.csv",header = F)
head(data)


help("setwd")
setwd("/home/pict/Documents/SL-VI/")



library(reshape2)

str(data)
first_half <- data[1:350,]
second_half <- data[350:700,]

# Put header



# Count no of records



# Datatype of every variable



# Display no of null/nonnull attributes



# Calculate average 



# Encode output labels



# 