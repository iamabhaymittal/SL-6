

# new dataset where commas are replaced with decimal point
decimal_data <- read.csv("/home/pict/SL-6/Rprograms/airquality-master/AirQualityUCI_new.csv", sep = ';', header = T)
str(decimal_data)

drop_cols <- c("X","X.1")

#deleting extra columns
new_data <- decimal_data[ , !(names(main_data) %in% drop_cols)]

head(new_data)
str(new_data)
boxplot(new_data$CO.GT.)

typeof(new_data$Date)


# finding -200 values
length(which(new_data$CO.GT. == -200))
outlier_indices <- which(new_data$CO.GT. == -200)

non_outliers <- which(new_data$CO.GT. != -200)
avg_cogt <- mean(new_data$CO.GT.[non_outliers])

new_data$CO.GT.[outlier_indices] <- avg_cogt

#thus -200s are replaced by the mean value of other cells of the column CO.GT
boxplot(new_data$CO.GT.)

new_data
# exclude variables v1, v2, v3
# myvars <- names(mydata) %in% c("v1", "v2", "v3")
# newdata <- mydata[!myvars]