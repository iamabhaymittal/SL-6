

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
summary(new_data)
# thus -200s are replaced by the mean value of other cells of the column CO.GT
boxplot(new_data)



cor(new_data)
typeof(new_data)
str(new_data)
rem_cols <- c("Date","Time")

cor_data <- new_data[,!(names(new_data) %in% rem_cols)]
summary(cor_data)

cor_data_na <- cor_data[!is.na.data.frame(cor_data)] 

summary(cor_data_na)
corrplot(cor(cor_data,method = c("pearson", "kendall", "spearman")))
cor_data
