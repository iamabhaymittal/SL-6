
myString <- "my name is ankit"
print(myString)
sample_vector <- c(10,'abc')
print(sample_vector)
list1 = list(sample_vector)
list1
M = matrix(sample_vector, nrow = 2, ncol = 1)
M
BMI <- data.frame(
  gender = c("Male","Male","Female"),
  height = c(152,171,165),
  Age = c(42,14,19)
)
BMI
myvars <- c("v1","v2","v3")
myvars


#Loading data into dataframe

fb_data <- read.csv("/home/hduser/33137/SL-6/FacebookMetrics/dataset_Facebook.csv", sep = ";", header = T)
class(fb_data)

#Printing First Five Rows

head(fb_data)
names(fb_data)

#Subsetting of data

myvars <- c("V1","V3","V6")
namevars <- c("Page.total.likes","Type","Paid")

new_data <- fb_data[myvars]
  name_new_data <- fb_data[namevars]

head(new_data)
head(name_new_data)

#Using subset() 
names(new_data)
sub_data <- subset(new_data,  V3 == 2,select = c("V1","V3","V6"))
head(sub_data)

names(name_new_data)
sub_data_name <- subset(name_new_data, Type == "Photo")
sub_data_name

