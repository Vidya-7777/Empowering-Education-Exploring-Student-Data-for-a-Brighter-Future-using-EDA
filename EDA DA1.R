# Loading the necessary libraries
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(moments)
library(dplyr)
# Loading the dataset
data <- read.csv("Student_dataset.csv")
#Finding the dimensions of the dataset
dimensions_of_dataset = dim(data)
print(dimensions_of_dataset)
#Frequency Table
print(table(data$school))  
#Counts how many students are in each school
#Mean
print(mean(data$age))  
#Calculates the average age of students
#Median
print(median(data$age))  
#Finds the middle value of the age
#Mode
mode_function <- function(x) {
  return(names(sort(table(x), decreasing = TRUE)[1]))
}
print(mode_function(data$age))  
#Finds the most frequently occurring age
#Variance
print(var(data$age))  
#Measures how the ages are spread out 
#Standard Deviation
print(sd(data$age))  
#Calculates the standard deviation of age
#IQR
print(IQR(data$age))  
#Finds the middle 50% range of ages
#Range
print(range(data$age))  
#Shows the minimum and maximum ages
#Skewness
skewness(data$G3) 
#Measures the asymmetry of the data. 
#Kurtosis
kurtosis(data$G3)  
#Measures the "tailedness" of the data.
#Barplot
barplot(table(data$school), col = rainbow(5), main = "School Distribution")  
#Creates a bar plot for school counts
#Pie Chart
pie(table(data$sex), col = c("blue", "pink"), main = "Gender Distribution")  
#Histogram
hist(data$age, col = "lightblue", main = "Age Distribution", xlab = "Age")  
#Creates a histogram for age
#Box Plot
boxplot(data$age, col = "green", main = "Age Boxplot")  
#Creates a boxplot for age
#Quantile Plot
qqnorm(data$age)  
#Creates a normal probability plot
qqline(data$age, col = "red") 
#Adds a reference line to check normality
#Stem and Leaf Plot
stem(data$age)
#organizes numerical data by splitting values into stems 
#(leading digits) and leaves (trailing digits)
#Cross Tabulation
print(table(data$sex, data$school))  
#Creates a table showing school vs gender
#Co-variance
print(cov(data$G1, data$G3))  
#Calculates covariance between first and final grades
#Correlation
print(cor(data$G1, data$G3))  
#Calculates correlation between first and final grades
#Stacked Bar Plot
barplot(table(data$school, data$sex), beside = FALSE, col = c("skyblue", "lightcoral"), 
main = "Gender vs School", xlab = "Gender", ylab = "Count", 
legend = rownames(table(data$school, data$sex)))
#visualizes two categorical variables by stacking segments within bars
#Scatter Plot
plot(data$G1, data$G3, col = "red", pch = 16, main = "G1 vs G3", xlab = "G1", ylab = "G3")  
#Creates a scatter plot for grades
#Line Chart
ggplot(data, aes(x = G1, y = G3)) + geom_line(color = "blue") + 
ggtitle("G1 vs G3 Line Chart")  
#Creates a line chart for grades
#Area Graph
ggplot(data, aes(x = G1, y = G3, fill = G3)) + geom_area(alpha = 0.5) + 
ggtitle("G1 vs G3 Area Chart")  
#Creates an area graph for grades
#Heatmap
#Selecting two numeric variables for finding the correlation
cor_matrix <- cor(data[c("G3", "absences")], use = "complete.obs")
#Converting the data into long format for heatmap
melted_cor <- melt(cor_matrix)
#Creating heatmap
ggplot(melted_cor, aes(Var1, Var2, fill = value)) + geom_tile() +
geom_text(aes(label = round(value, 2)), color = "white", size = 5) +
scale_fill_gradient2(low = "green", high = "red", mid = "yellow", midpoint = 0) +
theme_minimal() +
labs(title = "Correlation Heatmap: G3 vs Absences", fill = "Correlation")