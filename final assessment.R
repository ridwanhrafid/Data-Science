library(dplyr)
library(ggplot2)
library(e1071)

stroke_data <- read.csv("R:/New folder/AIUB/New folder (4)/healthcare-dataset-strokedata.csv")

stroke_data$bmi <- suppressWarnings(as.numeric(as.character(stroke_data$bmi)))

stroke_data$id <- NULL

for (col in names(stroke_data)) {
  if (is.numeric(stroke_data[[col]])) {
    stroke_data[[col]][is.na(stroke_data[[col]])] <- mean(stroke_data[[col]], na.rm = TRUE)
  }
}

stroke_data$stroke_label <- factor(stroke_data$stroke, levels = 0:1, labels = c("No Stroke", "Stroke"))

print(summary(stroke_data))
str(stroke_data)

no_of_row <- nrow(stroke_data)
no_of_col <- ncol(stroke_data)
cat("No of rows in the dataset:", no_of_row, "\n")
cat("No of columns in the dataset:", no_of_col, "\n")




hist(stroke_data$age, main = "Histogram of Age", xlab = "Age", col = "blue")
hist(stroke_data$avg_glucose_level, main = "Histogram of Avg Glucose Level", xlab = "Avg Glucose Level", col = "blue")
hist(stroke_data$bmi, main = "Histogram of BMI", xlab = "BMI", col = "blue")



ggplot(stroke_data, aes(x = age)) +
  geom_density(fill = "lightblue") +
  labs(title = "Density Plot of Age", x = "Age", y = "Density")

ggplot(stroke_data, aes(x = avg_glucose_level)) +
  geom_density(fill = "lightgreen") +
  labs(title = "Density Plot of Avg Glucose Level", x = "Avg Glucose Level", y = "Density")

ggplot(stroke_data, aes(x = bmi)) +
  geom_density(fill = "lightpink") +
  labs(title = "Density Plot of BMI", x = "BMI", y = "Density")





ggplot(stroke_data, aes(x = gender)) +
  geom_bar(fill = "purple") +
  labs(title = "Bar Plot of Gender", x = "Gender", y = "Count")

ggplot(stroke_data, aes(x = work_type)) +
  geom_bar(fill = "orange") +
  labs(title = "Bar Plot of Work Type", x = "Work Type", y = "Count")

ggplot(stroke_data, aes(x = smoking_status)) +
  geom_bar(fill = "cyan4") +
  labs(title = "Bar Plot of Smoking Status", x = "Smoking Status", y = "Count")




ggplot(stroke_data, aes(y = age)) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(title = "Boxplot of Age", y = "Age")

ggplot(stroke_data, aes(y = avg_glucose_level)) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(title = "Boxplot of Avg Glucose Level", y = "Avg Glucose Level")

ggplot(stroke_data, aes(y = bmi)) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(title = "Boxplot of BMI", y = "BMI")





cat("Skewness of Age: ", round(skewness(stroke_data$age), 3), "\n")
cat("Skewness of BMI: ", round(skewness(stroke_data$bmi), 3), "\n")
cat("Skewness of Avg Glucose Level: ", round(skewness(stroke_data$avg_glucose_level), 3), "\n")





ggplot(stroke_data, aes(x = age, y = avg_glucose_level)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Age vs Avg Glucose Level", x = "Age", y = "Avg Glucose Level")

ggplot(stroke_data, aes(x = age, y = bmi)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot of Age vs BMI", x = "Age", y = "BMI")






ggplot(stroke_data, aes(x = stroke_label, y = age, fill = stroke_label)) +
  geom_violin() +
  labs(title = "Violin Plot of Age by Stroke", x = "Stroke", y = "Age")

ggplot(stroke_data, aes(x = stroke_label, y = avg_glucose_level, fill = stroke_label)) +
  geom_violin() +
  labs(title = "Violin Plot of Glucose Level by Stroke", x = "Stroke", y = "Avg Glucose Level")

ggplot(stroke_data, aes(x = stroke_label, y = bmi, fill = stroke_label)) +
  geom_violin() +
  labs(title = "Violin Plot of BMI by Stroke", x = "Stroke", y = "BMI")
