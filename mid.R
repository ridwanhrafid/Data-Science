library(conflicted)
library(tidyverse)
library(ggplot2)
library(naniar)
library(mice)
library(tidyr)
library(dplyr)

dataframe <- read.csv("G:/R/Dataset.csv",header = TRUE, sep=",")
options(max.print=2000)
dataframe
view(dataframe)
str(dataframe)
summary(dataframe)
md.pattern(dataframe)

dataframe$gender <- factor(dataframe$gender,levels = c("male","female"),labels = c(1,0))
dataframe$glucose <- factor(dataframe$glucose,levels = c("High","Low"),labels = c(1,0))
dataframe$class <- factor(dataframe$class,levels = c("positive","negative"),labels = c(1,0))
str(dataframe)
summary(dataframe)
colSums(is.na(dataframe))
md.pattern(dataframe)

dataframe <- dataframe %>% drop_na(gender,glucose)
md.pattern(dataframe)
dataframe$age[is.na(dataframe$age)] <- ceiling(mean(dataframe$age, na.rm = TRUE))
dataframe$pressurehight[is.na(dataframe$pressurehight)] <- ceiling(mean(dataframe$pressurehight, na.rm = TRUE))
md.pattern(dataframe)

boxplot(dataframe$age, main = "Age")
boxplot(dataframe$impluse, main = "Impluse")
boxplot(dataframe$pressurehight, main = "Pressure High")
boxplot(dataframe$pressurelow, main = "Pressure Low")

dataframe$age[dataframe$age >110] <- ceiling(median(dataframe$age))
dataframe$impluse[dataframe$impluse > 200] <- ceiling(median(dataframe$impluse))
dataframe$pressurehight[dataframe$pressurehight > 250 | dataframe$pressurehight < 0] <- ceiling(median(dataframe$pressurehight))
dataframe$pressurelow[dataframe$pressurelow < 20] <- ceiling(median(dataframe$pressurelow))

dataframe <- dataframe [!duplicated(dataframe), ]

mode_function <- function(x) names(sort(table(x), decreasing=TRUE))[1]
aggregate(age ~ gender, data=dataframe, FUN = function(x) c(mean=mean(x), median=median(x), mode=mode_function(x)))
summary(dataframe)

aggregate(age ~ glucose, data=dataframe, FUN = function(x) c(mean=mean(x), median=median(x), mode=mode_function(x)))
summary(dataframe)

table(dataframe$class)
minority <- dataframe[dataframe$class == 0, ]  
majority <- dataframe[dataframe$class == 1, ]  
oversampled_minority <- minority[rep(1:nrow(minority), length.out = 88), ]
balanced_data <- rbind(majority, oversampled_minority)
balanced_data <- balanced_data[sample(nrow(balanced_data)), ]
table(balanced_data$class)

age_min <- aggregate(age ~ gender, dataframe, min, na.rm = TRUE)
age_max <- aggregate(age ~ gender, dataframe, max, na.rm = TRUE)
age_range <- data.frame(gender = age_min$gender, Range = age_max$age - age_min$age)


age_iqr <- aggregate(age ~ gender, dataframe, IQR, na.rm = TRUE)


age_var <- aggregate(age ~ gender, dataframe, var, na.rm = TRUE)


age_sd <- aggregate(age ~ gender, dataframe, sd, na.rm = TRUE)


summary_stats <- merge(age_range, age_iqr, by = "gender")
colnames(summary_stats)[2:3] <- c("Range", "IQR")

summary_stats <- merge(summary_stats, age_var, by = "gender")
colnames(summary_stats)[4] <- "Variance"

summary_stats <- merge(summary_stats, age_sd, by = "gender")
colnames(summary_stats)[5] <- "Std_Dev"


boxplot(age ~ gender, data = dataframe,
        main = "Age Distribution by Gender",
        xlab = "Gender",
        ylab = "Age",
        col = c("skyblue", "lightgreen"),
        border = "darkblue")

print(summary_stats)

set.seed(1)
split <- sample(1:nrow(dataframe), 0.7 * nrow(dataframe))
train_data <- dataframe[split, ]
test_data <- dataframe[-split, ]
nrow(dataframe)       
nrow(train_data)       
nrow(test_data)

write.csv(dataframe,"G:/R/UPDATED_Dataset.csv")

dataframe$age <- scale(dataframe$age)
dataframe$impluse <- scale((dataframe$impluse))
dataframe$pressurehight <- scale(dataframe$pressurehight)
dataframe$pressurelow <- scale(dataframe$pressurelow)
dataframe

