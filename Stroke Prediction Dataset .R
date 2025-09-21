library(dplyr)
library(infotheo)

stroke_data <- read.csv("R:/New folder/AIUB/New folder (4)/healthcare-dataset-strokedata.csv")
print(summary(stroke_data))
str(stroke_data)
head(stroke_data)
View(stroke_data)

stroke_data$id <- NULL
stroke_data$bmi <- suppressWarnings(as.numeric(as.character(stroke_data$bmi)))
stroke_data$stroke <- as.numeric(as.character(stroke_data$stroke))


for (col in names(stroke_data)) {
  if (is.numeric(stroke_data[[col]])) {
    stroke_data[[col]][is.na(stroke_data[[col]])] <- mean(stroke_data[[col]], na.rm = TRUE)
  }
}





cor_age_stroke <- cor.test(stroke_data$age, stroke_data$stroke, method = "pearson")
print(paste("Age vs Stroke: r =", round(cor_age_stroke$estimate, 4), 
            "p =", round(cor_age_stroke$p.value, 4),
            ifelse(cor_age_stroke$p.value < 0.05, "→ Significant", "→ Not significant")))


cor_glucose_stroke <- cor.test(stroke_data$avg_glucose_level, stroke_data$stroke, method = "pearson")
print(paste("Avg Glucose vs Stroke: r =", round(cor_glucose_stroke$estimate, 4), 
            "p =", round(cor_glucose_stroke$p.value, 4),
            ifelse(cor_glucose_stroke$p.value < 0.05, "→ Significant", "→ Not significant")))


cor_bmi_stroke <- cor.test(stroke_data$bmi, stroke_data$stroke, method = "pearson")
print(paste("BMI vs Stroke: r =", round(cor_bmi_stroke$estimate, 4), 
            "p =", round(cor_bmi_stroke$p.value, 4),
            ifelse(cor_bmi_stroke$p.value < 0.05, "→ Significant", "→ Not significant")))






anova_gender <- aov(stroke ~ gender, data = stroke_data)
p_gender <- summary(anova_gender)[[1]]$"Pr(>F)"[1]
print(paste("Gender vs Stroke: p =", round(p_gender, 4), 
            ifelse(p_gender < 0.05, "→ Significant", "→ Not significant")))


anova_married <- aov(stroke ~ ever_married, data = stroke_data)
p_married <- summary(anova_married)[[1]]$"Pr(>F)"[1]
print(paste("Marital Status vs Stroke: p =", round(p_married, 4), 
            ifelse(p_married < 0.05, "→ Significant", "→ Not significant")))

anova_work <- aov(stroke ~ work_type, data = stroke_data)
p_work <- summary(anova_work)[[1]]$"Pr(>F)"[1]
print(paste("Work Type vs Stroke: p =", round(p_work, 4), 
            ifelse(p_work < 0.05, "→ Significant", "→ Not significant")))






chi_ht <- chisq.test(table(stroke_data$hypertension, stroke_data$stroke))
print(paste("Hypertension vs Stroke: p =", round(chi_ht$p.value, 4), 
            ifelse(chi_ht$p.value < 0.05, "→ Significant", "→ Not significant")))

chi_hd <- chisq.test(table(stroke_data$heart_disease, stroke_data$stroke))
print(paste("Heart Disease vs Stroke: p =", round(chi_hd$p.value, 4), 
            ifelse(chi_hd$p.value < 0.05, "→ Significant", "→ Not significant")))

chi_res <- chisq.test(table(stroke_data$Residence_type, stroke_data$stroke))
print(paste("Residence Type vs Stroke: p =", round(chi_res$p.value, 4), 
            ifelse(chi_res$p.value < 0.05, "→ Significant", "→ Not significant")))






stroke_discrete <- discretize(stroke_data, disc = "equalfreq", nbins = 5)

mi_age <- mutinformation(stroke_discrete$stroke, stroke_discrete$age)
print(paste("Mutual Information with Age:", round(mi_age, 4)))

mi_glucose <- mutinformation(stroke_discrete$stroke, stroke_discrete$avg_glucose_level)
print(paste("Mutual Information with Avg Glucose Level:", round(mi_glucose, 4)))

mi_bmi <- mutinformation(stroke_discrete$stroke, stroke_discrete$bmi)
print(paste("Mutual Information with BMI:", round(mi_bmi, 4)))
