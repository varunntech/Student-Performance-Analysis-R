
# ============================================================
# STUDENT PERFORMANCE ANALYSIS AND PREDICTION USING R
# Dataset: Student Lifestyle Dataset (2000 records)
# ============================================================


# 1. LOAD DATASET

file_path <- "C:/Users/varun/Downloads/Student-Performance-Analysis/student_lifestyle_dataset.csv"

if (!file.exists(file_path)) {
  stop("CSV file not found. Check the dataset path.")
}

students <- read.csv(file_path, stringsAsFactors = FALSE)

names(students) <- trimws(names(students))

required <- c(
  "Student_ID",
  "Study_Hours_Per_Day",
  "Extracurricular_Hours_Per_Day",
  "Sleep_Hours_Per_Day",
  "Social_Hours_Per_Day",
  "Physical_Activity_Hours_Per_Day",
  "GPA",
  "Stress_Level"
)

if (!all(required %in% names(students))) {
  print(names(students))
  stop("Dataset columns do not match the expected names.")
}

cat("DATASET LOADED SUCCESSFULLY\n")
cat("Rows:", nrow(students), "\n")
cat("Columns:", ncol(students), "\n")


# 2. EXPLORE DATA

cat("\nFIRST SIX RECORDS\n")
print(head(students))

cat("\nDATA STRUCTURE\n")
str(students)

cat("\nSUMMARY STATISTICS\n")
print(summary(students))

cat("\nMISSING VALUES\n")
print(colSums(is.na(students)))

cat("\nDUPLICATE ROWS\n")
print(sum(duplicated(students)))


# 3. GPA ANALYSIS

cat("\nGPA STATISTICS\n")
cat("Average GPA:", round(mean(students$GPA, na.rm = TRUE), 2), "\n")
cat("Median GPA:", median(students$GPA, na.rm = TRUE), "\n")
cat("Highest GPA:", max(students$GPA, na.rm = TRUE), "\n")
cat("Lowest GPA:", min(students$GPA, na.rm = TRUE), "\n")
cat("GPA standard deviation:", round(sd(students$GPA, na.rm = TRUE), 2), "\n")


# 4. PREPARE DATA FOR REGRESSION

model_data <- students[, c(
  "GPA",
  "Study_Hours_Per_Day",
  "Extracurricular_Hours_Per_Day",
  "Sleep_Hours_Per_Day",
  "Social_Hours_Per_Day",
  "Physical_Activity_Hours_Per_Day"
)]

model_data <- na.omit(model_data)

if (nrow(model_data) < 2) {
  stop("Not enough complete records to train the model.")
}


# 5. TRAINING AND TESTING SPLIT

set.seed(123)

train_index <- sample(
  seq_len(nrow(model_data)),
  size = floor(0.8 * nrow(model_data))
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat("\nTraining records:", nrow(train_data), "\n")
cat("Testing records:", nrow(test_data), "\n")


# 6. MULTIPLE LINEAR REGRESSION

# Physical activity is excluded because daily activity-hour
# variables can be linearly dependent in this dataset.

model <- lm(
  GPA ~ Study_Hours_Per_Day +
    Extracurricular_Hours_Per_Day +
    Sleep_Hours_Per_Day +
    Social_Hours_Per_Day,
  data = train_data
)

cat("\nREGRESSION MODEL SUMMARY\n")
print(summary(model))


# 7. PREDICT GPA

predictions <- as.numeric(predict(model, newdata = test_data))
actual <- test_data$GPA

results <- data.frame(
  Actual_GPA = actual,
  Predicted_GPA = round(predictions, 2)
)

cat("\nFIRST 10 PREDICTIONS\n")
print(head(results, 10))


# 8. MODEL EVALUATION

MAE <- mean(abs(actual - predictions))
RMSE <- sqrt(mean((actual - predictions)^2))

SST <- sum((actual - mean(actual))^2)
SSE <- sum((actual - predictions)^2)

R_squared <- if (SST == 0) NA_real_ else 1 - SSE / SST

cat("\nMODEL PERFORMANCE\n")
cat("MAE:", round(MAE, 3), "\n")
cat("RMSE:", round(RMSE, 3), "\n")
cat("Test R-squared:", round(R_squared, 3), "\n")


# 9. CORRELATION ANALYSIS

numeric_data <- students[, c(
  "Study_Hours_Per_Day",
  "Extracurricular_Hours_Per_Day",
  "Sleep_Hours_Per_Day",
  "Social_Hours_Per_Day",
  "Physical_Activity_Hours_Per_Day",
  "GPA"
)]

cat("\nCORRELATION MATRIX\n")
print(round(cor(numeric_data, use = "complete.obs"), 2))

cat("\nCORRELATION WITH GPA\n")
print(sort(
  cor(numeric_data, use = "complete.obs")[, "GPA"],
  decreasing = TRUE
))


# 10. STRESS LEVEL DISTRIBUTION

cat("\nSTRESS LEVEL DISTRIBUTION\n")
print(table(students$Stress_Level))


# 11. SAVE ALL GRAPHS IN ONE PDF FILE

output_file <- file.path(
  "C:/Users/varun/Downloads/Student-Performance-Analysis",
  "Student_Performance_All_Graphs.pdf"
)

pdf(output_file, width = 10, height = 7)

# Graph 1: GPA Histogram
hist(
  students$GPA,
  main = "Distribution of Student GPA",
  xlab = "GPA",
  ylab = "Number of Students",
  col = "skyblue"
)

# Graph 2: GPA Boxplot
boxplot(
  students$GPA,
  main = "Boxplot of Student GPA",
  ylab = "GPA",
  col = "lightgreen"
)

# Graph 3: Study Hours vs GPA
plot(
  students$Study_Hours_Per_Day, students$GPA,
  main = "Daily Study Hours vs GPA",
  xlab = "Study Hours Per Day",
  ylab = "GPA",
  pch = 19,
  col = "blue"
)

# Graph 4: Sleep Hours vs GPA
plot(
  students$Sleep_Hours_Per_Day, students$GPA,
  main = "Sleep Hours vs GPA",
  xlab = "Sleep Hours Per Day",
  ylab = "GPA",
  pch = 19,
  col = "darkgreen"
)

# Graph 5: GPA by Stress Level
boxplot(
  GPA ~ Stress_Level,
  data = students,
  main = "GPA by Stress Level",
  xlab = "Stress Level",
  ylab = "GPA",
  col = "orange"
)

# Graph 6: Actual vs Predicted GPA
plot(
  actual, predictions,
  main = "Actual vs Predicted GPA",
  xlab = "Actual GPA",
  ylab = "Predicted GPA",
  pch = 19,
  col = "purple"
)

abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)

# Graphs 7–10: Regression Diagnostic Plots
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

# Graph 11: Stress Level Distribution
barplot(
  table(students$Stress_Level),
  main = "Distribution of Student Stress Levels",
  xlab = "Stress Level",
  ylab = "Number of Students",
  col = "steelblue"
)

# Close PDF device to finish writing the file
dev.off()


# 12. FINAL SUMMARY

cat("\n====================================\n")
cat("PROJECT COMPLETED SUCCESSFULLY\n")
cat("====================================\n")
cat("Dataset: Student Lifestyle Dataset\n")
cat("Students:", nrow(students), "\n")
cat("Variables:", ncol(students), "\n")
cat("Average GPA:", round(mean(students$GPA, na.rm = TRUE), 2), "\n")
cat("MAE:", round(MAE, 3), "\n")
cat("RMSE:", round(RMSE, 3), "\n")
cat("Test R-squared:", round(R_squared, 3), "\n")
cat("\nAll graphs saved to:\n")
cat(output_file, "\n")
