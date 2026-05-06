# Load CSV file
data <- read.csv("C:\\Users\\Bjacob\\OneDrive\\Documents\\Study\\stats-stud\\data\\ccsict\\ccsict-dataset-clean.csv")

View(data)
# Shapiro-Wilk test
set.seed(1)

sample_data <- data %>%
  group_by(program) %>%
  sample_n(15)
View(sample_data)

# Save to CSV
write.csv(sample_data, "stratified_sample.csv", row.names = FALSE)
data <- read.csv("stratified_sample.csv")
View(data)

# Pie chart for sex distribution
sex_counts <- table(data$sex)
percent <- round(100 * sex_counts / sum(sex_counts), 1)

pie(sex_counts,
    main = "Sex Distribution of Students",
    col = c("pink", "skyblue"),
    radius= 1.9,
    cex = 1.1,
    labels = paste(names(sex_counts), "\n", percent, "%"))

# Pie chart for Porgram Distribution
program_counts <- table(data$program)
percent <- round(100 * program_counts / sum(program_counts), 1)

pie(program_counts,
    main = "Program Distribution of Students",
    col = c("blue", "red"),
    radius= 1.9,
    cex = 1.1,
    labels = paste(names(program_counts), "\n", percent, "%"))

# Pie chart for Prep method Distribution
prep_counts <- table(data$preparation_mode)
percent <- round(100 * prep_counts / sum(prep_counts), 1)

pie(prep_counts,
    main = "Program Distribution of Students",
    col = c("blue", "red"),
    radius= 1.9,
    cex = 1.1,
    labels = paste(names(prep_counts), "\n", percent, "%"))

#histogram for exam score distribution
hist(data$lecture_score,
     main = "Distribution of Exam Scores",
     xlab = "Exam Scores",
     col = "lightblue",
     border = "black")

#histogram for preparedness level distribution
hist(data$preparedness_score,
     main = "Distribution of Preparedness Level",
     xlab = "Preparedness Level",
     col = "lightblue",
     border = "black")

# Shapiro-Wilk test
shapiro.test(data$lecture_score)
shapiro.test(data$preparedness_score)

# Pearson correlation test
result <- cor.test(data$preparedness_score,
                   data$lecture_score,
                   method = "pearson")

# Show result
result

# Spearman correlation test
result_spearman <- cor.test(data$preparedness_score,
                            data$lecture_score,
                            method = "spearman")

# Show result
result_spearman
