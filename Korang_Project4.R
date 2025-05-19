#Abena Benewaa Korang #May 5th #ALY6000 #Intro to Analytics

library(tidyverse)

student_data <- read_csv("student_habits_performance.csv")

glimpse(student_data)

#checking for missing values in each column
colSums(is.na(student_data))

#view the structure of the dataset to check variable types
str(student_data)

#convert categorical character columns to factors
student_data <- student_data %>%
  mutate(
    gender = as.factor(gender),
    part_time_job = as.factor(part_time_job),
    diet_quality = as.factor(diet_quality),
    parental_education_level = as.factor(parental_education_level),
    internet_quality = as.factor(internet_quality),
    extracurricular_participation = as.factor(extracurricular_participation)
  )

summary(student_data)

#histogram of exam scores
ggplot(student_data, aes(x = exam_score)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Exam Scores", x = "Exam Score", y = "Frequency")

#select numeric variables
numeric_data <- student_data %>%
  select(where(is.numeric))

#correlation matrix
cor_matrix <- cor(numeric_data)

#visualize correlation
library(corrplot)
corrplot::corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black")

#group attendance into categories
student_data$attendance_group <- cut(student_data$attendance_percentage,
                                     breaks = c(0, 60, 80, 100),
                                     labels = c("Low", "Medium", "High"))

#boxplot:attendance categories
ggplot(student_data, aes(x = attendance_group, y = exam_score, fill = attendance_group)) +
  geom_boxplot() +
  labs(title = "Exam Scores by Attendance Group",
       x = "Attendance Group",
       y = "Exam Score") +
  theme_minimal()


#filter students who scored below 70
low_scores <- student_data %>% filter(exam_score < 70)

#view first few
head(low_scores)

#filter students who scored above 70
high_scores <- student_data %>% filter(exam_score < 70)

#summary of students with low scores
summary(low_scores)

#summary of students with high scores
summary(high_scores)

#create a new column for score category
student_data <- student_data %>%
  mutate(score_group = ifelse(exam_score < 70, "Low", "High"))

#boxplot:study hours by score group
ggplot(student_data, aes(x = score_group, y = study_hours_per_day, fill = score_group)) +
  geom_boxplot() +
  labs(title = "Study Hours: Low vs High Scorers", x = "Score Group", y = "Study Hours per Day")

#create sleep groups
student_data$sleep_group <- cut(student_data$sleep_hours,
                             breaks = c(0, 4, 6, 8, 10),
                             labels = c("<4", "4–6", "6–8", "8–10"))

#boxplot:sleep vs exam Score
ggplot(student_data, aes(x = sleep_group, y = exam_score, fill = sleep_group)) +
  geom_boxplot() +
  labs(title = "Exam Score by Sleep Duration",
       x = "Sleep Group (Hours per Night)",
       y = "Exam Score") +
  theme_minimal()

#create social media usage groups
student_data$social_group <- cut(student_data$social_media_hours,
                             breaks = c(0, 2, 4, 6, 8),
                             labels = c("0–2", "2–4", "4–6", "6–8"))

#boxplot:social media hours vs exam score
ggplot(student_data, aes(x = social_group, y = exam_score, fill = social_group)) +
  geom_boxplot() +
  labs(title = "Exam Score by Social Media Use",
       x = "Social Media Hours per Day",
       y = "Exam Score") +
  theme_minimal()

#scatter plot for mental health vs exam score
ggplot(student_data, aes(x = mental_health_rating, y = exam_score)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "Mental Health Rating vs Exam Score", x = "Mental Health Rating", y = "Exam Score")

#boxplot:exam score by part-time job
ggplot(student_data, aes(x = part_time_job, y = exam_score, fill = part_time_job)) +
  geom_boxplot() +
  labs(title = "Exam Score by Part-Time Job", x = "Part-Time Job", y = "Exam Score")

#boxplot:exam score by gender
ggplot(student_data, aes(x = gender, y = exam_score, fill = gender)) +
  geom_boxplot() +
  labs(title = "Exam Score by Gender", x = "Gender", y = "Exam Score")






