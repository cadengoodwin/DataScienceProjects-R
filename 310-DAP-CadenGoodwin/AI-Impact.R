library(ggplot2)
library(readr)

#make sure to set working directory
#load dataset
data <- read_csv("AI-Job-Impact.csv")

#convert 'AI Impact' to numeric
data$`AI Impact` <- as.numeric(sub("%", "", data$`AI Impact`)) / 100

#sort and get top 5 jobs
top_5_jobs <- head(data[order(-data$`AI Impact`),], 5)

#plot
ggplot(top_5_jobs, aes(x=`AI Impact`, y=`Job titiles`)) +
  geom_bar(stat="identity", fill="skyblue") +
  labs(title="Top 5 Jobs Most Impacted by AI", x="AI Impact", y="Job Titles") +
  theme_minimal()

#plotting boxplot for AI Impact across different domains
ggplot(data, aes(x=Domain, y=`AI Impact`, fill=Domain)) +
  geom_boxplot() +
  labs(title="Distribution of AI Impact Across Domains", x="Domain", y="AI Impact") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#scatter plot for AI Impact vs. Number of AI Models by Domain
ggplot(data, aes(x=`AI models`, y=`AI Impact`, color=Domain)) +
  geom_point(alpha=0.7) +
  labs(title="AI Impact vs. Number of AI Models by Domain", x="Number of AI Models", y="AI Impact") +
  theme(legend.position="top")


#scatter plot for relationship between number of tasks and AI impact
ggplot(data, aes(x=Tasks, y=`AI Impact`)) +
  geom_point(aes(color=Domain), alpha=0.6) +
  labs(title="Relationship Between Number of Tasks and AI Impact", x="Number of Tasks", y="AI Impact") +
  theme_minimal() +
  theme(legend.position="top")

#histogram for AI Workload Ratio
ggplot(data, aes(x=AI_Workload_Ratio)) +
  geom_histogram(aes(y=..density..), bins=20, fill="lightcoral", color="black") +
  labs(title="Distribution of AI Workload Ratio Across Jobs", x="AI Workload Ratio", y="Number of jobs") +
  theme_minimal()

#filter jobs with AI Impact greater than 70% and count by domain
ai_intensive_jobs <- subset(data, `AI Impact` > 0.7)
ai_intensive_count_by_domain <- table(ai_intensive_jobs$Domain)

#plotting the number of AI-intensive job titles by domain
ggplot(as.data.frame(ai_intensive_count_by_domain), aes(x=Var1, y=Freq, fill=Var1)) +
  geom_bar(stat="identity") +
  labs(title="Domains with Most AI-Intensive Jobs (AI Impact Over 70%)", x="Domain", y="Number of AI-Intensive Job Titles (Over 70%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#hstogram for the distribution of AI models across jobs
ggplot(data, aes(x=`AI models`)) +
  geom_histogram(aes(y=..count..), bins=30, fill="lightblue", color="black") +
  labs(title="Distribution of AI Models Across Jobs", x="Number of AI Models", y="Number of Jobs") +
  theme_minimal()

