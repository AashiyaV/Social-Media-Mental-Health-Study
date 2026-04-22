# Load required packages if not already loaded
library(MASS)

# Fit the ordinal logistic regression model
model1 <- polr(depressed_q ~ age + gender + relationship_status + sm_avgtime + sm_q1 + sm_q2 + sm_q3 + validation_q + activites_q + sleep_q +
                 selfcompare_q + distracted_q + bothered_q + concentrate_q, data = pdata, Hess = TRUE)

# Plot boxplot for age relative to depressed_q
boxplot(age ~ depressed_q, data = pdata, xlab = "Depression Score", ylab = "Age", main = "Age vs. Depression Score")

# Plot boxplot for sm_avgtime relative to depressed_q
# Define custom labels for the x-axis
custom_labels <- c("Less than 1 hour", "Between 1 and 2 hours", "Between 2 and 3 hours", 
                   "Between 3 and 4 hours", "Between 4 and 5 hours", "More than 5 hours")

# Plot boxplot with custom x-axis labels
boxplot(depressed_q ~ sm_avgtime, data = pdata, 
        xlab = "Average Social Media Time", 
        ylab = "Depression Score", 
        main = "Average Social Media Time vs. Depression Score",
        names = custom_labels)


# Convert depressed_q into a factor
pdata$depressed_q <- factor(pdata$depressed_q)

# Plot bar plot for sm_avgtime relative to depressed_q for each relationship status
ggplot(pdata, aes(x = sm_avgtime, fill = depressed_q)) +
  geom_bar(position = "dodge", color = "black") +
  facet_wrap(~relationship_status) +
  labs(title = "Average Social Media Time vs. Depression Score by Relationship Status",
       x = "Average Social Media Time",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
