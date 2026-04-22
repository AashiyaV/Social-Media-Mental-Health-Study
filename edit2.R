library(ggplot2)

ggplot(pdata2, aes(x = Stress_Level, y = Depression_Score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  facet_wrap(~ Sleep_Quality)  

ggplot(pdata2, aes(x = Anxiety_Score, y = Depression_Score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") 

library(ggplot2)
library(gridExtra)

# Example variables to plot against Price
variables <- c( "Age" , "CGPA" , "Diet_Quality" , "Sleep_Quality" , "Relationship_Status" , "Semester_Credit_Load" , 
                 "Gender" , "Physical_Activity" , "Financial_Stress", "Stress_Level", "Anxiety_Score")
# Empty list to store plots
plots <- list()

# Loop through variables and create regression plots
for (var in variables) {
  p <- ggplot(pdata2, aes_string(x = var, y = "Depression_Score")) +
    geom_point() +  # Add the points
    geom_smooth(method = "lm", color = "blue") +  # Add regression line
    labs(title = paste("Regression of Depression_Score on", var), x = var, y = "Depression Score")
  plots[[var]] <- p
}

# Assuming you want to view the plots or save them to a list
for (var in variables) {
  print(plots[[var]])  # Print the plot to the R graphics device
}

ggplot(pdata2, aes(x=Stress_Level, y=Depression_Score)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  theme_minimal() +
  #facet_wrap( ~ Physical_Activity) +
  ggtitle("Scatter Plot with Regression Line for Age")

ggplot(pdata2, aes(x=Gender, y=Depression_Score)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Box Plot for Gender") +
  facet_wrap( ~ Sleep_Quality)


########################################

# Load necessary libraries
library(car)

# Assuming pdata2 is already loaded. If not, load your data.
# pdata2 <- read.csv("path/to/your/pdata2.csv")

# Ensure categorical variables are factored
pdata2$Gender <- as.factor(pdata2$Gender)
pdata2$Diet_Quality <- as.factor(pdata2$Diet_Quality)
pdata2$Sleep_Quality <- as.factor(pdata2$Sleep_Quality)
pdata2$Relationship_Status <- as.factor(pdata2$Relationship_Status)
pdata2$Physical_Activity <- as.factor(pdata2$Physical_Activity)
# Add other variables as necessary

# Fit the linear model
model_pdata2 <- lm(Depression_Score ~ Age + CGPA + Diet_Quality + Sleep_Quality + 
                     Relationship_Status + Semester_Credit_Load + Gender + Physical_Activity + 
                     Financial_Stress + Stress_Level + Anxiety_Score +factor(Course), data = pdata2)

crPlots(model_pdata2)
avPlots(model_pdata2)



