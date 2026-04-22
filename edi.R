library(ggplot2)

ggplot(pdata, aes(x = sm_q1, y = depressed_q)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  facet_wrap(~ sm_avgtime)  

ggplot(pdata, aes(x = sm_q2, y = depressed_q)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  facet_wrap(~ sm_avgtime)  

ggplot(pdata, aes(x = sm_q3, y = depressed_q)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  facet_wrap(~ sm_avgtime)  

library(ggplot2)
library(tidyr)

# Assuming 'data' is your original dataframe
# Reshape the data from wide to long
long_data <- pdata %>%
  pivot_longer(
    cols = c(sm_q1, sm_q2, sm_q3),
    names_to = "sm_question",
    values_to = "sm_value"
  )

# Create a ggplot object with color to distinguish between sm_q1, sm_q2, and sm_q3
ggplot(long_data, aes(x = sm_value, y = depressed_q, color = sm_question)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Social Media Question Response",
       y = "Frequency of Feeling Depressed",
       color = "Social Media Question") +
  theme_minimal()


ggplot(pdata, aes(x = validation_q, y = depressed_q)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") 


library(ggplot2)
library(gridExtra)

# Continuous variables to plot against depressed_q
variables <- c("age", "sm_avgtime", "sm_q1", "sm_q2", "sm_q3", 
               "validation_q", "activites_q", "sleep_q", 
               "bothered_q", "concentrate_q", "selfcompare_q", "compare_q")

# Empty list to store plots
plots <- list()

# Loop through variables and create regression plots
for (var in variables) {
  p <- ggplot(pdata, aes_string(x = var, y = "depressed_q")) +
    geom_point() +  # Add the points
    geom_smooth(method = "lm", color = "blue") +  # Add regression line
    labs(title = paste("Regression of depressed_q on", var), x = var, y = "depressed_q")
  plots[[var]] <- p
}
print(plots)
# Combine the plots into a single plot object
combined_plot <- do.call(grid.arrange, c(plots, ncol = 3))

# Optionally, you can save the combined plot to a file
#ggsave("combined_regression_plots.pdf", combined_plot)


for (var in variables) {
  p <- ggplot(pdata, aes_string(x = var, y = "depressed_q")) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue")   +
    facet_wrap(~ sm_avgtime)   # Faceting by sm_avgtime
    labs(title = paste("Relationship between", var, "and depressed_q, faceted by sm_avgtime"), 
         x = var, 
         y = "depressed_q")
  
  # Print the plot
  print(p)
}

library(ggplot2)

# Create a scatter plot of compare_q vs selfcompare_q
ggplot(pdata, aes(x = selfcompare_q, y = compare_q)) +
  geom_point() +  # add the scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # add a regression line with standard error
  labs(
    title = "Relationship between selfcompare_q and compare_q",
    x = "Self Comparison (selfcompare_q)",
    y = "Comparison (compare_q)"
  ) + 
  theme_minimal()  # optional: set a minimal theme for a clean look


install.packages("car")
library(car)

crPlots(model_pdata)
avPlots(model_pdata)
