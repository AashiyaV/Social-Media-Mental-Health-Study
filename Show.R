library(MASS)
library(effects)
library(coefplot)

# Assuming pdata is your data frame and depressed_q is the ordinal response variable
# Convert depressed_q to an ordered factor
pdata$depressed_q <- factor(pdata$depressed_q, ordered = TRUE)

pdata$gender <- as.factor(pdata$gender)
pdata$relationship_status <- as.factor(pdata$relationship_status)
pdata$sm_avgtime <- as.factor(pdata$sm_avgtime)


# Fit an ordinal logistic regression model
model1 <- polr(depressed_q ~ age + gender + relationship_status + sm_avgtime + sm_q1 + sm_q2 + sm_q3 + validation_q + activites_q + sleep_q +
              selfcompare_q + distracted_q + bothered_q + concentrate_q , data = pdata, Hess=TRUE)

# Summary of the model
summary(model1)

# Extracting coefficients and their standard errors
coefficients <- summary(model1)$coefficients[, 1] # Estimates
std_errors <- summary(model1)$coefficients[, 2] # Standard errors

# Calculating the t-values
t_values <- coefficients / std_errors

# Calculating p-values
p_values <- 2 * (1 - pt(abs(t_values), df=nrow(pdata) - length(coefficients)))

# Naming the p-values for clarity
names(p_values) <- names(coefficients)

# Printing the p-values
print(p_values)

# Model fitting omitted, assuming you have it in 'model'

# activites_q , selfcompare_q , sm_q2 , bothered_q 

# Plot effect of activites_q
plot_effect <- plot(Effect("activites_q", model1), 
                    xlab = "Frequency of activities", 
                    ylab = "Frequency of feeling depressed or down", 
                    main = "Effect of Activities on Depression Score")

plot_effect <- plot(Effect("sm_q2", model1), 
                    xlab = "Use social media without a specific purpose", 
                    ylab = "Frequency of feeling depressed or down", 
                    main = "Effect of Social Media Use on Depression Score")

plot_effect <- plot(Effect("selfcompare_q", model1), 
                    xlab = "Comparison of one to other successful people through social media", 
                    ylab = "Frequency of feeling depressed or down", 
                    main = "Effect of Social Media on Depression Score")

plot_effect <- plot(Effect("bothered_q", model1), 
                    xlab = "Bothered by worries", 
                    ylab = "Frequency of feeling depressed or down", 
                    main = "Effect of Worries on Depression Score")


plot_effect <- plot(Effect("validation_q", model1), 
                    xlab = "How often the respondent seeks validation from social media", 
                    ylab = "Frequency of feeling depressed or down", 
                    main = "Effect of Social Media on Depression Score")



# Plot the model coefficients
coefplot(model1)

library(ggplot2)

#################################################
# Assuming you have read your data into a dataframe called pdata2

# Convert categorical variables to factors
pdata2$Course <- as.factor(pdata2$Course)
pdata2$Gender <- as.factor(pdata2$Gender)
pdata2$Sleep_Quality <- as.factor(pdata2$Sleep_Quality)
pdata2$Physical_Activity <- as.factor(pdata2$Physical_Activity)
pdata2$Diet_Quality <- as.factor(pdata2$Diet_Quality)
pdata2$Relationship_Status <- as.factor(pdata2$Relationship_Status)
pdata2$Counseling_Service_Use <- as.factor(pdata2$Counseling_Service_Use)
pdata2$Family_History <- as.factor(pdata2$Family_History)
pdata2$Chronic_Illness <- as.factor(pdata2$Chronic_Illness)
pdata2$Extracurricular_Involvement <- as.factor(pdata2$Extracurricular_Involvement)
pdata2$Residence_Type <- as.factor(pdata2$Residence_Type)

# Make sure Depression_Score is an ordered factor
pdata2$Depression_Score <- factor(pdata2$Depression_Score, ordered = TRUE)

# Load the MASS package for ordinal logistic regression
library(MASS)

# Fit the ordinal logistic regression model
model2 <- polr(Depression_Score ~ Age + Course + Gender + CGPA + Stress_Level +
                Anxiety_Score + Sleep_Quality + Physical_Activity + Diet_Quality +
                Relationship_Status + Counseling_Service_Use  + Financial_Stress + Extracurricular_Involvement +
                Semester_Credit_Load , data = pdata2, Hess = TRUE)

# Summarize the model
summary(model2)

# Extracting coefficients and their standard errors
coefficients <- summary(model2)$coefficients[, 1] # Estimates
std_errors <- summary(model2)$coefficients[, 2] # Standard errors

# Calculating the t-values
t_values <- coefficients / std_errors

# Calculating p-values
p_values <- 2 * (1 - pt(abs(t_values), df=nrow(pdata) - length(coefficients)))

# Naming the p-values for clarity
names(p_values) <- names(coefficients)

# Printing the p-values
print(p_values)

coefplot(model2)
# CourseComputer Science  , CGPA , Diet_QualityGood , Semester_Credit_Load  

plot_effect <- plot(Effect("CGPA", model2), 
                    xlab = "GPA", 
                    ylab = "Level of depression", 
                    main = "Effect of GPA on Depression Score")


plot_effect <- plot(Effect("Semester_Credit_Load", model2), 
                    xlab = "Semester Credit Load", 
                    ylab = "Level of depression", 
                    main = "Effect of Semester Credit Load on Depression Score")

plot_effect <- plot(Effect("Anxiety_Score", model2), 
                    xlab = "Anxiety Score", 
                    ylab = "Level of depression", 
                    main = "Effect of Anxiety Score on Depression Score")

plot_effect <- plot(Effect("Stress_Level", model2), 
                    xlab = "Stress Level", 
                    ylab = "Level of depression", 
                    main = "Effect of Stress Level on Depression Score")

