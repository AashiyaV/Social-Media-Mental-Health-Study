model_pdata <- lm(depressed_q ~  age + gender + relationship_status + sm_avgtime + sm_q1 + sm_q2 + sm_q3 + validation_q + activites_q + sleep_q +
                    distracted_q + bothered_q + concentrate_q + selfcompare_q, data = pdata)

# Summary of the model
summary(model_pdata)

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

model_pdata2 = lm(Depression_Score ~ Age + CGPA + Diet_Quality + Sleep_Quality + Relationship_Status + Semester_Credit_Load + Gender + Physical_Activity + 
      Financial_Stress + Stress_Level + Anxiety_Score, data = pdata2)


summary(model_pdata2)




