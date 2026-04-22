library(ggplot2)

pdata = read.csv('smmh.csv')
attach(pdata)
names(pdata)

pdata = na.omit(pdata)

colnames(pdata) <- c("Timestamp" = "timestamp", "X1..What.is.your.age." = "age", "X2..Gender" = "gender",
               "X3..Relationship.Status"   = "relationship_status", 
               "X4..Occupation.Status" = "occupation_status",
               "X5..What.type.of.organizations.are.you.affiliated.with." = "organization", 
               "X6..Do.you.use.social.media." = "socialmedia_use",
               "X7..What.social.media.platforms.do.you.commonly.use."  = "sm_platform", 
               "X8..What.is.the.average.time.you.spend.on.social.media.every.day." = "sm_avgtime",
               "X9..How.often.do.you.find.yourself.using.Social.media.without.a.specific.purpose." = "sm_q1", 
               "X10..How.often.do.you.get.distracted.by.Social.media.when.you.are.busy.doing.something." = "sm_q2",
               "X11..Do.you.feel.restless.if.you.haven.t.used.Social.media.in.a.while." = "sm_q3",
               "X12..On.a.scale.of.1.to.5..how.easily.distracted.are.you." = "distracted_q",
               "X13..On.a.scale.of.1.to.5..how.much.are.you.bothered.by.worries." = "bothered_q",
               "X14..Do.you.find.it.difficult.to.concentrate.on.things." = "concentrate_q",
               "X15..On.a.scale.of.1.5..how.often.do.you.compare.yourself.to.other.successful.people.through.the.use.of.social.media." = "selfcompare_q",
               "X16..Following.the.previous.question..how.do.you.feel.about.these.comparisons..generally.speaking." = "compare_q",
               "X17..How.often.do.you.look.to.seek.validation.from.features.of.social.media." = "validation_q",
               "X18..How.often.do.you.feel.depressed.or.down." = "depressed_q", 
               "X19..On.a.scale.of.1.to.5..how.frequently.does.your.interest.in.daily.activities.fluctuate." = "activites_q",
               "X20..On.a.scale.of.1.to.5..how.often.do.you.face.issues.regarding.sleep." = "sleep_q")

pdata =  pdata[pdata$age <= 35, ]
pdata =  pdata[pdata$age >= 18, ]
pdata =  pdata[pdata$occupation_status == "University Student" | pdata$occupation_status == "School Student", ]
pdata <- pdata[pdata$relationship_status != "Divorced", ]
pdata =  pdata[, !(names(pdata) == "sm_platform")]
pdata =  pdata[, !(names(pdata) == "organization")]
pdata =  pdata[, !(names(pdata) == "timestamp")]
pdata =  pdata[, !(names(pdata) == "occupation_status")]
pdata =  pdata[, !(names(pdata) == "socialmedia_use")]
pdata <- pdata[pdata$gender != "Nonbinary ", ]
pdata <- pdata[pdata$gender != "Non-binary", ]
pdata <- pdata[pdata$gender != "Non binary ", ]

attach(pdata)
# 
# set.seed(375)
# pdata$sm_avgtime[pdata$sm_avgtime == "Less than an Hour"] <- runif(sum(pdata$sm_avgtime == "Less than an Hour"), min = 0, max = 0.5)
# pdata$sm_avgtime[pdata$sm_avgtime == "Between 1 and 2 hours"] = runif(sum(pdata$sm_avgtime == "Between 1 and 2 hours"), min = 1, max = 2)
# pdata$sm_avgtime[pdata$sm_avgtime == "Between 2 and 3 hours"] = runif(sum(pdata$sm_avgtime == "Between 2 and 3 hours"), min = 2, max = 3)
# pdata$sm_avgtime[pdata$sm_avgtime == "Between 3 and 4 hours"] = runif(sum(pdata$sm_avgtime == "Between 3 and 4 hours"), min = 3, max = 4)
# pdata$sm_avgtime[pdata$sm_avgtime == "Between 4 and 5 hours"] = runif(sum(pdata$sm_avgtime == "Between 4 and 5 hours"), min = 4, max = 5)
# pdata$sm_avgtime[pdata$sm_avgtime == "More than 5 hours"] = 5
# pdata =  pdata[, !(names(pdata) == "sm_platform")]
# pdata =  pdata[, !(names(pdata) == "organization")]
# #pdata =  pdata[, !(names(pdata) == "timestamp")]
# pdata =  pdata[, !(names(pdata) == "occupation_status")]
# pdata =  pdata[, !(names(pdata) == "socialmedia_use")]
# pdata$relationship_status[pdata$relationship_status == "Single"] = 0
# pdata$relationship_status[pdata$relationship_status == "In a relationship"] = 1
# pdata$relationship_status[pdata$relationship_status == "Married"] = 2
# pdata$relationship_status[pdata$relationship_status == "Married"] = 2
# pdata$gender[pdata$gender == "Male"] = 1
# pdata$gender[pdata$gender == "Female"] = 2
# pdata <- pdata[pdata$relationship_status != "Divorced", ]
# pdata <- pdata[pdata$gender != "Nonbinary ", ]
# pdata <- pdata[pdata$gender != "Non-binary", ]
# pdata <- pdata[pdata$gender != "Non binary ", ]
# pdata$gender <- as.numeric(pdata$gender)
# pdata$relationship_status <- as.numeric(pdata$relationship_status)
# pdata$sm_avgtime <- as.numeric(pdata$sm_avgtime)

pdata2 = read.csv('students_mental_health_survey.csv.xls')
attach(pdata2)
names(pdata2)

pdata2 = na.omit(pdata2)
pdata2 =  pdata2[, !(names(pdata2) == "Substance_Use")]
pdata2 =  pdata2[, !(names(pdata2) == "Social_Support")]

attach(pdata2)

# pdata2$Gender[pdata2$Gender == "Male"] = 1
# pdata2$Gender[pdata2$Gender == "Female"] = 2
# pdata2$Relationship_Status[pdata2$Relationship_Status == "Single"] = 0
# pdata2$Relationship_Status[pdata2$Relationship_Status == "In a Relationship"] = 1
# pdata2$Relationship_Status[pdata2$Relationship_Status == "Married"] = 2
# pdata2 =  pdata2[, !(names(pdata2) == "Substance_Use")]
# pdata2 =  pdata2[, !(names(pdata2) == "Social_Support")]
# pdata2$Counseling_Service_Use[pdata2$Counseling_Service_Use == "Never"] = 0
# pdata2$Counseling_Service_Use[pdata2$Counseling_Service_Use == "Occasionally"] = runif(sum(pdata2$Counseling_Service_Use == "Occasionally"), min = 1, max = 2.5)
# pdata2$Counseling_Service_Use[pdata2$Counseling_Service_Use == "Frequently"] = runif(sum(pdata2$Counseling_Service_Use == "Frequently"), min = 2.5, max = 5)
# pdata2$Extracurricular_Involvement[pdata2$Extracurricular_Involvement == "Low"] = runif(sum(pdata2$Extracurricular_Involvement == "Low"), min = 0, max = 1)
# pdata2$Extracurricular_Involvement[pdata2$Extracurricular_Involvement == "Moderate"] = runif(sum(pdata2$Extracurricular_Involvement == "Moderate"), min = 1, max = 3)
# pdata2$Extracurricular_Involvement[pdata2$Extracurricular_Involvement == "High"] = runif(sum(pdata2$Extracurricular_Involvement == "High"), min = 3, max = 5)
# pdata2$Family_History[pdata2$Family_History == "No"] = 0
# pdata2$Family_History[pdata2$Family_History == "Yes"] = 5
# pdata2$Chronic_Illness[pdata2$Chronic_Illness == "No"] = 0
# pdata2$Chronic_Illness[pdata2$Chronic_Illness == "Yes"] = 5
# pdata2$Physical_Activity[pdata2$Physical_Activity == "Low"] = runif(sum(pdata2$Physical_Activity == "Low"), min = 0, max = 1)
# pdata2$Physical_Activity[pdata2$Physical_Activity == "Moderate"] = runif(sum(pdata2$Physical_Activity == "Moderate"), min = 1, max = 3)
# pdata2$Physical_Activity[pdata2$Physical_Activity == "High"] = runif(sum(pdata2$Physical_Activity == "High"), min = 3, max = 5)
# pdata2$Sleep_Quality[pdata2$Sleep_Quality == "Poor"] = runif(sum(pdata2$Sleep_Quality == "Poor"), min = 0, max = 1)
# pdata2$Sleep_Quality[pdata2$Sleep_Quality == "Average"] = runif(sum(pdata2$Sleep_Quality == "Average"), min = 1, max = 3)
# pdata2$Sleep_Quality[pdata2$Sleep_Quality == "Good"] = runif(sum(pdata2$Sleep_Quality == "Good"), min = 3, max = 5)
# pdata2$Diet_Quality[pdata2$Diet_Quality == "Poor"] = runif(sum(pdata2$Diet_Quality == "Poor"), min = 0, max = 1)
# pdata2$Diet_Quality[pdata2$Diet_Quality == "Average"] = runif(sum(pdata2$Diet_Quality == "Average"), min = 1, max = 3)
# pdata2$Diet_Quality[pdata2$Diet_Quality == "Good"] = runif(sum(pdata2$Diet_Quality == "Good"), min = 3, max = 5)
# pdata2$Residence_Type[pdata2$Residence_Type == "Off-Campus"] = 0
# pdata2$Residence_Type[pdata2$Residence_Type == "On-Campus"] = 1
# pdata2$Residence_Type[pdata2$Residence_Type == "With Family"] = 2
# pdata2$Course[pdata2$Course == "Business"] = 0
# pdata2$Course[pdata2$Course == "Computer Science"] = 1
# pdata2$Course[pdata2$Course == "Engineering"] = 2
# pdata2$Course[pdata2$Course == "Law"] = 3
# pdata2$Course[pdata2$Course == "Medical"] = 4
# pdata2$Course[pdata2$Course == "Others"] = 5
# pdata2$Course <- as.numeric(pdata2$Course)
# pdata2$Gender <- as.numeric(pdata2$Gender)
# pdata2$Sleep_Quality <- as.numeric(pdata2$Sleep_Quality)
# pdata2$Physical_Activity <- as.numeric(pdata2$Physical_Activity)
# pdata2$Diet_Quality <- as.numeric(pdata2$Diet_Quality)
# pdata2$Relationship_Status <- as.numeric(pdata2$Relationship_Status)
# pdata2$Counseling_Service_Use <- as.numeric(pdata2$Counseling_Service_Use)
# pdata2$Family_History <- as.numeric(pdata2$Family_History)
# pdata2$Chronic_Illness <- as.numeric(pdata2$Chronic_Illness)
# pdata2$Extracurricular_Involvement <- as.numeric(pdata2$Extracurricular_Involvement)
# pdata2$Residence_Type <- as.numeric(pdata2$Residence_Type)
# 

# pdata$depressed_q[pdata$depressed_q == "1"] = runif(sum(pdata$depressed_q == "1"), min = 0, max = 1)
# pdata$depressed_q[pdata$depressed_q == "2"] = runif(sum(pdata$depressed_q == "2"), min = 1, max = 2)
# pdata$depressed_q[pdata$depressed_q == "3"] = runif(sum(pdata$depressed_q == "3"), min = 2, max = 3)
# pdata$depressed_q[pdata$depressed_q == "4"] = runif(sum(pdata$depressed_q == "4"), min = 3, max = 4)
# pdata$depressed_q[pdata$depressed_q == "5"] = runif(sum(pdata$depressed_q == "5"), min = 4, max = 5)



attach(pdata)
attach(pdata2)
# write.csv(pdata, file = "pdata.csv", row.names = FALSE)
# write.csv(pdata2, file = "pdata2.csv", row.names = FALSE)

