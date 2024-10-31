##### Import Packages ####
library(rio)
library(tidyverse)
library(lme4)
library(jtools)
library(ggpubr)

#### Cleaning and basic descriptives ####
Data <- import("Data_anony.xlsx")
table(Data$Progress)
Data = filter(Data, Progress == 100)
 
Data <- Data %>%
   rename(Job = Q2,
          Career = Q11,
          Gender = Q3,
          Country = Q6,
          Region_expertise = Q7,
          Country_expertise = Q9,
          Country_2_expertise = Q10,
          Disciplinary_background = Q12,
          Expertise_background = Q13)

Data = filter(Data, Job != "I’m not a researcher")

developed_countries <- c("Australia", "Austria", "Belgium", "Canada", "Czech Republic",
                         "Denmark", "France", "Germany", "Greece", "Ireland", "Israel",
                         "Italy", "Japan", "Netherlands", "New Zealand", "Norway",
                         "Portugal", "Singapore", "Slovakia", "South Korea",
                         "Spain", "Sweden", "Switzerland", "UK", "USA",
                         "Croatia", "San Marino", "Andorra", "Latvia", "Lithuania",
                         "Estonia", "Malta", "Liechtenstein", "Monaco", "Slovenia",
                         "Cyprus", "Taiwan", "Finland", "Iceland", "Luxembourg")

Data$Country_expertise <- gsub("Ireland \\{Republic\\}", "Ireland", Data$Country_expertise)
Data$Country_expertise[Data$Country_expertise == "Korea South"] <- "South Korea"
Data$Country_expertise[Data$Country_expertise == "Taiwan"] <- "Taiwan"
Data$Country_expertise[Data$Country_expertise == "United Kingdom"] <- "UK"
Data$Country_expertise[Data$Country_expertise == "United States"] <- "USA"
Data$Country_expertise[Data$Country_expertise == "Russian Federation"] <- "Russia"
Data$Developed <- ifelse(Data$Country_expertise %in% developed_countries, 1, 0)

Data_DVP = filter(Data, Developed=="1")
Data_no_dvp = filter(Data, Developed=="0")

##### Healthy Info Env ####
Data <- Data %>%
  mutate(across(Q15_1:Q15_7, ~ recode(.,
                                      "Absolutely essential" = 6,
                                      "Extremely important" = 5,
                                      "Very important" = 4,
                                      "Moderately important" = 3,
                                      "Not very important" = 2,
                                      "Not important at all" = 1,
                                      "I don't know" = NA_real_,
                                      .default = NA_real_)))%>%
  rename("Diversity of Voices" = Q15_1,
       "Diversity of media ownership" = Q15_2,
       "Availability of accurate information" = Q15_3,
       "Absence of false or misleading information" = Q15_4,
       "Absence of hateful content" = Q15_5,
       "Absence of micro-targeted political ads" = Q15_6,
       "Absence of AI-generated content" = Q15_7)

Health_1 <- lm(`Diversity of Voices` ~ Developed, data = Data)
summ(Health_1)
Health_1 <- wilcox.test(`Diversity of Voices` ~ Developed, data = Data)
Health_1

Health_2 <- lm(`Diversity of media ownership` ~ Developed, data = Data)
summ(Health_2)
Health_2 <- wilcox.test(`Diversity of media ownership` ~ Developed, data = Data)
Health_2

Health_3 <- lm(`Availability of accurate information` ~ Developed, data = Data)
summ(Health_3)
Health_3 <- wilcox.test(`Availability of accurate information` ~ Developed, data = Data)
Health_3

Health_4 <- lm(`Absence of false or misleading information` ~ Developed, data = Data)
summ(Health_4)
Health_4 <- wilcox.test(`Absence of false or misleading information` ~ Developed, data = Data)
Health_4

Health_5 <- lm(`Absence of hateful content` ~ Developed, data = Data)
summ(Health_5)
Health_5 <- wilcox.test(`Absence of hateful content` ~ Developed, data = Data)
Health_5

Health_6 <- lm(`Absence of micro-targeted political ads` ~ Developed, data = Data)
summ(Health_6)
Health_6 <- wilcox.test(`Absence of micro-targeted political ads` ~ Developed, data = Data)
Health_6

Health_7 <- lm(`Absence of AI-generated content` ~ Developed, data = Data)
summ(Health_7)
Health_7 <- wilcox.test(`Absence of AI-generated content` ~ Developed, data = Data)
Health_7


Data <- Data %>%
  mutate(across(Q16_1:Q16_5, ~ recode(.,
                                      "Extremely" = 6,
                                      "A lot" = 5,
                                      "Moderately" = 4,
                                      "Little" = 3,
                                      "Very little" = 2,
                                      "Not at all" = 1, 
                                      "I don't know" = NA_real_,
                                      .default = NA_real_)))%>%
  rename("Fact-checking" = Q16_1,
         "Labeling AI content" = Q16_2,
         "Labeling false/misleading content" = Q16_3,
         "Labeling untrustworthy sources" = Q16_4,
         "Digital/media literacy tips" = Q16_5)

mean(Data_DVP$"Fact-checking", na.rm =T)
mean(Data_no_dvp$"Fact-checking", na.rm =T)

mean(Data_DVP$"Digital/media literacy tips", na.rm =T)
mean(Data_no_dvp$"Digital/media literacy tips", na.rm =T)

Data <- Data %>%
  mutate(across(Q17_1:Q17_5, ~ recode(.,
                                      "Strongly in favor" = 7,
                                      "In favor" = 6,
                                      "Slightly in favor" = 5,
                                      "Neither" = 4,
                                      "Slightly against" = 3,
                                      "Against" = 2,
                                      "Strongly against" = 1,.default = NA_real_)))%>%
  rename("Expanding content moderation on social media" = Q17_1,
         "Deplatforming problematic actors" = Q17_2,
         "Adopting digital/media literacy campaigns and educational programs" = Q17_3,
         "Supporting free and independent media" = Q17_4,
         "Treat digital platforms as publishers rather than distributors" = Q17_5)

table(Data$Q18_1)
Data <- Data %>%
  mutate(across(Q18_1:Q18_8, ~ recode(.,
                                      "The biggest threat" = 7,
                                      "Very big" = 6,
                                      "Big threat" = 5,
                                      "Moderate threat" = 4,
                                      "Small threat" = 3,
                                      "Very small threat" = 2,
                                      "Not a threat at All" = 1,
                                      "I don't know" = NA_real_,
                                      .default = NA_real_)))%>%
  rename("Government, politicians/parties in my country" =  Q18_1,
        "Ordinary citizens" =     Q18_2,
           "Activists" = Q18_3,
             "Foreing government, politicians/parties" = Q18_4, 
             "Private news organizations" = Q18_5,
             "State-backed news organizations" = Q18_6,
             "Journalists or news organizations" = Q18_7,
             "Owners of social media platforms" = Q18_8)

mean(Data_DVP$"Owners of social media platforms", na.rm =T)
mean(Data_no_dvp$"Owners of social media platforms", na.rm =T)


mean(Data_DVP$"Government, politicians/parties in my country", na.rm =T)
mean(Data_no_dvp$"Government, politicians/parties in my country", na.rm =T)

table(Data$"Ordinary citizens")

Data <- Data %>%
  mutate(across(Q19_1:Q19_12, ~ recode(.,
                                       "The biggest threat" = 7,
                                       "Very big" = 6,
                                       "Big threat" = 5,
                                       "Moderate threat" = 4,
                                       "Small threat" = 3,
                                       "Very small threat" = 2,
                                       "Not a threat at All" = 1,
                                       "I don't know" = NA_real_,
                                      .default = NA_real_)))%>%
  rename(
    "News websites" = Q19_1,
    "Legacy media" = Q19_2,
    "Search engines" = Q19_3,
    "Podcast platforms" = Q19_4,
    "Wikis" = Q19_5,
    "Video platforms" = Q19_6,
    "Messaging apps" = Q19_7,
    "Discussion forums" = Q19_8,
    "Microblogging platforms" = Q19_9,
    "Professional networking sites" = Q19_10,
    "Social networking platforms" = Q19_11,
    "Generative AI tools" = Q19_12)

mean(Data_DVP$"Messaging apps", na.rm =T)
mean(Data_no_dvp$"Messaging apps", na.rm =T)

mean(Data_DVP$"Social networking platforms", na.rm =T)
mean(Data_no_dvp$"Social networking platforms", na.rm =T)

Data <- Data %>%
  mutate(across(Q20_1:Q20_4, ~ recode(.,
                                       "Greatly improve" = 7,
                                       "Somewhat improve" = 6,
                                       "Slightly improve" = 5,
                                       "Neither" = 4,
                                       "Slightly worsen" = 3,
                                       "Somewhat worsen" = 2,
                                       "Greatly worsen" = 1,
                                      "I don't know" = NA_real_,
                                      .default = NA_real_)))%>%
  rename(
    "PAST_AI-generated text" = Q20_1,
    "PAST_AI-generated images" = Q20_2,
    "PAST_AI-generated voices" = Q20_3,
    "PAST_AI-generated videos" = Q20_4)

Data <- Data %>%
  mutate(across(Q21_1:Q21_4, ~ recode(.,
                                      "Greatly improve" = 7,
                                      "Somewhat improve" = 6,
                                      "Slightly improve" = 5,
                                      "Neither" = 4,
                                      "Slightly worsen" = 3,
                                      "Somewhat worsen" = 2,
                                      "Greatly worsen" = 1,
                                      "I don't know" = NA_real_,
                                      .default = NA_real_)))%>%
  rename(
    "FUTURE_AI-generated text" = Q21_1,
    "FUTURE_AI-generated images" = Q21_2,
    "FUTURE_AI-generated voices" = Q21_3,
    "FUTURE_AI-generated videos" = Q21_4)

mean(Data_DVP$"FUTURE_AI-generated videos", na.rm=T)
mean(Data_DVP$"FUTURE_AI-generated text", na.rm=T)
mean(Data_DVP$"FUTURE_AI-generated voices", na.rm=T)
mean(Data_DVP$"FUTURE_AI-generated images", na.rm=T)
(2.634454+3.031111+2.855319+2.876543)/4

mean(Data_DVP$"PAST_AI-generated videos", na.rm=T)
mean(Data_DVP$"PAST_AI-generated text", na.rm=T)
mean(Data_DVP$"PAST_AI-generated voices", na.rm=T)
mean(Data_DVP$"PAST_AI-generated images", na.rm=T)
(2.904306+3.382653+2.969388+3.066351)/4

(2.634454+3.031111+2.855319+2.876543+2.904306+3.382653+2.969388+3.066351)/8
8-2.965016

mean(Data_no_dvp$"FUTURE_AI-generated videos", na.rm=T)
mean(Data_no_dvp$"FUTURE_AI-generated text", na.rm=T)
mean(Data_no_dvp$"FUTURE_AI-generated voices", na.rm=T)
mean(Data_no_dvp$"FUTURE_AI-generated images", na.rm=T)
(2.261194+2.756522+2.371212+2.566929)/4

mean(Data_no_dvp$"PAST_AI-generated videos", na.rm=T)
mean(Data_no_dvp$"PAST_AI-generated text", na.rm=T)
mean(Data_no_dvp$"PAST_AI-generated voices", na.rm=T)
mean(Data_no_dvp$"PAST_AI-generated images", na.rm=T)
(2.466102+3.23301+2.53271+2.87931)/4

(2.261194+2.756522+2.371212+2.566929+2.466102+3.23301+2.53271+2.87931)/8
8-2.633374


table(Data$Q23_1)
Data <- Data %>%
  mutate(across(Q22_1:Q22_5, ~ recode(.,
                                      "Extremely concerned" = 5,
                                      "Very concerned" = 4,
                                      "Moderately concerned" = 3,
                                      "A little concerned" = 2,
                                      "Not at all concerned" = 1,
                                      "I don't know" = NA_real_,
                                      .default = NA_real_)))%>%
  rename(
    "Increase misinfo quantity" = Q22_1,
    "Increase misinfo persuasiveness" = Q22_2,
    "Facilitate misinfo personalization" = Q22_3,
    "Perpetuate or amplify biases and stereotypes" = Q22_4,
    "Amplify harassment and abuse" = Q22_5)

Data <- Data %>%
  mutate(across(Q23_1:Q23_5, ~ recode(.,
                                      "Extremely hopeful" = 5,
                                      "Very hopeful" = 4,
                                      "Moderately hopeful" = 3,
                                      "A little hopeful" = 2,
                                      "Not at all hopeful" = 1,
                                      "I don't know" = NA_real_,
                                      .default = NA_real_)))%>%
  rename(
    "Improve_automatic_content_detection" = Q23_1,
    "Facilitate_the_work_of_journalists" = Q23_2,
    "Facilitate_cross_cultural_and_cross_modal_communication" = Q23_3,
    "Increase_persuasiveness_of_reliable_information" = Q23_4,
    "Facilitate_personalization_of_reliable_information" = Q23_5)

Data <- Data %>%
  mutate(across(Q25, ~ recode(.,
                              "Very confident it will worsen" = 5,
                              "Moderately confident it will worsen" = 4,
                              "Neither" = 3,
                              "Moderately confident it will improve" = 2,
                              "Very confident it will improve" = 1,
                              .default = NA_real_)))%>%
  rename("Prediction(Neg)" = Q25)

table(Data$Q27_1)
Data <- Data %>%
  mutate(across(Q27_1:Q27_5, ~ recode(.,
                                      "Absolutely essential" = 6,
                                      "Extremely important" = 5,
                                      "Very important" = 4,
                                      "Moderately important" = 3,
                                      "Not very important" = 2,
                                      "Not at all important" = 1,
                                      "I don't know" = NA_real_,
                                      .default = NA_real_)))%>%
  rename(
    "Collect_more_data_outside_of_the_US_and_Western_democracies" = Q27_1,
    "Do_more_interdisciplinary_work" = Q27_2,
    "Mitigate_the_risks_posed_by_gen_AI" = Q27_3,
    "Move_away_from_social_media_to_study_messaging_apps" = Q27_4,
    "Do_more_cross_platforms_work" = Q27_5)

#write.csv(Data, "Data_stats_jamovi.csv")



