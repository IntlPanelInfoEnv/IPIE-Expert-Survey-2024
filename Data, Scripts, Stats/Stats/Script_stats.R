##### Import Packages ####
library(rio)
library(tidyverse)
library(lme4)
library(jtools)
library(ggpubr)

# This script is mostly to transform the variables in numerical variables to do stats

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

##### transforming into numericals ####
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

mean(Data$"Availability of accurate information", na.rm =T)
mean(Data$"Diversity of Voices", na.rm =T)
mean(Data$"Diversity of media ownership", na.rm =T)
(5.481663+5.141119+4.921376)/3

mean(Data$"Absence of false or misleading information", na.rm =T)
mean(Data$"Absence of hateful content", na.rm =T)
(4.613692+4.611247)/2

mean(Data$"Absence of micro-targeted political ads", na.rm =T)
mean(Data$"Absence of AI-generated content", na.rm =T)
(3.716792+3.191436)/2

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

mean(Data$"Supporting free and independent media", na.rm = TRUE)

mean(Data$"Adopting digital/media literacy campaigns and educational programs", na.rm = TRUE)

mean(Data$"Deplatforming problematic actors", na.rm = TRUE)
mean(Data$"Treat digital platforms as publishers rather than distributors", na.rm = TRUE)
mean(Data$"Expanding content moderation on social media", na.rm = TRUE)
(5.18732+5.198142+5.497159)/3

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

mean(Data$"Owners of social media platforms", na.rm = TRUE)

mean(Data$"Foreing government, politicians/parties", na.rm = TRUE)
mean(Data$"Government, politicians/parties in my country", na.rm = TRUE)
(4.295597+4.430723)/2

mean(Data$"State-backed news organizations", na.rm = TRUE)
mean(Data$"Private news organizations", na.rm = TRUE)
(3.839181+3.870056)/2

mean(Data$"Ordinary citizens", na.rm = TRUE)
mean(Data$"Journalists or news organizations", na.rm = TRUE)
(3.015748+2.803191)/2


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

mean(Data$"Social networking platforms", na.rm = TRUE)
mean(Data$"Generative AI tools", na.rm = TRUE)
mean(Data$"Search engines", na.rm = TRUE)
mean(Data$"Messaging apps", na.rm = TRUE)
mean(Data$"Microblogging platforms", na.rm = TRUE)
mean(Data$"Video platforms", na.rm = TRUE)
mean(Data$"Discussion forums", na.rm = TRUE)

mean(Data$"News websites", na.rm = TRUE)
mean(Data$"Legacy media", na.rm = TRUE)
mean(Data$"Podcast platforms", na.rm = TRUE)

mean(Data$"Wikis", na.rm = TRUE)
mean(Data$"Professional networking sites", na.rm = TRUE)
(3.236422+3.178125)/2

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

mean(Data$"FUTURE_AI-generated videos", na.rm=T)
mean(Data$"PAST_AI-generated videos", na.rm=T)
(2.746177+2.5)/2
8-2.623088

mean(Data$"FUTURE_AI-generated text", na.rm=T)
mean(Data$"PAST_AI-generated text", na.rm=T)
(2.938235+3.331104)/2
8-3.13467

mean(Data$"FUTURE_AI-generated voices", na.rm=T)
mean(Data$"FUTURE_AI-generated images", na.rm=T)
(2.681199+2.77027+2.5+2.938235)/4
8-2.722426

mean(Data$"PAST_AI-generated videos", na.rm=T)
mean(Data$"PAST_AI-generated text", na.rm=T)
mean(Data$"PAST_AI-generated voices", na.rm=T)
mean(Data$"PAST_AI-generated images", na.rm=T)
(2.746177+3.331104+2.815182+3)/4
8-2.973116

(5.026884+5.277574)/2

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

mean(Data$"Increase misinfo quantity", na.rm =T)
mean(Data$"Increase misinfo persuasiveness", na.rm =T)
mean(Data$"Facilitate misinfo personalization", na.rm =T)
mean(Data$"Perpetuate or amplify biases and stereotypes", na.rm =T)
mean(Data$"Amplify harassment and abuse", na.rm =T)
(3.710462+3.694377+3.741379+3.885366+3.713235)/5


table(Data$"Increase misinfo quantity")
(118+ 127)/412
0.5946602
table(Data$"Increase misinfo persuasiveness")
(141 +112 )/412
0.6140777
table(Data$"Facilitate misinfo personalization")
(137 +124 )/412
0.6334951
table(Data$"Perpetuate or amplify biases and stereotypes")
(134 +149 )/412
0.6868932
table(Data$"Amplify harassment and abuse")
(134 +123 )/412
0.6237864

(0.6237864+0.6868932+0.6334951+0.6140777+0.5946602)/5

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

mean(Data$"Improve_automatic_content_detection", na.rm =T)
mean(Data$"Facilitate_the_work_of_journalists", na.rm =T)
mean(Data$"Facilitate_cross_cultural_and_cross_modal_communication", na.rm =T)
mean(Data$"Increase_persuasiveness_of_reliable_information", na.rm =T)
mean(Data$"Facilitate_personalization_of_reliable_information", na.rm =T)
(3.299754+3.189055+3.1825+2.896806+3.071605)/5


table(Data$"Improve_automatic_content_detection")
(134+ 115+63)/412
0.7572816
table(Data$"Facilitate_the_work_of_journalists")
(117+ 110 + 61 )/412
0.6990291
table(Data$"Facilitate_cross_cultural_and_cross_modal_communication")
(96 +120+  61  )/412
0.6723301
table(Data$"Increase_persuasiveness_of_reliable_information")
(98 + 89+  52  )/412
0.5800971
table(Data$"Facilitate_personalization_of_reliable_information")
(93 +111 + 55 )/412
0.6286408

(0.7572816+0.6990291+0.6723301+0.5800971+0.6286408)/5
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


mean(Data$"Collect_more_data_outside_of_the_US_and_Western_democracies", na.rm=T)
mean(Data$"Do_more_interdisciplinary_work", na.rm=T)
mean(Data$"Do_more_cross_platforms_work", na.rm=T)
(4.922141+4.82963)/2

mean(Data$"Mitigate_the_risks_posed_by_gen_AI", na.rm=T)
mean(Data$"Move_away_from_social_media_to_study_messaging_apps", na.rm=T)


write.csv(Data, "Data_stats_numerical.csv")



