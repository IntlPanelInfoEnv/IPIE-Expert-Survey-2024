##### Import Packages ####
library(rio)
library(tidyverse)
library(lme4)
library(jtools)
library(ggpubr)
library(forcats)

#### Cleaning and basic descriptives ####
Data <- import("Data.xlsx")
table(Data$Progress)
Data = filter(Data, Progress == 100)
 
Data <- Data %>%
   rename(Job = Q2,
          Career = Q11,
          Gender = Q3,
          Age = Q4,
          Country = Q6,
          Region_expertise = Q7,
          Country_expertise = Q9,
          Country_2_expertise = Q10,
          Disciplinary_background = Q12,
          Expertise_background = Q13)

Data = filter(Data, Job != "I’m not a researcher")

table(Data$Job)
table(Data$Career) 
table(Data$Gender) 
  
median(as.numeric(Data$Age), na.rm=T)
mean(as.numeric(Data$Age), na.rm=T)
sd(as.numeric(Data$Age), na.rm=T)


table(Data$Disciplinary_background)
table(Data$Expertise_background)

table(Data$Country) 

table(Data$Country_expertise) 
table(Data$Country_2_expertise) 
table(Data$Region_expertise)

all_regions <- unique(unlist(strsplit(as.character(Data$Region_expertise), ",")))
all_regions <- trimws(all_regions)
for (region in all_regions) {
  Data[[region]] <- 0
}
for (i in 1:nrow(Data)) {
  regions <- unlist(strsplit(as.character(Data$Region_expertise[i]), ","))
  regions <- trimws(regions)
  Data[i, regions] <- 1
}
Data <- Data[, !names(Data) %in% "Region_expertise"]
region_sums <- colSums(Data[, all_regions])
region_sums <- region_sums[!grepl("\\.1$", names(region_sums))]
sorted_regions <- sort(region_sums, decreasing = TRUE)
library(officer)
library(flextable)
region_data <- data.frame(
  Region = c("Northern America", "Western Europe", "Latin America and the Caribbean",
             "Eastern Europe", "Northern Europe", "Sub-Saharan Africa", "Eastern Asia",
             "South-eastern Asia", "Southern Europe", "Southern Asia",
             "Australia and New Zealand", "Northern Africa", "Western Asia",
             "Central Asia", "Polynesia"),
  Sum = c(193, 137, 54, 41, 40, 37, 31, 29, 28, 27, 25, 14, 13, 8, 1))
ft <- flextable(region_data)
ft <- set_table_properties(ft, width = 0.8, align = "center")
ft <- add_header_row(ft, values = c("Region", "Sum"))
print(ft)
doc <- read_docx() 
doc <- doc %>%
  body_add_flextable(value = ft)
print(doc, target = "table_output.docx")

table(Data$UserLanguage)

##### Healthy Info Env ####
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

Data_HealthInfoEnv <-gather(Data, Q, Response, Q15_1:Q15_7, factor_key=TRUE)%>%
  mutate(Response = ifelse(is.na(Response), "I don't know", Response))%>%
  mutate(Response = ifelse(Response == "I don't know", "Don't know", Response))

Data_HealthInfoEnv <- Data_HealthInfoEnv %>%
  mutate(Response = fct_relevel(Response, 
                                "Absolutely essential",
                                "Extremely important", 
                                "Very important", 
                                "Moderately important",
                                "Not very important", 
                                "Not important at all",
                                "Don't know"))%>%
  mutate(Q = fct_relevel(Q, 
                         "Q15_7",
                         "Q15_6",
                         "Q15_5",
                         "Q15_4",
                         "Q15_2", 
                         "Q15_1",
                         "Q15_3" ))%>%
  mutate(Q = recode(Q, 
                    Q15_1 = "Diversity of Voices",
                    Q15_2 = "Diversity of media ownership ",
                    Q15_3 = "Availability of accurate information",
                    Q15_4 = "Absence of false or misleading information", 
                    Q15_5 = "Absence of hateful content",
                    Q15_6 = "Absence of micro-targeted political ads",
                    Q15_7 = "Absence of AI-generated content"))

table(Data_HealthInfoEnv$Q, Data_HealthInfoEnv$Developed)

calculate_percentages_by_group <- function(data, developed_status) {
  total_responses <- nrow(data %>% filter(Developed == developed_status))
  data %>%
    filter(Developed == developed_status) %>%
    group_by(Q, Response) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    mutate(Percentage = Count / total_responses * 100)
}

percentages_developed <- calculate_percentages_by_group(Data_HealthInfoEnv, 1)
percentages_developing <- calculate_percentages_by_group(Data_HealthInfoEnv, 0)

combined_percentages <- bind_rows(
  percentages_developed %>% mutate(Developed = "Developed"),
  percentages_developing %>% mutate(Developed = "Developing")
)

combined_percentages = as.data.frame(combined_percentages)
print(combined_percentages)


ggplot(combined_percentages, aes(x=Q, y=Percentage,fill=Response)) + 
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c("#458cff", "#82c2ff","#bef7ff","#f7cdcd", "#ea8181","#d50e00","gray90"))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9,  color = "black", ),
        legend.key.height = unit(0.6, "cm"),
        legend.key.width = unit(0.6, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 19, lineheight = 1.1, color = "black"),
        legend.title =element_blank(),
        legend.direction = "horizontal",
        plot.title = element_text(size = 23, color = "black",vjust = 3,face="bold", hjust = 0.5),
        plot.margin = margin(20, 10, 10, 10))+
  guides(fill = guide_legend(nrow = 1,reverse=TRUE))+
  facet_wrap(~ Developed) +
  ggtitle("Importance for a good & healthy information environment")

ggsave("plot_HealthInfoEnv_DVP.pdf", width = 15, height = 10, dpi = 3000)

#### Individual interventions ####

Data_interventions <-gather(Data, Q, Response, Q16_1:Q16_5, factor_key=TRUE)%>%
  mutate(Response = ifelse(is.na(Response), "I don't know", Response))%>%
  mutate(Response = ifelse(Response == "I don't know", "Don't know", Response))

Data_interventions <- Data_interventions %>%
  mutate(Response = fct_relevel(Response, 
                                "Extremely",
                                "A lot", 
                                "Moderately", 
                                "Little",
                                "Very little", 
                                "Not at all",
                                "Don't know"))%>%
  mutate(Q = fct_relevel(Q, 
                         "Q16_2", 
                         "Q16_4",
                         "Q16_3",
                         "Q16_1",
                         "Q16_5",))%>%
  mutate(Q = recode(Q, 
                    Q16_1 = "Fact-checking",
                    Q16_2 = "Labeling AI content",
                    Q16_3 = "Labeling false/misleading content",
                    Q16_4 = "Labeling untrustworthy sources", 
                    Q16_5 = "Digital/media literacy tips"))


calculate_percentages_by_group <- function(data, developed_status) {
  total_responses <- nrow(data %>% filter(Developed == developed_status))
  data %>%
    filter(Developed == developed_status) %>%
    group_by(Q, Response) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    mutate(Percentage = Count / total_responses * 100)
}

percentages_developed <- calculate_percentages_by_group(Data_interventions, 1)
percentages_developing <- calculate_percentages_by_group(Data_interventions, 0)

combined_percentages <- bind_rows(
  percentages_developed %>% mutate(Developed = "Developed"),
  percentages_developing %>% mutate(Developed = "Developing"))

combined_percentages = as.data.frame(combined_percentages)

ggplot(combined_percentages, aes(x=Q,y=Percentage, fill=Response)) + 
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c("#458cff", "#82c2ff","#bef7ff","#f7cdcd", "#ea8181","#d50e00","gray90"))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 13,  color = "black", ),
        legend.key.height = unit(0.9, "cm"),
        legend.key.width = unit(0.9, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 20, lineheight = 1.2, color = "black"),
        legend.title =element_blank(),
        legend.direction = "horizontal",
        plot.title = element_text(size = 21, color = "black",vjust = 3,face="bold",hjust = 0.5),
        plot.margin = margin(20, 10, 10, 10))+
  guides(fill = guide_legend(nrow = 1,reverse=TRUE))+
  facet_wrap(~ Developed) +
  ggtitle("To what extent would the information environment improve if 
          the following were deployed at scale and adopted widely?")

ggsave("plot_Interventions_I_DVP.pdf", width = 15, height = 10, dpi = 3000)

#### System interventions ####

Data_interventions_system <-gather(Data, Q, Response, Q17_1:Q17_5, factor_key=TRUE)%>%
  mutate(Response = ifelse(is.na(Response), "Neither in favor nor against", Response))%>%
  mutate(Response = ifelse(Response == "Neither in favor nor against", "Neither", Response))

Data_interventions_system <- Data_interventions_system %>%
  mutate(Response = fct_relevel(Response, 
                                "Strongly in favor" ,
                                "In favor",
                                "Slightly in favor" ,
                                "Neither",
                                "Slightly against",
                                "Against" ,
                                "Strongly against"))%>%
  mutate(Q = fct_relevel(Q, 
                         "Q17_5",
                         "Q17_2",
                         "Q17_1",
                          
                         "Q17_3",
                         "Q17_4"
                         ))%>%
  mutate(Q = recode(Q, 
                    Q17_1 = "Expanding content moderation 
                    on social media",
                    Q17_2 = "Deplatforming problematic actors",
                    Q17_3 = "Adopting digital/media literacy campaigns
                     and educational programs",
                    Q17_4 = "Supporting free and independent media", 
                    Q17_5 = "Treat digital platforms as publishers
                     rather than distributors"))


percentages_developed <- calculate_percentages_by_group(Data_interventions_system, 1)
percentages_developing <- calculate_percentages_by_group(Data_interventions_system, 0)

combined_percentages <- bind_rows(
  percentages_developed %>% mutate(Developed = "Developed"),
  percentages_developing %>% mutate(Developed = "Developing")
)

combined_percentages = as.data.frame(combined_percentages)

ggplot(combined_percentages, aes(x=Q,y=Percentage, fill=Response)) + 
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c("#458cff", "#82c2ff","#bef7ff","gray90","#f7cdcd", "#ea8181","#d50e00"))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11,  color = "black", ),
        legend.key.height = unit(0.9, "cm"),
        legend.key.width = unit(0.9, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 20, lineheight = 1.2, color = "black"),
        legend.title =element_blank(),
        legend.direction = "horizontal",
        plot.title = element_text(size = 21, color = "black",vjust = 3,face="bold",hjust = 0.5),
        plot.margin = margin(20, 10, 10, 10))+
  guides(fill = guide_legend(nrow = 1,reverse=TRUE))+
  facet_wrap(~ Developed) +
  ggtitle("To what extent are you in favor of these system-level 
          measures to improve the information environment?")

ggsave("plot_Interventions_S_DVP.pdf", width = 15, height = 10, dpi = 3000)

#### Threat Actors ####

Data_threat_actors <-gather(Data, Q, Response, Q18_1:Q18_8, factor_key=TRUE)%>%
  mutate(Response = ifelse(is.na(Response), "I don't know", Response))%>%
  mutate(Response = ifelse(Response == "I don't know", "Don't know", Response))%>%
  mutate(Response = recode(Response,
                         "Not a threat at All" = "Not at All",
                         "Very small threat" = "Very small",
                         "Small threat" = "Small",
                         "Moderate threat" = "Moderate",
                         "Big threat" = "Big",
                         "Very big threat" = "Very big",
                         "The biggest threat" = "The biggest"))

Data_threat_actors <- Data_threat_actors %>%
  mutate(Response = fct_relevel(Response, 
                                "The biggest",
                                "Very big", 
                                "Big", 
                                "Moderate",
                                "Small",
                                "Very small", 
                                "Not at All",
                                "Don't know"))%>%
  mutate(Q = fct_relevel(Q, 
                         "Q18_7",
                         "Q18_2",
                         "Q18_3",
                         "Q18_5",
                         "Q18_6",
                         "Q18_4",
                         "Q18_1",
                         "Q18_8"
                         ))%>%
  mutate(Q = recode(Q, 
                    Q18_1 = "Government, politicians/parties 
                    in my country",
                    Q18_2 = "Ordinary citizens",
                    Q18_3 = "Activists",
                    Q18_4 = "Foreing government, politicians/parties", 
                    Q18_5 = "Private news organizations",
                    Q18_6 = "State-backed news organizations",
                    Q18_7 = "Journalists or news organizations",
                    Q18_8 = "Owners of social media platforms"))

percentages_developed <- calculate_percentages_by_group(Data_threat_actors, 1)
percentages_developing <- calculate_percentages_by_group(Data_threat_actors, 0)

combined_percentages <- bind_rows(
  percentages_developed %>% mutate(Developed = "Developed"),
  percentages_developing %>% mutate(Developed = "Developing")
)

combined_percentages = as.data.frame(combined_percentages)
  


ggplot(combined_percentages, aes(x=Q,y=Percentage, fill=Response)) + 
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c(
    "The biggest" = "#b2182b",    # dark red
    "Very big" = "#d6604d",       # soft red-orange
    "Big" = "#f4a582",            # peach
    "Moderate" = "#fddbc7",       # muted purple
    "Small" = "#d1e5f0",          # soft blue
    "Very small" = "#67a9cf",     # light blue
    "Not at All" = "#2166ac",     # dark blue
    "Don't know" = "#bdbdbd"      # gray
  ))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11,  color = "black", ),
        legend.key.height = unit(0.9, "cm"),
        legend.key.width = unit(0.9, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 20, lineheight = 1.2, color = "black"),
        legend.title =element_blank(),
        legend.direction = "horizontal",
        plot.title = element_text(size = 21, color = "black",vjust = 3,face="bold",hjust = 0.5),
        plot.margin = margin(20, 10, 10, 10))+
  guides(fill = guide_legend(nrow = 1,reverse=TRUE))+
  facet_wrap(~ Developed) +
  ggtitle("To what extent does each of the following represent
          a threat to the information environment?")

ggsave("plot_Threats_Actors_DVP.pdf", width = 15, height = 10, dpi = 3000)


#### Threat Platforms ####
Data_threat_plat <-gather(Data, Q, Response, Q19_1:Q19_12, factor_key=TRUE)%>%
  mutate(Response = ifelse(is.na(Response), "I don't know", Response))%>%
  mutate(Response = ifelse(Response == "I don't know", "Don't know", Response))%>%
  mutate(Response = recode(Response,
                           "Not a threat at all" = "Not at All",
                           "Very small threat" = "Very small",
                           "Small threat" = "Small",
                           "Moderate threat" = "Moderate",
                           "Big threat" = "Big",
                           "Very big threat" = "Very big",
                           "The biggest threat" = "The biggest"))


Data_threat_plat <- Data_threat_plat %>%
  mutate(Response = fct_relevel(Response, 
                                "The biggest",
                                "Very big", 
                                "Big", 
                                "Moderate",
                                "Small",
                                "Very small", 
                                "Not at All",
                                "Don't know"))%>%
  mutate(Q = fct_relevel(Q, 
                         "Q19_5",
                         "Q19_10",
                         "Q19_4", 
                         "Q19_2", 
                         "Q19_1",
                         "Q19_3",
                         "Q19_8",
                         "Q19_9",
                         "Q19_7",
                         "Q19_12",
                         "Q19_6",
                         "Q19_11"))%>%
  mutate(Q = recode(Q, 
                    Q19_1 = "News websites",
                    Q19_2 = "Legacy media",
                    Q19_3 = "Search engines",
                    Q19_4 = "Podcast platforms", 
                    Q19_5 = "Wikis",
                    Q19_6 = "Video platforms",
                    Q19_7 = "Messaging apps",
                    Q19_8 = "Discussion forums",
                    Q19_9 = "Microblogging platforms",
                    Q19_10 = "Professional networking sites",
                    Q19_11 = "Social networking platforms",
                    Q19_12 = "Generative AI tools"))


percentages_developed <- calculate_percentages_by_group(Data_threat_plat, 1)
percentages_developing <- calculate_percentages_by_group(Data_threat_plat, 0)

combined_percentages <- bind_rows(
  percentages_developed %>% mutate(Developed = "Developed"),
  percentages_developing %>% mutate(Developed = "Developing")
)

combined_percentages = as.data.frame(combined_percentages)

ggplot(combined_percentages, aes(x=Q,y=Percentage, fill=Response)) + 
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c(
    "The biggest" = "#b2182b",    # dark red
    "Very big" = "#d6604d",       # soft red-orange
    "Big" = "#f4a582",            # peach
    "Moderate" = "#fddbc7",       # muted purple
    "Small" = "#d1e5f0",          # soft blue
    "Very small" = "#67a9cf",     # light blue
    "Not at All" = "#2166ac",     # dark blue
    "Don't know" = "#bdbdbd"      # gray
  ))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11,  color = "black", ),
        legend.key.height = unit(0.9, "cm"),
        legend.key.width = unit(0.9, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 20, lineheight = 1.2, color = "black"),
        legend.title =element_blank(),
        legend.direction = "horizontal",
        plot.title = element_text(size = 21, color = "black",vjust = 3,face="bold",hjust = 0.5),
        plot.margin = margin(20, 10, 10, 10))+
  guides(fill = guide_legend(nrow = 1,reverse=TRUE))+
  facet_wrap(~ Developed) +
  ggtitle("To what extent does each of the following represent
          a threat to the information environment?")

ggsave("plot_Threats_Plat_DVP.pdf", width = 15, height = 10, dpi = 3000)

#### AI past and future #### 
Data_past <-gather(Data, Q, Response, Q20_1:Q20_4, factor_key=TRUE)%>%
  mutate(Response = ifelse(is.na(Response), "I don't know", Response))%>%
  mutate(Response = ifelse(Response == "I don't know", "Don't know", Response))%>%
  mutate(Response = recode(Response,
                           "Neither improve nor worsen" = "Neither"))

Data_past <- Data_past %>%
  mutate(Response = fct_relevel(Response,
                                "Greatly improve",
                                "Somewhat improve",
                                "Slightly improve",
                                "Neither",
                                "Don't know",
                                "Slightly worsen",
                                "Somewhat worsen",
                                "Greatly worsen"))%>%
  mutate(Q = fct_relevel(Q, 
                         "Q20_1",
                         "Q20_2",
                         "Q20_3",
                         "Q20_4"))%>%
  mutate(Q = recode(Q, 
                    "Q20_1"= "AI-generated text",
                    "Q20_2"="AI-generated images",
                    "Q20_3"= "AI-generated voices",
                    "Q20_4" = "AI-generated videos"))

percentages_developed <- calculate_percentages_by_group(Data_past, 1)
percentages_developing <- calculate_percentages_by_group(Data_past, 0)

combined_percentages <- bind_rows(
  percentages_developed %>% mutate(Developed = "Developed"),
  percentages_developing %>% mutate(Developed = "Developing")
)

combined_percentages = as.data.frame(combined_percentages)

past = ggplot(combined_percentages, aes(x=Q, y=Percentage,fill=Response)) + 
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c("#458cff", "#82c2ff","#bef7ff","gray70","gray50","#f7cdcd", "#ea8181","#d50e00"))+
  theme_bw()+
  theme(legend.position = "none",
        legend.text = element_text(size = 13,  color = "black", ),
        legend.key.height = unit(0.9, "cm"),
        legend.key.width = unit(0.9, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 27, lineheight = 1.2, color = "black"),
        legend.title =element_blank(),
        legend.direction = "horizontal",
        plot.title = element_text(size = 26, color = "black",vjust = 3,face="bold",hjust = 0.5),
        plot.margin = margin(20, 10, 10, 10))+
  guides(fill = guide_legend(nrow = 1,reverse=TRUE))+
  facet_wrap(~ Developed) +
  ggtitle("How gen-AI tools *have affected*  
  the info. env.?")
past
ggsave("plot_past_DVP.pdf", width = 15, height = 10, dpi = 3000) #Manually add the legend on powerpoint

Data_future <-gather(Data, Q, Response, Q21_1:Q21_4, factor_key=TRUE)%>%
  mutate(Response = ifelse(is.na(Response), "I don't know", Response))%>%
  mutate(Response = ifelse(Response == "I don't know", "Don't know", Response))%>%
  mutate(Response = recode(Response,
                           "Neither improve nor worsen" = "Neither"))

Data_future <- Data_future %>%
  mutate(Response = fct_relevel(Response,
                                "Greatly improve",
                                "Somewhat improve",
                                "Slightly improve",
                                "Neither",
                                "Don't know",
                                "Slightly worsen",
                                "Somewhat worsen",
                                "Greatly worsen"))%>%
  mutate(Q = fct_relevel(Q, 
                         "Q21_1",
                         "Q21_2",
                         "Q21_3",
                         "Q21_4"))%>%
  mutate(Q = recode(Q, 
                    "Q21_1"= "AI-generated text",
                    "Q21_2"="AI-generated images",
                    "Q21_3"= "AI-generated voices",
                    "Q21_4" = "AI-generated videos"))


percentages_developed <- calculate_percentages_by_group(Data_future, 1)
percentages_developing <- calculate_percentages_by_group(Data_future, 0)

combined_percentages <- bind_rows(
  percentages_developed %>% mutate(Developed = "Developed"),
  percentages_developing %>% mutate(Developed = "Developing")
)

combined_percentages = as.data.frame(combined_percentages)

future = ggplot(combined_percentages, aes(x=Q, y=Percentage,fill=Response)) + 
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c("#458cff", "#82c2ff","#bef7ff","gray70","gray50","#f7cdcd", "#ea8181","#d50e00"))+
  theme_bw()+
  theme(legend.position = "none",
        legend.text = element_text(size = 11,  color = "black", ),
        legend.key.height = unit(0.9, "cm"),
        legend.key.width = unit(0.9, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 24, lineheight = 1.2, color = "black"),
        legend.title =element_blank(),
        legend.direction = "horizontal",
        plot.title = element_text(size = 26, color = "black",vjust = 3,face="bold",hjust = 0.5),
        plot.margin = margin(20, 10, 10, 10))+
  guides(fill = guide_legend(nrow = 1,reverse=TRUE))+
  facet_wrap(~ Developed) +
  ggtitle("How gen-AI tools *will affect* the info.
           env. in the next 5 years?")
future
ggsave("plot_future_DVP.pdf", width = 15, height = 10, dpi = 3000) #Manually add the legend on powerpoint


#### Concerns Gen-AI  ####

Data_concernAI <-gather(Data, Q, Response, Q22_1:Q22_5, factor_key=TRUE)%>%
  mutate(Response = ifelse(is.na(Response), "I don't know", Response))%>%
  mutate(Response = ifelse(Response == "I don't know", "Don't know", Response))%>%
  mutate(Response = recode(Response,
                           "Extremely concerned" = "Extremely",
                           "Very concerned" = "Very",
                           "Moderately concerned" = "Moderately",
                           "A little concerned" = "A little",
                           "Not at all concerned" = "Not at all"))

Data_concernAI <- Data_concernAI %>%
  mutate(Response = fct_relevel(Response, 
                                "Extremely", 
                                "Very", 
                                "Moderately",
                                "A little", 
                                "Not at all",
                                "Don't know"))%>%
  mutate(Q = fct_relevel(Q, 
                         "Q22_2", 
                         "Q22_1",
                         "Q22_5",
                         "Q22_3",
                         "Q22_4"))%>%
  mutate(Q = recode(Q, 
                    "Q22_1" = "Increase misinfo quantity",
                    "Q22_2" = "Increase misinfo persuasiveness",
                    "Q22_3" = "Facilitate misinfo personalization",
                    "Q22_4" = "Perpetuate or amplify biases and stereotypes", 
                    "Q22_5" = "Amplify harassment and abuse"))

percentages_developed <- calculate_percentages_by_group(Data_concernAI, 1)
percentages_developing <- calculate_percentages_by_group(Data_concernAI, 0)

combined_percentages <- bind_rows(
  percentages_developed %>% mutate(Developed = "Developed"),
  percentages_developing %>% mutate(Developed = "Developing")
)

combined_percentages = as.data.frame(combined_percentages)


ggplot(combined_percentages, aes(x=Q,y=Percentage, fill=Response)) + 
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c("#b2182b","#d6604d", "#f4a582", "#fddbc7","#458cff","gray70"))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11,  color = "black", ),
        legend.key.height = unit(0.9, "cm"),
        legend.key.width = unit(0.9, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 20, lineheight = 1.2, color = "black"),
        legend.title =element_blank(),
        legend.direction = "horizontal",
        plot.title = element_text(size = 21, color = "black",vjust = 3,face="bold",hjust = 0.5),
        plot.margin = margin(20, 10, 10, 10))+
  guides(fill = guide_legend(nrow = 1,reverse=TRUE))+
  facet_wrap(~ Developed) +
  ggtitle("To what extent are you *concerned* that generative AI may…")

ggsave("plot_concernsAI_DVP.pdf", width = 15, height = 10, dpi = 3000)

#### Hopes Gen-AI  ####

Data_hopesAI <-gather(Data, Q, Response, Q23_1:Q23_5, factor_key=TRUE)%>%
  mutate(Response = ifelse(is.na(Response), "I don't know", Response))%>%
  mutate(Response = ifelse(Response == "I don't know", "Don't know", Response))%>%
  mutate(Response = recode(Response,
                           "Extremely hopeful" = "Extremely",
                           "Very hopeful" = "Very",
                           "Moderately hopeful" = "Moderately",
                           "A little hopeful" = "A little",
                           "Not at all hopeful" = "Not at all"))

Data_hopesAI <- Data_hopesAI %>%
  mutate(Response = fct_relevel(Response, 
                                "Extremely", 
                                "Very", 
                                "Moderately",
                                "A little", 
                                "Not at all",
                                "Don't know"))%>%
  mutate(Q = fct_relevel(Q, 
                         "Q23_4",  
                         "Q23_5",
                         "Q23_3",
                         "Q23_2", 
                         "Q23_1"))%>%
  mutate(Q = recode(Q, 
                    "Q23_1" = "Improve automatic content detection",
                    "Q23_2" = "Facilitate the work of journalists",
                    "Q23_3" = "Facilitate cross-cultural and 
                    cross-modal communication",
                    "Q23_4" = "Increase persuasiveness of 
                    reliable information", 
                    "Q23_5" = "Facilitate personalization of 
                    reliable information"))

percentages_developed <- calculate_percentages_by_group(Data_hopesAI, 1)
percentages_developing <- calculate_percentages_by_group(Data_hopesAI, 0)

combined_percentages <- bind_rows(
  percentages_developed %>% mutate(Developed = "Developed"),
  percentages_developing %>% mutate(Developed = "Developing"))
combined_percentages = as.data.frame(combined_percentages)


ggplot(combined_percentages, aes(x=Q,y=Percentage, fill=Response)) + 
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c("#2166ac","#4393c4", "#92c5de", "#d1e5f0","#b2182b","gray70"))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11,  color = "black", ),
        legend.key.height = unit(0.9, "cm"),
        legend.key.width = unit(0.9, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 20, lineheight = 1.2, color = "black"),
        legend.title =element_blank(),
        legend.direction = "horizontal",
        plot.title = element_text(size = 21, color = "black",vjust = 3,face="bold",hjust = 0.5),
        plot.margin = margin(20, 10, 10, 10))+
  guides(fill = guide_legend(nrow = 1,reverse=TRUE))+
  facet_wrap(~ Developed) +
  ggtitle("To what extent are you *hopeful* that generative AI may…")

ggsave("plot_hopesAI_DVP.pdf", width = 15, height = 10, dpi = 3000)


#### PREDICTION FUTURE by country ####
library(forcats)

Data <- Data %>%
  mutate(Q25 = ifelse(Q25 == "Believe that it will neither improve nor worsen", "Neither", Q25)) %>%
  mutate(Q25 = fct_relevel(Q25, 
                           "Very confident it will worsen", 
                           "Moderately confident it will worsen", 
                           "Neither",
                           "Moderately confident it will improve",
                           "Very confident it will improve")) %>%
  mutate(Q25 = recode(Q25,
                      "Very confident it will worsen" = "Worsen - Very confident",
                      "Moderately confident it will worsen" = "Worsen - Moderately confident",
                      "Very confident it will improve" = "Improve - Very confident",
                      "Moderately confident it will improve" = "Improve - Moderately confident"))

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
Data$Developed <- ifelse(Data$Country_expertise %in% developed_countries, "Developed", "Developing")

total_responses_by_group <- Data %>%
  group_by(Developed) %>%
  summarise(total_responses = n())

results_by_income_group <- Data %>%
  group_by(Developed, Q25) %>%
  summarise(count = n(), .groups = 'drop') %>%
  left_join(total_responses_by_group, by = "Developed") %>%
  mutate(percentage = count / total_responses * 100) %>%
  arrange(Developed, Q25)

percent_data <- as.data.frame(results_by_income_group)

print(results_by_income_group)


ggplot(percent_data, aes(x = Q25, y = percentage, fill = Q25)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#b2182b", "#ea8181", "gray70", "#82c2ff", "#458cff")) +  # Colors
  labs(title = "How confident are you that the information environment will improve 
       or worsen in the next year in your country of expertise?",
       x = "",  # No x-label
       y = "N") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust =1, hjust = 1)) +  # Rotate x-axis labels by 45 degrees
  theme(legend.position = "none") +  # Hide legend
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10, lineheight = 1.2, color = "black"),
        axis.text.x = element_text(size = 15, lineheight = 1.2, color = "black"),
        legend.title =element_blank(),
        legend.direction = "horizontal",
        plot.title = element_text(size = 22, color = "black",vjust = 3,face="bold",hjust = 0.5),
        plot.margin = margin(20, 10, 10, 10))+
  facet_wrap(~ Developed) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_stack(vjust = 0.5), size = 7)

ggsave("plot_prediction_general_DVP.pdf", width = 15, height = 10, dpi = 3000)

#### Future of the field  ####
Data_FutureField <-gather(Data, Q, Response, Q27_1:Q27_5, factor_key=TRUE)%>%
  mutate(Response = ifelse(is.na(Response), "I don't know", Response))%>%
  mutate(Response = ifelse(Response == "I don't know", "Don't know", Response))%>%
  mutate(Response = recode(Response,
                           "Extremely important" = "Extremely",
                           "Very important" = "Very",
                           "Moderately important" = "Moderately",
                           "Not very important" = "A little",
                           "Not at all important" = "Not at all"))

Data_FutureField <- Data_FutureField %>%
  mutate(Response = fct_relevel(Response, 
                                "Absolutely essential",
                                "Extremely", 
                                "Very", 
                                "Moderately",
                                "A little", 
                                "Not at all",
                                "Don't know"))%>%
  mutate(Q = fct_relevel(Q, 
                         "Q27_4", 
                         "Q27_3",
                         "Q27_5",
                         "Q27_2", 
                         "Q27_1",
                         ))%>%
  mutate(Q = recode(Q, 
                    "Q27_1" = "Collect more data outside of 
                    the US and Western democracies",
                    "Q27_2" = "Do more interdisciplinary work",
                    "Q27_3" = "Mitigate the risks posed by gen-AI",
                    "Q27_4" = "Move away from social media 
                    to study messaging apps", 
                    "Q27_5" = "Do more cross-platforms work"))

Data$Developed <- ifelse(Data$Country_expertise %in% developed_countries, 1,0)
percentages_developed <- calculate_percentages_by_group(Data_FutureField, 1)
percentages_developing <- calculate_percentages_by_group(Data_FutureField, 0)

combined_percentages <- bind_rows(
  percentages_developed %>% mutate(Developed = "Developed"),
  percentages_developing %>% mutate(Developed = "Developing"))
combined_percentages = as.data.frame(combined_percentages)


ggplot(combined_percentages, aes(x=Q,y=Percentage, fill=Response)) + 
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c("#2166ac","#4393c4", "#92c5de", "#d1e5f0", "#ea8181","#b2182b","gray70"))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11,  color = "black", ),
        legend.key.height = unit(0.9, "cm"),
        legend.key.width = unit(0.9, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 20, lineheight = 1.2, color = "black"),
        legend.title =element_blank(),
        legend.direction = "horizontal",
        plot.title = element_text(size = 21, color = "black",vjust = 3,face="bold",hjust = 0.5),
        plot.margin = margin(20, 10, 10, 10))+
  guides(fill = guide_legend(nrow = 1,reverse=TRUE))+
  facet_wrap(~ Developed) +
  ggtitle("To what extent is it important that future research
            on the information environment…")
ggsave("plot_future_field_DVP.pdf", width = 15, height = 10, dpi = 3000)


#### BARRIERS ####
# Some participants at the beginning could only select one barrier. 

table(Data$Q26)

Data$Funding <- 0
Data$DataAccess <- 0
Data$GovRestriction <- 0
Data$PolPressure <- 0
Data$PrivaRestric <- 0

Data$Funding <- as.integer(grepl("Funding opportunities", Data$Q26, ignore.case = TRUE))
Data$DataAccess <- as.integer(grepl("Data access", Data$Q26, ignore.case = TRUE))
Data$GovRestriction <- as.integer(grepl("Governmental restrictions", Data$Q26, ignore.case = TRUE))
Data$PolPressure <- as.integer(grepl("Political pressures", Data$Q26, ignore.case = TRUE))
Data$PrivaRestric <- as.integer(grepl("Privacy restrictions", Data$Q26, ignore.case = TRUE))
Data$none <- ifelse(is.na(Data$Q26), 1, 0)

column_sums <- colSums(Data[, c("Funding", "DataAccess", 
                                "GovRestriction", "PolPressure", 
                                "PrivaRestric", "none")], na.rm = TRUE)
sum_data <- data.frame(variable = names(column_sums), sum = column_sums)
sum_data <- sum_data[order(sum_data$sum, decreasing = TRUE), ]
sum_data$variable <- factor(sum_data$variable, levels = sum_data$variable)

sum_data <- sum_data %>%
  mutate(variable = recode(variable,
                           "Funding" = "Funding opportunities",
                           "DataAccess" = "Data access",
                           "GovRestriction" = "Governmental restrictions 
                           (e.g. censorship)",
                           "PolPressure" = "Political pressures 
                           (e.g. intimidations, lawsuits, etc.)",
                           "PrivaRestric" = "Privacy restrictions 
                           (e.g. GDPR)",
                           "none" = "None"))

# ggplot(sum_data, aes(x = variable, y = sum)) +
#   geom_bar(stat = "identity", fill = "skyblue", color = "black") +
#   geom_text(size = 5, vjust = -0.5, color = "black", aes(label = sum)) +
#   labs(title = "Sum of Variables", x = "Variables", y = "Sum") +
#   theme_minimal() +
#   theme_bw()+
#   theme(legend.position = "bottom",
#         legend.text = element_text(size = 11,  color = "black", ),
#         legend.key.height = unit(0.9, "cm"),
#         legend.key.width = unit(0.9, "cm"),
#         axis.title.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_text(size = 17.5, angle = 45, hjust = 1),
#         axis.text.y = element_text(size = 13, lineheight = 1.2, color = "black"),
#         legend.title =element_blank(),
#         legend.direction = "horizontal",
#         plot.title = element_text(size = 21, color = "black",vjust = 3,face="bold",hjust = 0.5),
#         plot.margin = margin(20, 10, 10, 10))+
#   ggtitle("What barriers or challenges do you currently face in your research on the information environment? 
#           (N = 412)")+
#   scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))+
#   scale_x_discrete(expand = expansion(add = c(0.7, 0.5)))
# 
# ggsave("plot_barriers.pdf", width = 15, height = 10, dpi = 3000)


#### BARRIERS By country ####
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

calculate_percentages <- function(data, developed_status) {
  total_responses <- nrow(data %>% filter(Developed == developed_status))
  data %>%
    filter(Developed == developed_status) %>%
    summarise(across(c(Funding, DataAccess, GovRestriction, PolPressure, PrivaRestric, none), 
                     ~sum(.x, na.rm = TRUE) / total_responses * 100))}

percentages_developed <- calculate_percentages(Data, 1)
percentages_developing <- calculate_percentages(Data, 0)

combined_percentages <- rbind(
  cbind(variable = names(percentages_developed), percentage = as.numeric(percentages_developed), Developed = "Developed"),
  cbind(variable = names(percentages_developing), percentage = as.numeric(percentages_developing), Developed = "Developing")
)

percent_data <- as.data.frame(combined_percentages)
percent_data$percentage <- as.numeric(as.character(percent_data$percentage))

percent_data <- percent_data %>%
  mutate(variable = recode(variable,
                           "Funding" = "Funding opportunities",
                           "DataAccess" = "Data access",
                           "GovRestriction" = "Governmental restrictions 
                           (e.g. censorship)",
                           "PolPressure" = "Political pressures 
                           (e.g. intimidations, lawsuits, etc.)",
                           "PrivaRestric" = "Privacy restrictions 
                           (e.g. GDPR)",
                           "none" = "None"))

percent_data$percentage <- round(percent_data$percentage,1)

ggplot(percent_data, aes(x = variable, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(size = 5, vjust = -0.5, color = "black", aes(label = percentage)) +
  labs(title = "Sum of Variables", x = "Variables", y = "Sum") +
  theme_minimal() +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11,  color = "black", ),
        legend.key.height = unit(0.9, "cm"),
        legend.key.width = unit(0.9, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 17.5, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 13, lineheight = 1.2, color = "black"),
        legend.title =element_blank(),
        legend.direction = "horizontal",
        plot.title = element_text(size = 21, color = "black",vjust = 3,face="bold",hjust = 0.5),
        plot.margin = margin(20, 10, 10, 10))+
  ggtitle("What barriers or challenges do you currently face in your research on the information environment?
          (N = 412)")+
  facet_wrap(~Developed)+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))+
  scale_x_discrete(expand = expansion(add = c(0.7, 0.5)))

ggsave("plot_barriers_DVP.pdf", width = 15, height = 10, dpi = 3000)



#### Myths about misinformation ####

Data$Misinfo_Social_Media <- 0
Data$Internet_Rife_Misinfo <- 0
Data$Falsehoods_Spread_Faster <- 0
Data$Large_Number_Misinformed <- 0
Data$Misinfo_Influences_Behavior <- 0
Data$More_People_Misinformed <- 0
Data$Gullible <- 0
Data$no_misconception <- 0

Data$Misinfo_Social_Media <- as.integer(grepl("Misinformation is predominantly a social media problem", Data$Q24, ignore.case = TRUE))
Data$Internet_Rife_Misinfo <- as.integer(grepl("The internet is rife with misinformation", Data$Q24, ignore.case = TRUE))
Data$Falsehoods_Spread_Faster <- as.integer(grepl("Falsehoods spread faster than the truth", Data$Q24, ignore.case = TRUE))
Data$Large_Number_Misinformed <- as.integer(grepl("A large number of people are misinformed about important topics", Data$Q24, ignore.case = TRUE))
Data$Misinfo_Influences_Behavior <- as.integer(grepl("Misinformation has a substantial influence on people's behavior", Data$Q24, ignore.case = TRUE))
Data$More_People_Misinformed <- as.integer(grepl("More people have become misinformed about important topics over time", Data$Q24, ignore.case = TRUE))
Data$Gullible <- as.integer(grepl("People readily believe much of what they see on the internet whether it is true or false", Data$Q24, ignore.case = TRUE))
Data$no_misconception <- ifelse(is.na(Data$Q24), 1, 0)

column_sums <- colSums(Data[, c("Misinfo_Social_Media", "Internet_Rife_Misinfo", 
                                "Falsehoods_Spread_Faster", "Large_Number_Misinformed", 
                                "Misinfo_Influences_Behavior", "More_People_Misinformed", 
                                "Gullible", "no_misconception")], na.rm = TRUE)

sum_data <- data.frame(variable = names(column_sums), sum = column_sums)
sum_data <- sum_data[order(sum_data$sum, decreasing = TRUE), ]
sum_data$variable <- factor(sum_data$variable, levels = sum_data$variable)

sum_data <- sum_data %>%
  mutate(variable = recode(variable,
                           "Misinfo_Social_Media" = "Misinformation is predominantly 
                           a social media problem",
                           "Internet_Rife_Misinfo" = "Internet is rife with misinformation",
                           "Falsehoods_Spread_Faster" = "Falsehoods spread
                           faster than the truth",
                           "Large_Number_Misinformed" = "Large numbers are misinformed 
                           about important topics",
                           "Misinfo_Influences_Behavior" = "Misinfo has substantial 
                           influence on behaviors",
                           "More_People_Misinformed" = "More people have become misinformed
                           about important topics over time",
                           "Gullible" = "People readily believe much
                            of what they see online",
                           "no_misconception" = "None"))




ggplot(sum_data, aes(x = variable, y = sum)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(size = 5, vjust = -0.5, color = "black", aes(label = sum)) +
  labs(title = "Sum of Variables", x = "Variables", y = "Sum") +
  theme_minimal() +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11,  color = "black", ),
        legend.key.height = unit(0.9, "cm"),
        legend.key.width = unit(0.9, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 17.5, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 13, lineheight = 1.2, color = "black"),
        legend.title =element_blank(),
        legend.direction = "horizontal",
        plot.title = element_text(size = 21, color = "black",vjust = 3,face="bold",hjust = 0.5),
        plot.margin = margin(20, 10, 10, 10))+
  ggtitle("Which, if any, of the following do you agree with? (N = 412)")+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))+
  scale_x_discrete(expand = expansion(add = c(0.7, 0.5)))

ggsave("plot_misinfomisinfo.pdf", width = 15, height = 10, dpi = 3000)

