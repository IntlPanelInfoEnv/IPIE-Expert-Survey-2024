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

#### MAP COUNTRY EXPERTISE SF package #####
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(maps)
library(countrycode)
library(ggthemes)

table_expertise1 <- as.data.frame(table(Data$Country_expertise))
table_expertise2 <- as.data.frame(table(Data$Country_2_expertise))
summed_table <- aggregate(Freq ~ Var1, FUN = sum, data = rbind(table_expertise1, table_expertise2))
names(summed_table) <- c("country", "count")
data <- as.data.frame(summed_table)

data$country <- gsub("Ireland \\{Republic\\}", "Ireland", data$country)
data$country[data$country == "Korea South"] <- "South Korea"
#data$country[data$country == "Taiwan"] <- "Taiwan"
#data$country[data$country == "United Kingdom"] <- "UK"
data$country[data$country == "United States"] <- "USA"
data$country[data$country == "Russian Federation"] <- "Russia"

world_map <- ne_countries(scale = "medium", returnclass = "sf")
world_map <- subset(world_map, name != "Antarctica")
world_map$name[world_map$name == "United States of America"] <- "USA"

merged_data <- merge(world_map, data, by.x = "name", by.y = "country", all.x = TRUE)
merged_data$count[is.na(merged_data$count)] <- 0

merged_data$count <- cut(merged_data$count, 
                         breaks = c(-1, 0, 1, 5, 10, 20, 40, 187),
                         labels = c("0", "1", "2-5", "5-10", "10-20", "20-40", "187"))
merged_data_nozero <- subset(merged_data, count != "0")



ggplot(data = merged_data) +
  geom_sf(aes(fill = count, color = count), lwd = 0.2) +
  theme_map() +
  coord_sf(crs = "+proj=eqearth")+
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black", "black")) +
  scale_fill_manual(values = c("white", "lightgoldenrod1", "gold", "goldenrod2", "darkorange2", "firebrick", "black"))+
  guides(fill = "none", color = "none")
ggsave("plot_map.pdf", width = 15, height = 10, dpi = 3000)




