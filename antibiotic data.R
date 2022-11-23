# Analyzing lab data 

#load packages
library(tidyverse)
library(car)
library(readr)
library(janitor)

data <- read_csv("C:/Users/waven/Downloads/Data reorganized - Sheet1.csv")
data <- data %>% clean_names()
data <- data %>%
  group_by(antibiotic_treatment, hours) %>%
  mutate(mean_percent_resistance = mean(percent_resistance))

data1 <- data

data1$hours <- as.character(data1$hours)
glimpse(data1)

data1 %>% 
  ggplot(aes(antibiotic_treatment, percent_resistance,
             color = hours)) +
  geom_point() +
  stat_summary(fun = mean, geom="crossbar")+
  theme_minimal()+
  labs(y= "Percent Resistance to 100ug/ml Rifampicin", x = "Antibiotic Treatment")+
  labs(fill = "Hours Exposed to Each Antibiotic")+
  theme_bw()+
  theme(legend.position = "top")

 # data %>%
 # ggplot(aes(x = antibiotic_treatment, 
 #           y = percent_resistance, 
 #            fill = as.factor(hours)))+
 # geom_boxplot()+ 
 # geom_point( color = "gray") +
 # stat_summary(fun = mean, geom="point",
 #              position = position_dodge2(width = 0.75,
 #                                         preserve = "single"))
        
two_way_ANOVA <- summary(
  aov(percent_resistance ~ hours*antibiotic_treatment, 
      data = data1))
