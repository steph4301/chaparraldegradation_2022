# load data
library(readxl)
library(tidyverse)
library(openxlsx)


grassmortality_load <- read_excel("Grass seedling trial - Piru 2018.xlsx", sheet = "grassmortality")
grassmortality <- grassmortality_load %>% 
  mutate(date = as.character(date), 
         temp = as.factor(temp), 
         min = as.character(min), 
         rep = as.character(rep), 
         grass_germ = as.integer(grass_germ)) %>% 
  mutate(temp.min = paste(temp, min, sep = "C.")) %>% 
  mutate(uniqueID = paste(temp.min, rep, sep = ".rep")) %>% 
  mutate(temp = factor(temp, levels = c(20, 50, 100, 150, 200, 250)))  # Reorder
  
  
  
  
# head(grassmortality)
#  unique(grassmortality$date) <-- 12 days
#  unique(grassmortality$temp) <-- 6 temperatures "20"  "100" "150" "200" "250" "50" 
#  unique(grassmortality$min) <-- 3 exposure durations "2"  "5"  "15"
#  unique(grassmortality$rep) <-- 5 reps per treatment
#  6 x 3 x 5 = 90 unique samples

# view data
plot(grass_germ ~ date, grassmortality)

max_perrep <- grassmortality %>%
  dplyr::select(temp, min, rep, temp.min, uniqueID, grass_germ) %>% 
  group_by(temp, min, rep, temp.min, uniqueID) %>%
  summarize(max_germ = max(grass_germ)) 
 

mean_pertreat <- max_perrep %>% 
  filter(min == "5") %>% # select only 2 min duration for Ch1 Stand Deg Suppl. Table
  group_by(temp, temp.min) %>% 
  mutate(temp = factor(temp, levels = c(20, 50, 100, 150, 200, 250))) %>%  # Reorder
  summarize(#n = length(max_germ), 
            #mean_germ = mean(max_germ),
            #sd = round(sd(max_germ), 2), 
            #se = round((sd(max_germ)) / sqrt(length(max_germ)),1),
            median_germ = median(max_germ),
            #max = max(max_germ),
            #min = min(max_germ),
            lower_quartile = quantile(max_germ, 0.25),  # 25th percentile
            upper_quartile = quantile(max_germ, 0.75)   # 75th percentile
            )


write.xlsx(mean_pertreat, file = "grassmortality_stats_2024dec16.xlsx")

hist(max_perrep$max_germ)
shapiro.test(max_perrep$max_germ) # <- data: not normal
boxplot(max_germ ~ temp, max_perrep)

kruskal.test(max_germ ~ temp, data = max_perrep) 
pairwise.wilcox.test(max_perrep$max_germ, max_perrep$temp, p.adjust.method = "BH")
#       20      50      100     150     200   
#   50  0.0114  -       -       -       -     
#   100 0.1242  0.4738  -       -       -     
#   150 0.0010  0.0188  0.0114  -       -     
#   200 7.0e-06 7.0e-06 8.8e-06 0.0018  -     
#   250 3.2e-06 3.2e-06 3.2e-06 1.8e-05 0.0114
#   
#   20 a
#   50 b
#   100 ab
#   150 c
#   200 d
#   250 e


############ figures
ggplot(data = summary, 
       aes(x = temp, y = 100*(mean_germ/10), color = factor(min))) + 
  geom_point(size = 3, alpha = 0.8) +
 # geom_smooth(method = "lm", se = FALSE, aes(group = min, color = (factor(min)))) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, 
              aes(group = min, color = (factor(min)))) +  # Add polynomial regression line (degree = 2)
  ylim(0, 100)+
  xlim(0,250) +
  labs(x = "Temperature (C)", 
       y = "Germination (%)", 
       title = "Average BRDI germination by temperatue and time") +
  scale_color_manual(values = c("orange", "forestgreen", "navy") ,name = "Duration (minutes)") +  
  theme_bw() 


ggplot(data = summary, 
       aes(x = min, y = 100*(mean_germ/10), color = factor(temp))) + 
  geom_point(size = 3, alpha = 0.8) +
  # geom_smooth(method = "lm", se = FALSE, aes(group = min, color = (factor(min)))) +
  geom_smooth(method = "lm", se = TRUE,# formula = y ~ poly(x, 2), 
              aes(group = temp, color = (factor(temp)))) +  # Add polynomial regression line (degree = 2)
  ylim(0, 100)+
  xlim(0,15) +
  labs(x = "Exposure time (minutes)", 
       y = "Germination (%)", 
       title = "Average BRDI germination by exposure time and temperature") +
  scale_color_manual(values = c("blue", "purple", "darkgreen", "orange", "red", "black") ,name = "Temperature (C)") +  
  theme_bw() 


############ tables






