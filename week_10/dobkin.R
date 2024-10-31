# We are going to replicate Carpenter and Dobkin one the effects of minimum drikjning age on
# mortality - This is the exericse condicted by Philip Leppert. 

rm(list = ls()) # clear memory
# Replace this with whatever your directory is. 
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2024/code/private-repository1/week10")
library(pacman)
p_load(dplyr,ggplot2,rddtools,magrittr)

# Read in data
carpenter_dobkin_2009 <- read.csv("dobkin.csv")

# At first, we have to compute a dummy variable (threshold), indicating whether an indidivual is below or above the cutoff. The dummy is equal to zero for observations below and equal to one for observations aboev the cutoff of 21 years. Then I am specifiying a linear model with function lm() to regress all deaths per 100.000 (all) on the threshold dummy and the respondents’ age which is centered around the threshold value of age (21 years). This is done with function I() by substracting the cutoff from each age bin.
carpenter_dobkin_2009 %>% 
  ggplot(aes(x = agecell, y = all)) + 
  geom_point() +
  geom_vline(xintercept = 21, color = "#ff0091", linewidth = 1, linetype = "dashed") + 
  annotate("text", x = 20.4, y = 105, label = "Minimum Drinking Age") +
  labs(y = "Mortality rate (per 100.000)",
       x = "Age (binned)") +
  theme_classic(base_size = 14)

#At first, we have to compute a dummy variable (threshold), indicating whether an indidivual is below or above the cutoff. The dummy is equal to zero for observations below and equal to one for observations aboev the cutoff of 21 years. Then I am specifiying a linear model with function lm() to regress all deaths per 100.000 (all) on the threshold dummy and the respondents’ age which is centered around the threshold value of age (21 years). This is done with function I() by substracting the cutoff from each age bin.

lm_same_slope <- carpenter_dobkin_2009 %>% 
  mutate(threshold = ifelse(agecell >= 21, 1, 0)) %$% 
  lm(all ~ threshold + I(agecell - 21))
# Output of the model. 
summary(lm_same_slope) 

#There is an alternative approach by using R package rddtools which contains various functions related to applying the RDD. Within function rdd_reg_lm() I am using the argument slope = "same" to achieve the same result with the previous approach.

rdd_data(y = carpenter_dobkin_2009$all, 
         x = carpenter_dobkin_2009$agecell, 
         cutpoint = 21) %>% 
  rdd_reg_lm(slope = "same") %>% 
  summary()

# Do the same thing, but different slopes on each side. Do it by hand

lm_different_slope <- carpenter_dobkin_2009 %>%
  mutate(threshold = ifelse(agecell >= 21, 1, 0)) %$%
  lm(all ~ threshold + I(agecell - 21) + threshold:I(agecell - 21))
summary(lm_different_slope)

# You can achieve the same with the fancy package. 
rdd_data(y = carpenter_dobkin_2009$all, 
         x = carpenter_dobkin_2009$agecell, 
         cutpoint = 21) %>% 
  rdd_reg_lm(slope = "separate") %>% 
  summary()

# Now let's visualize this. 

carpenter_dobkin_2009 %>%
  select(agecell, all) %>%
  mutate(threshold = as.factor(ifelse(agecell >= 21, 1, 0))) %>%
  ggplot(aes(x = agecell, y = all, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = 21, color = "red",
             size = 1, linetype = "dashed") +
  labs(y = "Mortality rate (per 100.000)",
       x = "Age (binned)")+
  theme_classic(base_size = 14)

# Now quadratic by hand
lm_quadratic <- carpenter_dobkin_2009 %>% 
  mutate(threshold = ifelse(agecell >= 21, 1, 0)) %$% 
  lm(all ~ threshold + I(agecell - 21) + I((agecell -21)^2) + threshold:I(agecell - 21) +
       threshold:I((agecell - 21)^2))

summary(lm_quadratic)

# Now quadratic by fancy. 

rdd_data(y = carpenter_dobkin_2009$all, 
         x = carpenter_dobkin_2009$agecell, 
         cutpoint = 21) %>% 
  rdd_reg_lm(slope = "separate", order = 2) %>% 
  summary()

carpenter_dobkin_2009 %>%
  select(agecell, all) %>%
  mutate(threshold = as.factor(ifelse(agecell >= 21, 1, 0))) %>%
  ggplot(aes(x = agecell, y = all, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x ^ 2),
              se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = 21, color = "red",
             size = 1, linetype = "dashed") +
  labs(y = "Mortality rate (per 100.000)",
       x = "Age (binned)")+
  theme_classic(base_size = 14)

# Now let's limit the sample! Only folks between 20 and 22!
lm_sensitivity <- carpenter_dobkin_2009 %>%
  filter(agecell >= 20 & agecell <= 22) %>%
  mutate(threshold = ifelse(agecell >= 21, 1, 0)) %$%
  lm(all ~ threshold + I(agecell - 21) + threshold:I(agecell - 21))

summary(lm_sensitivity)

carpenter_dobkin_2009 %>%
  filter(agecell >= 20 & agecell <= 22) %>%
  select(agecell, all) %>%
  mutate(threshold = as.factor(ifelse(agecell >= 21, 1, 0))) %>%
  ggplot(aes(x = agecell, y = all, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = 21, color = "red",
             size = 1, linetype = "dashed") +
  labs(y = "Mortality rate (per 100.000)",
       x = "Age (binned)")+
theme_classic(base_size = 14)

# What happens if you use a third, fourth and fifth order polynomial? With and without the data limitation to 20-22?

