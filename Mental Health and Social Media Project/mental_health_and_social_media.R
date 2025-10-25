library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)

install.packages("readxl")
library(readxl)

df2 <- read_excel("mental_health_survey.xlsx", sheet = 1)  

aim <- df2 %>% 
  select(c(`7. How much time do you spend daily in social media?`,`13. What contents do you mainly look for in your social media news feed?`)) %>% 
  rename(c(time_spent = `7. How much time do you spend daily in social media?`,contents = `13. What contents do you mainly look for in your social media news feed?`)) %>% 
  group_by(time_spent,contents) %>% 
  summarise( n = n()) %>% 
  filter(n>5) %>% 
  group_by(time_spent) %>%
  mutate(percent = n / sum(n) * 100)

aim$time_spent <- factor(aim$time_spent, levels = c("Less than 1 hour", "1-3 hours", "3-5 hours", "More than 5 hours"))
  

aim %>% 
  ggplot(aes(x = time_spent, y = percent, fill = contents)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "What contents do you mainly look for in your social media news feed?",
       x = "Time users usually spend on social media",
       y = "Amount of users",
       fill = "Answers") +
  theme_minimal()



social_good <- df2 %>% 
  select(c(`7. How much time do you spend daily in social media?`,`14.Do you believe social media is a good thing?`)) %>% 
  rename(c(time_spent = `7. How much time do you spend daily in social media?`, answer = `14.Do you believe social media is a good thing?`) ) %>% 
  group_by(time_spent, answer) %>% 
  summarise(n = n()) %>% 
  group_by(time_spent) %>%
  mutate(percent = n / sum(n) * 100)

social_good$time_spent <- factor(social_good$time_spent, levels = c("Less than 1 hour", "1-3 hours", "3-5 hours", "More than 5 hours"))

ggplot(social_good,aes(x = time_spent, y = percent, fill = answer)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Do you think social media is a good thing?",
       x = "Time spent",
       y = "Amount of users",
       fill = "Answer") +
  theme_minimal()

df2$`17. Does your emotion get influenced by other's posts (success, failure, loss)?`

influence <- df2 %>% 
  select(c(`7. How much time do you spend daily in social media?`,`17. Does your emotion get influenced by other's posts (success, failure, loss)?`)) %>% 
  rename(c(time_spent = `7. How much time do you spend daily in social media?`, answer = `17. Does your emotion get influenced by other's posts (success, failure, loss)?`) ) %>% 
  group_by(time_spent, answer) %>% 
  summarise(n = n()) %>% 
  filter(answer %in% c('Always', 'Not at all')) %>% 
  group_by(time_spent) %>%
  mutate(percent = n / sum(n) * 100) 
influence
  
install.packages('showtext')
library(showtext)
font_add_google("Montserrat")
showtext_auto()

influence$time_spent <- factor(influence$time_spent, levels = c("Less than 1 hour", "1-3 hours", "3-5 hours", "More than 5 hours"))

influence_plot <- ggplot(influence,aes(x = time_spent, y = percent, fill = answer)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Does your emotion get influenced by 
       other's posts (success, failure, loss)?",
       x = "Time spent on social media",
       y = "Percent of users",
       fill = "Answer") +
  theme_minimal() + theme(
    text = element_text(family = "Montserrat", color = "white"),  # Kolor tekstu na biały
    plot.title = element_text(size = 25, face = "bold", color = "white"),
    axis.title = element_text(size = 20, color = "white"),
    axis.text = element_text(size = 17, color = "white"),
    legend.title = element_text(size = 17, color = "white"),
    legend.text = element_text(size = 14, color = "white"),
    panel.background = element_blank(),         # Usunięcie tła panelu
    plot.background = element_blank(),          # Usunięcie tła wykresu
    legend.background = element_blank() # Zmiana rozmiaru tekstu osi
  ) + scale_fill_manual(values = c("Always" = "#ff66c4", "Not at all" = "#6891d3"))

influence_plot
ggsave("plot.png", plot = influence_plot, bg = "transparent")
influence_pivot <- influence %>% 
  select(-n) %>% 
  pivot_wider(names_from = answer, values_from = percent)
write.csv(influence_pivot, "influence_p.csv", row.names = F)
