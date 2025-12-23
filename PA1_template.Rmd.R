install.packages("tidyverse")
library(tidyverse)


activity <- read_csv("activity.csv")
str(activity)
spec(activity)

daily_steps_total <- activity %>%
  group_by(date) %>%
  summarise(sum_total_steps = sum(steps))

hist(daily_steps_total$sum_total_steps,
     main = "Total number of steps per day",
     xlab = "Number of Steps",
     ylab = "Frequency",
     )

daily_steps_mean_median <- daily_steps_total %>%
  group_by(date) %>%
  summarise(mean_total_steps = mean(sum_total_steps), median_total_steps = median(sum_total_steps))

head(daily_steps_mean_median)

interval_mean <- activity %>%
  group_by(interval) %>%
  summarise(inetrval_mean = mean(steps))

interval_mean <- activity %>%
  group_by(interval) %>%
  summarise(interval_mean = mean(steps))

interval_mean <- activity %>%
  group_by(interval) %>%
  summarise(interval_mean = mean(steps, na.rm = TRUE))

ggplot(interval_mean, aes(x = interval, y = interval_mean)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Daily Activity Pattern",
       x = "Interval",
       y = "Mean number of steps per interval") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

interval_max <- activity %>%
  group_by(interval) %>%
  summarise(interval_max = max(steps, na.rm = TRUE))

ggplot(interval_max, aes(x = interval, y = interval_max)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Daily Activity Pattern",
       x = "Interval",
       y = "Max number of steps per interval") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

interval_max <- activity %>%
  group_by(interval) %>%
  summarise(max_steps = max(steps, na.rm = TRUE)) %>%
  arrange(desc(max_steps))

head(interval_max)

summary(activity)

colSums(is.na(activity))


activity_imputed <- activity %>%
  group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps),
                        mean(steps, na.rm = TRUE),
                        steps))

colSums(is.na(activity_imputed))

activity_imputed <- activity_imputed %>%
  mutate(date, weekdays(date)) %>%
  mutate(weekdays = case_when(`weekdays(date)` %in% c("Saturday", "Sunday") ~ "Weekend",TRUE ~ "Weekday"))

activity_imputed_averaged <- activity_imputed %>%
  group_by(interval, weekdays) %>%
  summarise(mean_steps = mean(steps))

ggplot(activity_imputed_averaged, aes(x = interval, y = mean_steps)) +
  geom_line(color = "blue", size = 1) +
  facet_wrap(~ weekdays) +   
  labs(title = "Weekday v Weekend Activity Pattern",
       x = "Interval",
       y = "Mean number of steps per interval") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))