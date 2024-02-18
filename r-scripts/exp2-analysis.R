library(tidyverse)
library(effsize)
options(
  pillar.print_max = 500,
  pillar.width = Inf
)

# Import Data
full_data <- read_csv("../data/exp-data.csv")
sess_list <- c(15:17, 42:44) # Last 3 sessions
exp2 <- full_data %>%
  filter(exp == 2 & session %in% sess_list) %>%
  select(exp, subject, condition, session, pks_sub, pks_opt)

# Factor subject column
exp2$subject <- factor(exp2$subject)

# Reorder Condition Levels
exp2$condition <- factor(exp2$condition,
  levels = c("VI 4.75", "VI 1.7", "VI 35")
)

# Create phase factor
exp2$phase <- ifelse(exp2$session <= 17, "Phase 1", "Phase 2")

# Create order factor
birds <- c(1332, 43, 42, 6125, 1311)
exp2$order <- ifelse(exp2$subject %in% birds,
  "VI 4.75 / VI 1.7",
  "VI 4.75 / VI 35"
)

# Get choice proportions
exp2$cp <- exp2$pks_sub / (exp2$pks_sub + exp2$pks_opt)

# Subject Means
bird_means <- exp2 %>%
  group_by(order, subject, condition) %>%
  summarise(
    mean_cp = mean(cp)
  )

#Summary Stats Ignoring Order
bird_means %>%
  group_by(condition) %>%
  summarise(
    m = mean(mean_cp),
    s = sd(mean_cp)
  )

# Tests
# 4.75 vs 1.7
test_df <- droplevels(subset(bird_means, order == 'VI 4.75 / VI 1.7'))

t1 <- t.test(mean_cp ~ condition, paired = TRUE, data = test_df)
g1 <- cohen.d(mean_cp ~ condition | Subject(subject), 
        paired = TRUE,
        hedges.correction = TRUE,
        data = test_df)
t1
g1


# 4.75 vs 35
t2 <- test_df <- droplevels(subset(bird_means, order == 'VI 4.75 / VI 35'))

t2 <- t.test(mean_cp ~ condition, paired = TRUE, data = test_df)
g2 <- cohen.d(mean_cp ~ condition | Subject(subject), 
        paired = TRUE,
        hedges.correction = TRUE,
        data = test_df)
t2
g2

# 1.7 vs 35
test_df <- droplevels(subset(bird_means, condition != 'VI 4.75'))

t3 <- t.test(mean_cp ~ condition, data = test_df)
g3 <- cohen.d(mean_cp ~ condition, 
        hedges.correction = TRUE,
        data = test_df)
t3
g3

t1$p.value
t2$p.value
t3$p.value
p <- p.adjust(c(t1$p.value, t2$p.value, t3$p.value), method = 'holm')
round(p, 3)




