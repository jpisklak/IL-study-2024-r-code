library(tidyverse)
library(effsize)
options(
  pillar.print_max = 500,
  pillar.width = Inf
)

# Import Data
full_data <- read.csv("../data/exp-data.csv")
sess_list_exp1 <- c(14:16, 44:46) # Last 3 sessions
sess_list_exp2 <- c(15:17, 42:44) # Last 3 sessions

exp1 <- full_data %>%
  filter(exp == 1 & session %in% sess_list_exp1)
exp2 <- full_data %>%
  filter(exp == 2 & session %in% sess_list_exp2)

exp <- rbind(exp1, exp2)

exp <- exp %>%
  select(
    exp,
    subject,
    condition,
    session,
    pks_sub,
    pks_opt,
    pk1_sub,
    pk1_opt
  )

# Merge FR1 into "First Peck" column
exp$pk1_sub <- ifelse(exp$condition == "FR 1", exp$pks_sub, exp$pk1_sub)
exp$pk1_opt <- ifelse(exp$condition == "FR 1", exp$pks_opt, exp$pk1_opt)

# Calculate choice proportions
exp$cp_full <- exp$pks_sub / (exp$pks_sub + exp$pks_opt)
exp$cp_first_pk <- exp$pk1_sub / (exp$pk1_sub + exp$pk1_opt)

# Remove birds with no first peck data (i.e., Exp 1 VI 30 first phase)
`%!in%` = Negate(`%in%`)
birds_pk1_miss <- unique(filter(exp, is.na(cp_first_pk))$subject)
exp <- exp %>%
  filter(
    !(exp == 1 & subject %in% birds_pk1_miss)
  )

# Bird Stats
bird_stats <- exp %>%
  group_by(exp, subject, condition) %>%
  summarise(
    cp_full = mean(cp_full),
    cp_first_pk = mean(cp_first_pk),
  )

# Summary Stats (Exp 1 and 2)
sum_stats <- bird_stats %>%
  group_by(exp, condition) %>%
  summarise(
    full_cp = mean(cp_full, na.rm = TRUE),
    full_sd = sd(cp_full, na.rm = TRUE),
    first_pk_cp = mean(cp_first_pk, na.rm = TRUE),
    first_pk_sd = sd(cp_first_pk, na.rm = TRUE),
  )

sum_stats


# Appendix Exp 1 First Peck Analysis
#-------------------------------------------------------------------------------

# Isolate Exp 1 Birds
exp1_pk1 <- bird_stats %>% filter(exp == 1)

# Factor subject column
exp1_pk1$subject <- factor(exp1_pk1$subject)

# Factor Condition Levels
exp1_pk1$condition <- factor(exp1_pk1$condition,
  levels = c("FR 1", "VI 30")
)

# t-test FR1 vs VI 30
t.test(cp_first_pk ~ condition, paired = TRUE, data = exp1_pk1)
cohen.d(cp_first_pk ~ condition | Subject(subject),
  paired = TRUE,
  hedges.correction = TRUE,
  data = exp1_pk1
)



# Appendix Exp 2 First Peck Analysis
#-------------------------------------------------------------------------------

# Isolate Exp 2 Birds
exp2_pk1 <- bird_stats %>% filter(exp == 2)

# Factor subject column
exp2_pk1$subject <- factor(exp2_pk1$subject)

# Factor Condition Levels
exp2_pk1$condition <- factor(exp2_pk1$condition,
  levels = c("VI 4.75", "VI 1.7", "VI 35")
)

# Create order factor
birds <- c(1332, 43, 42, 6125, 1311)
exp2_pk1$order <- ifelse(exp2_pk1$subject %in% birds,
  "VI 4.75 / VI 1.7",
  "VI 4.75 / VI 35"
)

# Summary Stats
exp2_pk1 %>%
  group_by(condition) %>%
  summarise(
    m_full = mean(cp_full),
    m_pk1 = mean(cp_first_pk),
    s_full = sd(cp_full),
    s_pk1 = sd(cp_first_pk)
  )


# Tests
# 4.75 vs 1.7
test_df <- droplevels(subset(exp2_pk1, order == "VI 4.75 / VI 1.7"))

t1 <- t.test(cp_first_pk ~ condition, paired = TRUE, data = test_df)
g1 <- cohen.d(cp_first_pk ~ condition | Subject(subject),
  paired = TRUE,
  hedges.correction = TRUE,
  data = test_df
)
t1
g1


# 4.75 vs 35
test_df <- droplevels(subset(exp2_pk1, order == "VI 4.75 / VI 35"))

t2 <- t.test(cp_first_pk ~ condition, paired = TRUE, data = test_df)
g2 <- cohen.d(cp_first_pk ~ condition | Subject(subject),
  paired = TRUE,
  hedges.correction = TRUE,
  data = test_df
)
t2
g2


# 4.75 vs 35
test_df <- droplevels(subset(exp2_pk1, condition != "VI 4.75"))

t3 <- t.test(cp_first_pk ~ condition, data = test_df)
g3 <- cohen.d(cp_first_pk ~ condition,
  hedges.correction = TRUE,
  data = test_df
)
t3
g3

t1$p.value
t2$p.value
t3$p.value
p <- p.adjust(c(t1$p.value, t2$p.value, t3$p.value), method = "holm")
round(p, 3)
