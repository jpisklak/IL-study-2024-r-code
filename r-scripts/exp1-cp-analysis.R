library(tidyverse)
options(
  pillar.print_max = 500,
  pillar.width = 50
)
library(nlme)
library(rsq)

# Import Data
full_data <- read_csv('../data/exp-data.csv')
sess_list <- c(14:16, 44:46) # Last 3 sessions

exp1 <- full_data %>%
  filter(exp == 1 & session %in% sess_list) %>%
  select(exp, subject, condition, session, pks_sub, pks_opt)

# Create order column
# Order A = FR 1 / VI 30
# Order B = VI 30 / FR 1
exp1$order <- ifelse(((exp1$session <= 16 & exp1$condition == "FR 1") |
  (exp1$session > 16 & exp1$condition == "VI 30")),
"A", "B"
)

# Create phase column
exp1$phase <- ifelse(exp1$session <= 16, 'Phase 1', 'Phase 2')

#Get choice proportions
exp1$cp <- exp1$pks_sub / (exp1$pks_sub + exp1$pks_opt)

# Subject Means
bird_means <- exp1 %>%
  group_by(order, subject, condition) %>%
  summarise(
    mean_cp = mean(cp)
  )

#Factor
bird_means$subject <- factor(bird_means$subject)
bird_means$order <- factor(bird_means$order)
bird_means$condition <- factor(bird_means$condition)

# Summary Stats
sum_stats <- bird_means %>%
  group_by(order, condition) %>%
  summarise(
    n = length(mean_cp),
    cp = mean(mean_cp)
  )

# Summary Stats ignoring order
bird_means %>%
  group_by(condition) %>%
  summarise(
    n = length(mean_cp),
    cp = mean(mean_cp),
    s = sd(mean_cp)
  )


# Contrasts
levels(bird_means$order)
A_vs_B <- c(1, -1)
contrasts(bird_means$order) <- cbind(A_vs_B)

levels(bird_means$condition)
FR1_vs_FR30 <- c(1, -1)
contrasts(bird_means$condition) <- cbind(FR1_vs_FR30)

# Multi-level model
base_mod <- lme(mean_cp ~ 1, 
                random = ~1 | subject,
                method = 'ML',
                data = bird_means)

sched_mod <- update(base_mod, .~. + condition)

order_sched_mod <- update(sched_mod, .~. + order)

int_mod <- update(order_sched_mod, .~. + condition : order)
#plot(int_mod)

results <- anova(base_mod, sched_mod, order_sched_mod, int_mod)
results


#Inverse Bayes Factor
delta_BIC <- results$BIC[2:4] - results$BIC[1:3]
BF01 <- exp(delta_BIC / 2)
BF10 <- 1 / BF01
round(BF10, 3)

#Effect Size (delta R2)
  # Effect of schedule
round(rsq.lmm(sched_mod)$model - rsq.lmm(base_mod)$model, 3)

  #Effect of adding order
round(rsq.lmm(order_sched_mod)$model - rsq.lmm(sched_mod)$model, 3)

  #Effect of adding interaction
round(rsq.lmm(int_mod)$model - rsq.lmm(order_sched_mod)$model, 3)







