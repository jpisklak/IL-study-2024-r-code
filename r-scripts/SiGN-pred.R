library(tidyverse)
library(RColorBrewer)
source("theme-custom.R")
source("SiGN-function.R")
options(
  pillar.print_max = 500,
  pillar.width = 50
)

#Obtained Vales
#-------------------------------------------------------

# Import Data
full_data <- read_csv('../data/exp-data.csv')
sess_list1 <- c(14:16, 44:46) # Last 3 sessions
sess_list2 <- c(15:17, 42:44) # Last 3 sessions

exp <- full_data %>% filter(
  (exp == 1 & session %in% sess_list1) |
  (exp == 2 & session %in% sess_list2)) %>%
  select(exp, subject, condition, session, pks_sub, pks_opt)

# Get choice proportions
exp$cp <- exp$pks_sub / (exp$pks_sub + exp$pks_opt)

# Subject Means
bird_means <- exp %>%
  group_by(subject, condition) %>%
  summarise(
    mean_cp = mean(cp)
  )

# Factor
bird_means$subject <- factor(bird_means$subject)
bird_means$condition <- factor(bird_means$condition)

# Summary Stats
sum_stats <- bird_means %>%
  group_by(condition) %>%
  summarise(
    n = length(mean_cp),
    cp = mean(mean_cp)
  )

sum_stats



# Model Parameters
#-------------------------------------------------------
ILa_dur <- c(1, 30, 1.7, 4.75, 35)
ILb_dur <- c(1, 30, 1.7, 4.75, 35)

# Terminal Link Duration
TLa1_dur <- c(20, 20, 8, 8, 8)
TLa2_dur <- c(20, 20, 8, 8, 8)

TLb1_dur <- c(20, 20, 8, 8, 8)
TLb2_dur <- c(20, 20, 8, 8, 8)

# Terminal Link Entry Probability
TLa1_p <- c(0.2, 0.2, 0.2, 0.2, 0.2)
TLa2_p <- 1 - TLa1_p

TLb1_p <- c(0.5, 0.5, 0.5, 0.5, 0.5)
TLb2_p <- 1 - TLb1_p

# Terminal Link Reinforcement Probability
TLa1_rp <- c(1, 1, 1, 1, 1)
TLa2_rp <- c(0, 0, 0, 0, 0)

TLb1_rp <- c(0.5, 0.5, 0.5, 0.5, 0.5)
TLb2_rp <- c(0.5, 0.5, 0.5, 0.5, 0.5)

# Initial Link Schedules
Sched_IL_a <- c("FR", "FR", "FR", "FR", "FR")
Sched_IL_b <- c("FR", "FR", "FR", "FR", "FR")
# Schedules are set to FR because a single timer (not concurrent timers) were used.

# S-delta (extinction stimulus duration; a.k.a. the 'S-' for you Pavlov people)
Sdelt_dur <- c(1, 1, 1, 1, 1)

# Use Beta:
# B = FALSE: Do Not Use Beta
# B = TRUE: Use Beta

B <- TRUE
useBeta <- rep(B, length(Sdelt_dur))

# How Many decimal places should the output be rounded to?
decimals <- 3

# Calculate Predictions
preds <- SiGN(
  ILa_dur, ILb_dur,
  TLa1_dur, TLa2_dur,
  TLb1_dur, TLb2_dur,
  TLa1_p, TLa2_p,
  TLb1_p, TLb2_p,
  TLa1_rp, TLa2_rp,
  TLb1_rp, TLb2_rp,
  Sched_IL_a, Sched_IL_b,
  Sdelt_dur,
  useBeta
)

preds$condition <- c("FR 1", "VI 30", "VI 1.7", "VI 4.75", "VI 35")

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == "numeric"
  x[numeric_columns] <- round(x[numeric_columns], digits)
  x
} # https://stackoverflow.com/a/29876220

round_df(preds, decimals)


sum_stats <- sum_stats %>%
  mutate(SiGN = case_when(
    condition == "FR 1" ~ preds$SiGN_cp[preds$condition == "FR 1"],
    condition == "VI 30" ~ preds$SiGN_cp[preds$condition == "VI 30"],
    condition == "VI 4.75" ~ preds$SiGN_cp[preds$condition == "VI 4.75"],
    condition == "VI 1.7" ~ preds$SiGN_cp[preds$condition == "VI 1.7"],
    condition == "VI 35" ~ preds$SiGN_cp[preds$condition == "VI 35"]
  ))

sum_stats$condition <- factor(sum_stats$condition,
  levels = c(
    "FR 1",
    "VI 1.7",
    "VI 4.75",
    "VI 30",
    "VI 35"
  )
)

write_csv(sum_stats, "../data/predictions.csv")

# Plot
#-------------------------------------------------------
# Regression for plot
reg <- lm(cp ~ SiGN, data = sum_stats)
summary(reg)$r.squared

# Plot Annotations
LinEq <- paste(
  "'y = '~", round(reg$coefficients[2], 2),
  "~'x'~", round(reg$coefficients[1], 2)
)

Rsq <- paste("~R^2~'='~", round(summary(reg)$r.squared, 2))

#Scatter Plot
ggplot(sum_stats, aes(x = SiGN, y = cp)) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = 3 
  ) +
  
  geom_point(
    size = 11,
    stroke = 3,
    aes(
      shape = condition,
      fill = condition
    )
  ) +
  
  scale_shape_manual(values = c(21:25)) +
  guides(shape = guide_legend(override.aes = list(size = 7.5),
                              position = 'inside')) +
  scale_fill_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  
  geom_abline(
    intercept = reg$coefficients[1],
    slope = reg$coefficients[2],
    colour = "black",
    linewidth = 2
  ) +
  
  annotate("text",
    x = 0.2, y = 0.9,
    size = 12,
    label = LinEq,
    parse = TRUE
  ) +
  
  annotate("text",
    x = 0.2, y = 0.8,
    size = 12,
    label = Rsq,
    parse = TRUE
  ) +
  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    shape = "Schedule",
    fill = "Schedule"
  ) +
  xlab("Predicted Choice Proportion") +
  ylab("Obtained Choice Proportion") +
  #guides(shape = guide_legend(position = 'inside')) +
  
  theme_custom() +
  theme(
    legend.position.inside = c(0.85, 0.30),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(1.5, "cm"),
    legend.background = element_rect(
      colour = "black",
      linewidth = 0.5
    )
  )


# Save
ggsave('../plots/SiGN-preds.png', 
       units = 'cm', width = 30, height = 22, 
       dpi = 500)

ggsave('../plots/SiGN-preds.svg', 
       units = 'cm', width = 30, height = 22)

ggsave('../plots/SiGN-preds.pdf', 
       units = 'cm', width = 30, height = 22)

































