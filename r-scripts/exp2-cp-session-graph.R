library(tidyverse)
library(RColorBrewer)
source("theme-custom.R")
options(
  pillar.print_max = 500,
  pillar.width = 50
)

# Import Data
full_data <- read_csv("../data/exp-data.csv")
exp2 <- full_data %>%
  filter(exp == 2) %>%
  select(exp, subject, condition, session, pks_sub, pks_opt)

#Factor subject column
exp2$subject <- factor(exp2$subject)

#Reorder Condition Levels
exp2$condition <- factor(exp2$condition,
  levels = c("VI 4.75", "VI 1.7", "VI 35")
)

# Create phase factor
exp2$phase <- ifelse(exp2$session <= 17, "Phase 1", "Phase 2")

#Get choice proportions
exp2$cp <- exp2$pks_sub / (exp2$pks_sub + exp2$pks_opt)

# Remove Missing Values
exp2 <- exp2[-which(is.na(exp2$cp)), ]

# Session CPs
plt_means <- exp2 %>%
  group_by(condition, phase, session) %>%
  summarise(
    mean_cp = mean(cp)
    )

#Plot
ggplot(plt_means, aes(x = session, y = mean_cp)) +
  geom_hline(
    yintercept = 0.5,
    linetype = 3,
    linewidth = 0.5
  ) +
  
  geom_line(
    data = exp2,
    aes(
      x = session,
      y = cp,
      group = subject,
      colour = condition
    ),
    alpha = 0.30
  ) +
  
  geom_line(aes(colour = condition),
    linewidth = 1.5
  ) +
  
  geom_point(
    aes(
      colour = condition,
      shape = condition
    ),
    size = 4,
    stroke = 2,
    fill = "white"
  ) +
  
  scale_colour_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  scale_shape_manual(values = c(21, 24, 22)) +
  scale_x_continuous(breaks = seq(0, 44, 5)) +
  xlab("Session") +
  ylab("Suboptimal Choice Proportion") +
  labs(
    colour = "Schedule:",
    linewidth = "Schedule:",
    shape = "Schedule:"
  ) +
  
  facet_grid(~phase, scales = "free_x", space = "free") +
  
  theme_custom() +
  theme(
    legend.position = "bottom",
    legend.box.background = element_rect(colour = "white")
  )

# Save
ggsave('../plots/exp2_session_means.png', 
       units = 'cm', width = 40, height = 22, 
       dpi = 500)

ggsave('../plots/exp2_session_means.svg', 
       units = 'cm', width = 40, height = 22)

ggsave('../plots/exp2_session_means.pdf', 
       units = 'cm', width = 40, height = 22)