library(knitr)
library(kableExtra)
source('all-first-pk-analysis.R')
options(knitr.kable.NA = '')

# LaTeX Tables
#-------------------------------------------------------------------------------
# Table 1
tab_1 <- bird_stats %>%
  filter(condition %in% c('VI 30', 'VI 35')) %>%
  pivot_wider(names_from = c(exp, condition),
              values_from = c(cp_first_pk, cp_full)) %>%
  rename(Bird = subject,
         `First Peck VI 30` = `cp_first_pk_1_VI 30`,
         `First Peck VI 35` = `cp_first_pk_2_VI 35`,
         `Overall VI 30` = `cp_full_1_VI 30`,
         `Overall VI 35` = `cp_full_2_VI 35`) %>%
  select(Bird,
         `First Peck VI 30`,
         `Overall VI 30`,
         `First Peck VI 35`,
         `Overall VI 35`)


tab_1$Bird <- as.character(tab_1$Bird)

means <- tibble(
  Bird = "Mean",
  `First Peck VI 30` = mean(tab_1$`First Peck VI 30`, na.rm = TRUE),
  `Overall VI 30` = mean(tab_1$`Overall VI 30`, na.rm = TRUE),
  `First Peck VI 35` = mean(tab_1$`First Peck VI 35`, na.rm = TRUE),
  `Overall VI 35` = mean(tab_1$`Overall VI 35`, na.rm = TRUE)
)

tab_1 <- rbind(tab_1, means)

sink("../LaTeX-tables/LaTeX-tab-1.txt")
tab_1 %>%
  kable(format = 'latex', booktabs = TRUE, digits = 2) %>%
  add_header_above(header = c(" " = 1,
                              "Exp. 1 VI 30" = 2,
                              "Exp. 2 VI 35" = 2)) %>%
  row_spec(7, hline_after = TRUE)
sink()


#Appendix: Exp 1 First Peck CP
append_tab_1 <- bird_stats %>%
  filter(exp == 1) %>%
  select(-cp_full)

append_tab_1 <- append_tab_1[2:4] 

append_tab_1 <- append_tab_1 %>% 
  rename(Bird = subject) %>%
  pivot_wider(names_from = condition, values_from = cp_first_pk)

append_tab_1$Bird <- as.character(append_tab_1$Bird)

means <- tibble(
  Bird = "Mean",
  `FR 1` = mean(append_tab_1$`FR 1`, na.rm = TRUE),
  `VI 30` = mean(append_tab_1$`VI 30`, na.rm = TRUE)
)

append_tab_1 <- rbind(append_tab_1, means)

sink("../LaTeX-tables/LaTeX-append-tab-1.txt")
append_tab_1 %>%
  kable(format = 'latex', booktabs = TRUE, digits = 2) %>%
  add_header_above(header = c(" " = 1, "Initial-Link Schedule" = 2)) %>%
  row_spec(5, hline_after = TRUE)
sink()


#Appendix: Exp 2 First Peck CP
append_tab_2 <- bird_stats %>%
  filter(exp == 2) %>%
  select(-cp_full)

append_tab_2 <- append_tab_2[2:4]

append_tab_2 <- append_tab_2 %>% 
  rename(Bird = subject) %>%
  pivot_wider(names_from = condition, values_from = cp_first_pk)

append_tab_2$Bird <- as.character(append_tab_2$Bird)

means <- tibble(
  Bird = "Mean",
  `VI 35` = mean(append_tab_2$`VI 35`, na.rm = TRUE),
  `VI 4.75` = mean(append_tab_2$`VI 4.75`, na.rm = TRUE),
  `VI 1.7` = mean(append_tab_2$`VI 1.7`, na.rm = TRUE)
)

append_tab_2 <- rbind(append_tab_2, means)

append_tab_2 <- select(append_tab_2, Bird, `VI 4.75`, `VI 35`, `VI 1.7`)

sink("../LaTeX-tables/LaTeX-append-tab-2.txt")
append_tab_2 %>%
  kable(format = "latex", booktabs = TRUE, digits = 2) %>%
  add_header_above(header = c(" " = 1, "Initial-Link Schedule" = 3)) %>%
  row_spec(10, hline_after = TRUE)
sink()


