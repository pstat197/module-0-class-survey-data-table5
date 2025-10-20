library(tidyverse)
suppressPackageStartupMessages(library(patchwork))

background <- readr::read_csv("data/background-clean.csv") %>%
  mutate(
    updv.num  = stringr::str_trim(updv.num),
    updv.num  = factor(updv.num, levels = c("0-2","3-5","6-8","9+"), ordered = TRUE),
    updv_numeric = case_when(
      updv.num == "0-2" ~ 1,
      updv.num == "3-5" ~ 4,
      updv.num == "6-8" ~ 7,
      updv.num == "9+"  ~ 10,
      TRUE ~ NA_real_
    ),
    prog.prof = factor(prog.prof, levels = c("beg","int","adv"), ordered = TRUE),
    math.prof = factor(math.prof, levels = c("beg","int","adv"), ordered = TRUE),
    stat.prof = factor(stat.prof, levels = c("beg","int","adv"), ordered = TRUE)
  ) %>%
  drop_na(updv_numeric, prog.prof, math.prof, stat.prof)

out_dir <- "report_files"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

p1 <- ggplot(background, aes(x = prog.prof, y = updv_numeric, fill = prog.prof)) +
  geom_boxplot() +
  labs(title = "Upper-division (approx) vs Programming proficiency",
       x = "Programming proficiency (beg/int/adv)",
       y = "Upper-division courses (midpoint of bin)") +
  theme_minimal() + theme(legend.position = "none")

p2 <- ggplot(background, aes(x = math.prof, y = updv_numeric, fill = math.prof)) +
  geom_boxplot() +
  labs(title = "Upper-division (approx) vs Math proficiency",
       x = "Math proficiency (beg/int/adv)",
       y = "Upper-division courses (midpoint of bin)") +
  theme_minimal() + theme(legend.position = "none")

p3 <- ggplot(background, aes(x = stat.prof, y = updv_numeric, fill = stat.prof)) +
  geom_boxplot() +
  labs(title = "Upper-division (approx) vs Statistics proficiency",
       x = "Statistics proficiency (beg/int/adv)",
       y = "Upper-division courses (midpoint of bin)") +
  theme_minimal() + theme(legend.position = "none")

ggsave(file.path(out_dir, "box_updv_vs_prog.png"), p1, width = 7, height = 5, dpi = 300)
ggsave(file.path(out_dir, "box_updv_vs_math.png"), p2, width = 7, height = 5, dpi = 300)
ggsave(file.path(out_dir, "box_updv_vs_stat.png"), p3, width = 7, height = 5, dpi = 300)

p_combined <- p1 | p2 | p3
ggsave(file.path(out_dir, "box_updv_all_three.png"), p_combined, width = 14, height = 5, dpi = 300)

cat("\nSaved boxplots to:", normalizePath(out_dir), "\n")

run_chisq <- function(var_name) {
  tab <- table(background[[var_name]], background$updv.num)
  cs  <- chisq.test(tab, correct = FALSE)
  cv  <- effectsize::cramers_v(tab, ci = 0.95)
  cat("\n====", var_name, "vs updv.num ====\n")
  print(tab)
  cat("\nChi-square test:\n"); print(cs)
  cat("\nCramer's V (effect size):\n"); print(cv)
}

run_chisq("prog.prof")
run_chisq("math.prof")
run_chisq("stat.prof")