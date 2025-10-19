library(tidyverse)

# Convert proficiency to numeric 
df <- df %>%
  mutate(
    prog.prof.num = case_when(
      prog.prof == "beg" ~ 1,
      prog.prof == "int" ~ 2,
      prog.prof == "adv" ~ 3
    ),
    math.prof.num = case_when(
      math.prof == "beg" ~ 1,
      math.prof == "int" ~ 2,
      math.prof == "adv" ~ 3
    ),
    stat.prof.num = case_when(
      stat.prof == "beg" ~ 1,
      stat.prof == "int" ~ 2,
      stat.prof == "adv" ~ 3
    )
  )

# lms
lm_prog <- lm(prog.comf ~ prog.prof.num, data = df)
lm_math <- lm(math.comf ~ math.prof.num, data = df)
lm_stat <- lm(stat.comf ~ stat.prof.num, data = df)

# summaries
summary(lm_prog)
summary(lm_math)
summary(lm_stat)

# clean result
lm_results <- tibble(
  domain = c("Programming", "Mathematics", "Statistics"),
  intercept = c(coef(lm_prog)[1], coef(lm_math)[1], coef(lm_stat)[1]),
  slope = c(coef(lm_prog)[2], coef(lm_math)[2], coef(lm_stat)[2]),
  r_squared = c(summary(lm_prog)$r.squared,
                summary(lm_math)$r.squared,
                summary(lm_stat)$r.squared)
)

print(lm_results)

# visualization
plot_data <- bind_rows(
  df %>% select(prof = prog.prof.num, comf = prog.comf) %>% mutate(domain = "Programming"),
  df %>% select(prof = math.prof.num, comf = math.comf) %>% mutate(domain = "Mathematics"),
  df %>% select(prof = stat.prof.num, comf = stat.comf) %>% mutate(domain = "Statistics")
)

ggplot(plot_data, aes(x = prof, y = comf)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  facet_wrap(~ domain, scales = "free") +
  labs(
    x = "Self-rated Proficiency (1=beg, 2=int, 3=adv)",
    y = "Comfort Level (1-5)",
    title = "Linear Regression of Comfort on Proficiency by Domain"
  ) +
  theme_minimal()



# cor matrix
prof_vars <- df %>% select(prog.prof.num, math.prof.num, stat.prof.num)
comf_vars <- df %>% select(prog.comf, math.comf, stat.comf)

# compute correlations
cor_matrix <- cor(prof_vars, comf_vars, use = "complete.obs")

# convert to long format for ggplot
cor_long <- as.data.frame(cor_matrix) %>%
  rownames_to_column(var = "Proficiency") %>%
  pivot_longer(
    cols = -Proficiency,
    names_to = "Comfort",
    values_to = "Correlation"
  )

# heatmap (alternate way of visualizing)
ggplot(cor_long, aes(x = Comfort, y = Proficiency, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1, 1)) +
  labs(
    title = "Correlation Between Proficiency and Comfort by Domain",
    x = "Comfort",
    y = "Proficiency"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #experimenting with dimensional variables







