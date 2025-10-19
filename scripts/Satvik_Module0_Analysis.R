# =========================================================
# Author: Satvik Talchuru
# Research Question: "How do project type (industry vs. lab) and language 
# (Python vs. R) preferences vary by prior research experience?"
# =========================================================

# Required Libraries
library(tidyverse) 
library(janitor)    
library(forcats)   
library(broom) 

# Load Data
background <- read_csv("background-clean.csv") %>% clean_names()
interest   <- read_csv("interest-clean.csv")   %>% clean_names()
metadata   <- read_csv("survey-metadata.csv")  %>% clean_names()

glimpse(background)
glimpse(interest)
glimpse(metadata)

# Merge Datasets on Response ID
merged <- background %>%
  inner_join(interest, by = "response_id")

# Select variables ~ similar to what my teammate did
merged <- merged %>%
  select(response_id,
         prior_research = rsrch,
         preferred_project = type,
         preferred_language = lang) %>%
  mutate(
    prior_research = ifelse(prior_research == TRUE, "Yes", "No"), # Convert logical TRUE/FALSE to Yes/No for readability
    preferred_project = case_when(
      preferred_project == "ind" ~ "Industry",
      preferred_project == "lab" ~ "Lab",
      preferred_project == "both" ~ "Both",
      TRUE ~ NA_character_
    ),
    preferred_language_simplified = case_when(
      str_detect(preferred_language, regex("python", ignore_case = TRUE)) ~ "Python",
      str_detect(preferred_language, regex("(^|\\b)r(\\b|$)", ignore_case = TRUE)) ~ "R",
      str_detect(preferred_language, regex("no\\s*pref", ignore_case = TRUE)) ~ "No Preference",
      TRUE ~ "Other / Mixed"
    )
  ) %>%
  drop_na(prior_research, preferred_project, preferred_language_simplified)

# Summary Tables: Project and Programming Language
proj_tbl <- merged %>%
  count(prior_research, preferred_project, name = "n") %>%
  group_by(prior_research) %>%
  mutate(pct = n / sum(n),
         label = scales::percent(pct, accuracy = 1)) %>%
  ungroup()

lang_tbl <- merged %>%
  count(prior_research, preferred_language_simplified, name = "n") %>%
  group_by(prior_research) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# Heatmap showing project preferences differing between research experience groups
ggplot(proj_tbl, aes(x = prior_research, y = preferred_project, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label)) +
  scale_fill_continuous(labels = scales::percent) +
  labs(title = "Project Preference by Research Experience (Heatmap)",
       x = "Prior Research Experience", y = "Project Type", fill = "Proportion") +
  theme_minimal()

# Horizontal bar chart showing proportional breakdown of project preferences
ggplot(proj_tbl, aes(y = prior_research, x = pct, fill = preferred_project)) +
  geom_col(position = "fill", color = "white") +
  geom_text(aes(label = percent(pct, accuracy = 1)),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "Project Type Preferences by Research Experience",
    x = "Proportion",
    y = "Research Experience",
    fill = "Project Type"
  )

# Statistical Test 1: Two-Proportion Z-Test (Project Type)
proj_bin <- merged %>%
  filter(preferred_project %in% c("Industry", "Lab"))
proj_tab <- table(proj_bin$prior_research, proj_bin$preferred_project)
prop_test_proj <- prop.test(proj_tab)
print(prop_test_proj)

# Statistical Test 2: Two-Proportion Z-Test (Language Preference)
lang_bin <- merged %>%
  filter(preferred_language_simplified %in% c("Python", "R"))
lang_tab <- table(lang_bin$prior_research, lang_bin$preferred_language_simplified)
prop_test_lang <- prop.test(lang_tab)
print(prop_test_lang)

