
library(tidyverse)

# Retrieve class survey data
url <- 'https://raw.githubusercontent.com/pstat197/pstat197a/main/materials/labs/lab2-tidyverse/data/'
background <- paste(url, 'background-clean.csv', sep = '') %>%
  read_csv()
interest <- paste(url, 'interest-clean.csv', sep = '') %>%
  read_csv()
metadata <- paste(url, 'survey-metadata.csv', sep = '') %>%
  read_csv()

# filter
background %>%
  filter(math.comf > 3)

# select
background %>%
  select(math.comf)

# pull
background %>%
  pull(rsrch)

# mutate
background %>%
  mutate(avg.comf = (math.comf + prog.comf + stat.comf)/3)

# sequence of verbs
background %>%
  filter(stat.prof == 'adv') %>%
  mutate(avg.comf = (math.comf + prog.comf + stat.comf)/3) %>%
  select(avg.comf, rsrch)

# Summaries

# a summary
background %>%
  filter(stat.prof == 'adv') %>%
  mutate(avg.comf = (math.comf + prog.comf + stat.comf)/3) %>%
  select(avg.comf, rsrch) %>%
  summarize(prop.rsrch = mean(rsrch))

# equivalent (demonstrates mean() on a logical vector)
background %>%
  filter(stat.prof == 'adv') %>%
  mutate(avg.comf = (math.comf + prog.comf + stat.comf)/3) %>%
  select(avg.comf, rsrch) %>%
  pull(rsrch) %>%
  mean()

# multiple summaries
background %>%
  filter(stat.prof == 'adv') %>%
  mutate(avg.comf = (math.comf + prog.comf + stat.comf)/3) %>%
  select(avg.comf, rsrch) %>%
  summarize(prop.rsrch = mean(rsrch),
            med.comf = median(avg.comf))

# summarize_all
background %>%
  select(contains('comf')) %>%
  summarise_all(.funs = mean)

# create a grouping
background %>%
  group_by(stat.prof)

# count observations
background %>%
  group_by(stat.prof) %>%
  count()

# a grouped summary
background %>%
  group_by(stat.prof) %>%
  select(contains('.comf')) %>%
  summarize_all(.funs = mean)

# tidyr verbs examples

background %>% 
  filter(rsrch == TRUE) %>% 
  filter(updv.num == "6-8") %>% 
  select(prog.prof, stat.prof, math.prof)


background %>%
  filter(rsrch == FALSE) %>%
  arrange(updv.num) %>%
  select(updv.num, prog.prof, stat.prof, math.prof)

background %>%
  summarise(
    med_prog = median(prog.prof, na.rm = TRUE),
    med_stat = median(stat.prof, na.rm = TRUE),
    med_math = median(math.prof, na.rm = TRUE)
  )

background %>%
  group_by(updv.num) %>%
  summarise(
    med_prog = median(background$prog.prof, na.rm = TRUE),
    med_stat = median(background$stat.prof, na.rm = TRUE),
    med_math = median(background$math.prof, na.rm = TRUE)
  )

# many variables, many summaries
comf_sum <- background %>%
  select(contains('comf')) %>%
  summarise_all(.funs = list(mean = mean,
                             median = median,
                             min = min,
                             max = max))

# gather columns into long format
comf_sum %>%
  gather(stat, val)

# separate into rows and columns
comf_sum %>%
  gather(stat, val) %>%
  separate(stat, into = c('variable', 'stat'), sep = '_')

# spread into table
comf_sum %>%
  gather(stat, val) %>%
  separate(stat, into = c('variable', 'stat'), sep = '_') %>%
  spread(stat, val)

# ggplot example: summary of classes taken
classes <- background %>%
  select(11:28) %>%
  mutate(across(everything(), ~ ifelse(.x == 1, "yes", "no"))) %>%
  mutate(across(everything(), ~ factor(.x, levels = c("no", "yes")))) %>%
  summarize(across(everything(), ~ mean(as.numeric(.x) - 1, na.rm = TRUE))) %>%
  gather(class, proportion)

# arrange
classes %>%
  arrange(desc(proportion))

# plot it
classes %>%
  ggplot(aes(x = proportion, y = class)) +
  geom_point()

# reorder and save plot
fig <- classes %>%
  ggplot(aes(x = proportion, y = reorder(class, proportion))) +
  geom_point()

# adjust labels
fig + labs(x = 'proportion of class', y = '')