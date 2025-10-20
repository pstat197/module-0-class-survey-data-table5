library(tidyverse)
# install.packages('infer') # execute once then comment out

# data location
url <- 'https://raw.githubusercontent.com/pstat197/pstat197a/main/materials/labs/lab3-iteration/data/biomarker-clean.csv'

# function for outlier trimming
trim_fn <- function(x){
  x[x > 3] <- 3
  x[x < -3] <- -3
  
  return(x)
}

# read in and preprocess data
asd <- read_csv(url) %>%
  select(-ados) %>%
  # log transform
  mutate(across(.cols = -group, log10)) %>%
  # center and scale
  mutate(across(.cols = -group, ~ scale(.x)[, 1])) %>%
  # trim outliers
  mutate(across(.cols = -group, trim_fn))

x <- asd %>% filter(group == 'ASD') %>% pull(CHIP)
y <- asd %>% filter(group == 'TD') %>% pull(CHIP)
t.test(x, y, var.equal = F)

n_tests <- 100
p_vals <- rep(NA, n_tests)
for(i in 1:n_tests){
  x <- asd %>% filter(group == 'ASD') %>% pull(i + 1)
  y <- asd %>% filter(group == 'TD') %>% pull(i + 1)
  p_vals[i] <- t.test(x, y, var.equal = F)$p.value
}

tibble(protein = colnames(asd)[2:(n_tests + 1)],
       p = p_vals)

#action 1

n_tests <- 50
rslt <- tibble(
  protein = colnames(asd)[2:(n_tests + 1)],
  p = NA,
  diff = NA
)
for(i in 1:n_tests){ #first 50 proteins
  x <- asd %>% filter(group == 'ASD') %>% pull(i + 1)
  y <- asd %>% filter(group == 'TD') %>% pull(i + 1)
  t_out <- t.test(x, y, var.equal = FALSE)
  
  rslt$p[i] <- t_out$p.value
  rslt$diff[i] <- t_out$estimate[1] - t_out$estimate[2]  # mean(ASD) - mean(TD)
}


vals <- rnorm(n = 4)
simple_fn <- function(x){2*x}
lapply(vals, simple_fn)


sapply(vals, simple_fn)


# apply a function to an index set
simple_fn_ix <- function(i){2*vals[i]}
rslt_apply <- sapply(1:length(vals), simple_fn_ix)

# equivalent for loop
rslt_loop <- rep(NA, length(vals))
for(i in 1:length(vals)){
  rslt_loop[i] <- 2*vals[i]
}

# compare
rbind(rslt_loop, rslt_apply)


# number of tests to perform
n_tests <- 100

# convert to a list
asd_list <- asd %>% 
  select(1:(n_tests + 1)) %>%
  pivot_longer(cols = -group,
               names_to = 'protein',
               values_to = 'level') %>%
  group_by(protein) %>%
  group_split()

# first entry in list
asd_list[[1]]

# p value for ith protein
tt_fn <- function(i){
  t.test(level ~ group, data = asd_list[[i]])$p.value
}

# check
tt_fn(1)

start <- Sys.time()
rslt <- sapply(1:n_tests, tt_fn)
end <- Sys.time()

end - start

start <- Sys.time()
n_tests <- 100
rslt <- tibble(protein = colnames(asd)[2:(n_tests + 1)],
               p = NA)
for(i in 1:n_tests){
  x <- asd %>% filter(group == 'ASD') %>% pull(i + 1)
  y <- asd %>% filter(group == 'TD') %>% pull(i + 1)
  rslt$p[i] <- t.test(x, y, var.equal = F)$p.value
}
end <- Sys.time()

end - start


#action 2 

n_tests <- 50 #50 proteins

# fn to compute estimated difference and SE for the i-th protein
tt_fn <- function(i) {
  x <- asd %>% filter(group == "ASD") %>% pull(i + 1)
  y <- asd %>% filter(group == "TD") %>% pull(i + 1)
  t_out <- t.test(x, y, var.equal = FALSE)
  
  diff_val <- t_out$estimate[1] - t_out$estimate[2]
  se_val <- (t_out$conf.int[2] - t_out$conf.int[1]) /
    (2 * qt(0.975, df = t_out$parameter))
  
  # names are included
  out <- c(diff = diff_val, se = se_val)
  return(out) #forgetting this line led to many issues
}

# run across 50 proteins
rslt <- sapply(1:n_tests, tt_fn) %>%
  t() %>%
  as_tibble() %>%  # ensure valid names even if missing
  setNames(c("diff", "se")) %>%           #  rename columns
  mutate(protein = colnames(asd)[2:(n_tests + 1)]) %>%
  select(protein, diff, se)

rslt #worked!


asd_nested <- asd %>%
  pivot_longer(-group, 
               names_to = 'protein', 
               values_to = 'level') %>%
  nest(data = c(level, group))

asd_nested %>% head(5)


asd_nested %>%
  slice(1L) %>%
  pull(data)

tt_fn <- function(.df){
  t.test(level ~ group, data = .df)
}

rslt <- asd_nested %>%
  slice(1:10) %>%
  mutate(ttest.out = map(data, tt_fn))

rslt

asd_nested %>% 
  slice(1L) %>% 
  unnest(cols = data) %>% 
  infer::t_test(formula = level ~ group,
                order = c('ASD', 'TD'),
                alternative = 'two-sided',
                var.equal = F)

# wrapper around infer::t_test
tt_fn <- function(.df){
  infer::t_test(.df, 
                formula = level ~ group,
                order = c('ASD', 'TD'),
                alternative = 'two-sided',
                var.equal = F)
}

# compute test results
tt_out <- asd_nested %>%
  slice(1:n_tests) %>%
  mutate(ttest = map(data, tt_fn))

# preview
tt_out %>% head(4)


tt_out %>% 
  unnest(ttest) %>%
  head(4)

# time it
start <- Sys.time()
tt_out <- asd_nested %>%
  slice(1:n_tests) %>%
  mutate(ttest = map(data, tt_fn))
end <- Sys.time()

end - start

# bonferroni correction
tt_out %>% 
  unnest(ttest) %>%
  mutate(p_adj = p_value*n_tests) %>%
  select(protein, p_value, p_adj) %>%
  arrange(p_adj) %>%
  head(4)

#action 3
library(tidyverse)
library(infer)


n_tests <- 50  # start with 50; change later to 1317

#-nest data by protein
asd_nested <- asd %>%
  pivot_longer(-group,
               names_to = "protein",
               values_to = "level") %>%
  nest(data = c(level, group))

# t-test function
tt_fn <- function(.df) {
  infer::t_test(
    .df,
    formula = level ~ group,
    order = c("ASD", "TD"),
    alternative = "two-sided",
    var.equal = FALSE
  )
}

#apply function to first 50 proteins
tt_out <- asd_nested %>%
  slice(1:n_tests) %>%
  mutate(ttest = map(data, tt_fn)) %>%
  unnest(ttest)

#benjamini hochberg correction
tt_out_bh <- tt_out %>%
  arrange(p_value) %>%
  mutate(
    rank = row_number(),
    p_adj = p_value * n_tests / rank,
    p_adj = ifelse(p_adj > 1, 1, p_adj)  
  )

#identify sig proteins
sig_proteins <- tt_out_bh %>%
  filter(p_adj < 0.01) %>%
  select(protein, p_value, p_adj, estimate, lower_ci, upper_ci)

sig_proteins

n_scale <- 1317

#same code but using all 1317 
start <- Sys.time() #for fun
#apply function to first 50 proteins
tt_out <- asd_nested %>%
  slice(1:n_scale) %>%
  mutate(ttest = map(data, tt_fn)) %>%
  unnest(ttest)

#benjamini hochberg correction
tt_out_bh <- tt_out %>%
  arrange(p_value) %>%
  mutate(
    rank = row_number(),
    p_adj = p_value * n_scale / rank,
    p_adj = ifelse(p_adj > 1, 1, p_adj)  
  )

#identify sig proteins
sig_proteins <- tt_out_bh %>%
  filter(p_adj < 0.01) %>%
  select(protein, p_value, p_adj, estimate, lower_ci, upper_ci)

sig_proteins
end <- Sys.time()

end - start




