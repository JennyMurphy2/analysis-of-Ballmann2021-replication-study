# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)

# Data loading and prep ---------------------
data <- read_csv("bench_press_data.csv")
head(data)

# filtering the data to only keep the male sample for this replication analysis
# this is to match the original study

data1 <- data %>%
  filter(Sex == "1")
head(data1)

rep_data <- data1 %>%
  select(ID, Pref_TotalReps, NP_TotalReps) %>%
  drop_na()

rep_data <- rep_data %>% 
  mutate(differences =  Pref_TotalReps - NP_TotalReps) 

# Convert to long dataset

rep_data_long <- rep_data %>%
  gather(key = "genre", value = "total_reps", Pref_TotalReps, NP_TotalReps)
head(rep_data_long, 3)

# Descriptives Total Repetitions ---------------

replication_desc <- rep_data_long %>%
  group_by(genre) %>%
  summarise(count = n(),
            mean = mean(total_reps),
            sd = sd(total_reps)) %>%
  mutate(mean_diff = mean(rep_data$differences), 
         sd_diff = sd(rep_data$differences))

# Resolving assumptions  ---------------------------------------

## Distribution check 

ggplot(rep_data_long, aes(total_reps)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(rep_data_long, aes(genre, total_reps, color = genre)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

rep_data %>%
  identify_outliers(differences)

### Normality check  

rep_data %>% shapiro_test(differences) 

# Paired t-test Total Reps -----------------------------

replication_ttest <- t.test(total_reps ~ genre, rep_data_long, 
                  alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
replication_ttest

# Total reps effect size calculation ------

## Calculate replication effect size  -------

rep_dz <- d.dep.t.diff.t(t = replication_ttest$statistic, n = replication_desc$count[1], a = 0.05)
rep_dz

## Original study values ------

reps_ori_study <- data.frame(
  ori_pval = 0.005,
  ori_N = 12,
  reported_es = 0.84,
  ori_m1 = 10.58,
  ori_sd1 = 2.07,
  ori_m2 = 8.9,
  ori_sd2 = 1.8)

# Estimating the t-value

quantile = 1 - (reps_ori_study$ori_pval/2)  # for two-tailed

ori_tval <- qt(quantile, df = 11, lower.tail = FALSE) %>%
  abs()

# Confirming the reported effect size (d = 0.84)
#Calculating original ES and its CI

#PAIRED SAMPLES - Calculating dz and its CI using t value

ori_dz <- d.dep.t.diff.t(t=ori_tval, n=reps_ori_study$ori_N, a = 0.05)
ori_dz

# Cohen's dav

ori_dav <- d.dep.t.avg(m1=reps_ori_study$ori_m1, m2=reps_ori_study$ori_m2, 
                       sd1=reps_ori_study$ori_sd1, sd2=reps_ori_study$ori_sd2, 
                       n=reps_ori_study$ori_N, a = 0.05)
ori_dav

# The reported effect size of 0.84 does not seem to be dz 
# It seems closest to calculations of dav

# Total repetitions z-test --------

rep_test <- compare_smd(
  smd1 = rep_dz$d,
  n1 = replication_desc$count[1],
  smd2 = reps_ori_study$reported_es,
  n2 = 12,
  paired = TRUE,
  alternative = "less")
rep_test

# Motivation paired t-test replication sample -----------------------------------------------------------

motivation_data  <- data1 %>%
  select(ID, Pref_Motivation, NP_Motivation) %>%
  drop_na()

## Convert to long dataset 

motivation_data_long <- motivation_data %>%
  gather(key = "genre", value = "motivation", Pref_Motivation, NP_Motivation)
head(motivation_data_long, 3)

## Descriptives ------------

motivation_summary <- motivation_data_long  %>%
  group_by(genre) %>%
  summarise(count = n(),
            mean = mean(motivation),
            sd = sd(motivation))

## Resolving assumptions  -------
### Checking distribution 

ggplot(motivation_data_long, aes(motivation)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(motivation_data_long, aes(genre, motivation, color = genre)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Checking for outliers on difference score 

motivation_data <- motivation_data %>% 
  mutate(differences = Pref_Motivation - NP_Motivation) 

motivation_data %>%
  identify_outliers(differences)

### Checking normality 

motivation_data %>% shapiro_test(differences) 

## Motivation paired t-test -----------------

motivation_results <- t.test(motivation ~ genre, motivation_data_long, 
                             alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
motivation_results

### Motivation effect size -------

motivation_dz <- d.dep.t.diff.t(t=motivation_results$statistic, n=motivation_summary$count[1], a = 0.05) %>%
  as.data.frame()
motivation_dz

motivation_dz <- motivation_dz %>%
  select(d, dlow, dhigh, n, df, p) %>%
  abs()

## Motivation original study values ------
# have to conservatively estimate the p-value
mot_ori_study <- data.frame(
  ori_pval = 0.00099,
  ori_N = 12,
  reported_es = 5.90,
  ori_m1 = 80.4,
  ori_sd1 = 11.2,
  ori_m2 = 18.8,
  ori_sd2 = 9.29)

# Estimating the t-value

mot_quantile = 1 - (mot_ori_study$ori_pval/2)  # for two-tailed

mot_ori_tval <- qt(mot_quantile, df = 11, lower.tail = FALSE) %>%
  abs()

# Confirming the reported effect size (d = 0.84)
#Calculating original ES and its CI

#PAIRED SAMPLES - Calculating dz and its CI using t value

mot_ori_dz <- d.dep.t.diff.t(t=mot_ori_tval, n=mot_ori_study$ori_N, a = 0.05)
mot_ori_dz

# Cohen's dav

mot_ori_dav <- d.dep.t.avg(m1=mot_ori_study$ori_m1, m2=mot_ori_study$ori_m2, 
                       sd1=mot_ori_study$ori_sd1, sd2=mot_ori_study$ori_sd2, 
                       n=mot_ori_study$ori_N, a = 0.05)
mot_ori_dav

# The reported effect size of 5.90 does not seem to be dz 
# It seems closest to calculations of dav

# Motivation z-test --------

mot_rep_test <- compare_smd(
  smd1 = motivation_dz$d,
  n1 = motivation_summary$count[1],
  smd2 = mot_ori_study$reported_es,
  n2 = 12,
  paired = TRUE,
  alternative = "less")
mot_rep_test
