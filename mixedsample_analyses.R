# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)

# Total repetitions to failure mixed ANOVA -------------------------------------------------------
# Effect of genre and sex on total reps to failure

## Data prep 
data <- read_csv("bench_press_data.csv")
head(data)

# Select variables 
data_reps <- data %>%
  select(ID, Sex, PrefGroup, Pref_TotalReps, NP_TotalReps) %>%
  drop_na()

# Convert to long dataset

data_reps_long <- data_reps %>%
  gather(key = "genre", value = "total_reps", Pref_TotalReps, NP_TotalReps)
head(data_reps_long, 3)

## Overall descriptives --------

reps_summary <- data_reps_long %>%
  group_by(genre) %>%
  summarise(count = n(),
            mean = mean(total_reps),
            sd = sd(total_reps))

## Sex descriptives -------------------------------------

sex_summary <- data_reps_long %>%
  group_by(Sex, genre) %>%
  summarise(count = n(),
            mean = mean(total_reps),
            sd = sd(total_reps))

## Mixed ANOVA [sex*genre] ----------

data_reps_afx <- afex::aov_4(
  total_reps ~  Sex * genre + (genre | ID),
  data = data_reps_long,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction and partial eta squared
data_reps_afx

summary(data_reps_afx)

### Resolving assumptions -------------
# Checking distribution 

ggplot(data_reps_long, aes(total_reps)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(data_reps_long, aes(genre, total_reps, color = genre)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

# Normality test

# shapiro-wilk test on individual groups

data_reps_long %>% 
  dplyr::group_by(genre, Sex) %>% 
  rstatix::shapiro_test(total_reps) 

# checking normality on residuals

reps_norm <- performance::check_normality(data_reps_afx)

plot(reps_norm)
plot(reps_norm, type = "qq")

# Outliers check 

data_reps_long %>%
  group_by(genre) %>%
  identify_outliers(total_reps)

# Homogenity of variance

performance::check_homogeneity(data_reps_afx) 

# Motivation ANOVA -----------------------------------------------------------

#Effect of genre and sex on motivation

## Data prep 

data_mot <- data %>%
  select(ID, Sex, Pref_Motivation, NP_Motivation) %>%
  drop_na()

# Convert to long dataset 

data_mot_long <- data_mot %>%
  gather(key = "genre", value = "motivation", Pref_Motivation, NP_Motivation)
head(data_mot_long, 3)

## Descriptives -------------

mot_summary <- data_mot_long %>%
  group_by(genre) %>%
  summarise(count = n(),
            mean = mean(motivation),
            sd = sd(motivation))

## Mixed ANOVA [sex*genre] on motivation ----------

data_mot_afx <- afex::aov_4(
  motivation ~  Sex * genre + (genre | ID),
  data = data_mot_long,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction and partial eta squared
data_mot_afx

summary(data_mot_afx)

mot_data_emm <- emmeans::emmeans(data_mot_afx, ~Sex, model = "multivariate")
mot_data_emm

### Post hoc contrasts ----------

posthocresults <- pairs(mot_data_emm, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthocresults

### Resolving assumptions  --------------
# Checking distribution

ggplot(data_mot_long, aes(motivation)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(data_mot_long, aes(genre, motivation, color = genre)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

# Normality test

# shapiro-wilk test on individual groups

data_mot_long %>% 
  dplyr::group_by(genre, Sex) %>% 
  rstatix::shapiro_test(motivation) 

# checking normality on residuals

mot_norm <- performance::check_normality(data_mot_afx)

plot(mot_norm)
plot(mot_norm, type = "qq")

# Outliers check 

data_mot_long %>%
  group_by(genre) %>%
  identify_outliers(motivation)

# Homogenity of variance

performance::check_homogeneity(data_mot_afx) 

# Attentional focus paired t-test ------------------------------------------------------------
# Effect of genre on attentional focus

## Data prep --------

atten_data  <- data %>%
  select(ID, Pref_Attention, NP_Attention) %>%
  drop_na()

## Convert to long dataset 

atten_data_long <- atten_data %>%
  gather(key = "genre", value = "attention", Pref_Attention, NP_Attention)
head(atten_data_long, 3)

## Descriptives -------------

atten_summary <- atten_data_long  %>%
  group_by(genre) %>%
  summarise(count = n(),
            mean = mean(attention),
            sd = sd(attention))

## Resolving assumptions  ----------------
### Checking distribution 

ggplot(atten_data_long, aes(attention)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(atten_data_long, aes(genre, attention, color = genre)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Checking for outliers on difference score 

atten_data <- atten_data %>% 
  mutate(differences = NP_Attention - Pref_Attention) 

atten_data %>%
  identify_outliers(differences)

### Checking normality 

atten_data %>% shapiro_test(differences) 

## Attention test ------------

# As normality test was significant, we conducted a non-parametric test

atten_results <- wilcox.test(attention ~ genre, atten_data_long, paired = TRUE, alternative = "two.sided")
atten_results

# compute z-value for the wilcoxon test

z_atten <- qnorm(atten_results$p.value/2)
z_atten

# RPE paired t-test -----------------------------------------------------------
# Effect of genre on RPE

RPE_data  <- data %>%
  select(ID, Pref_RPE, NP_RPE) %>%
  drop_na()

## Convert to long dataset 

RPE_data_long <- RPE_data %>%
  gather(key = "genre", value = "RPE", Pref_RPE, NP_RPE)
head(RPE_data_long, 3)

## Descriptives --------------

RPE_summary <- RPE_data_long  %>%
  group_by(genre) %>%
  summarise(count = n(),
            mean = mean(RPE),
            sd = sd(RPE))

## Resolving assumptions  ----------
### Checking distribution 

ggplot(RPE_data_long, aes(RPE)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(RPE_data_long, aes(genre, RPE, color = genre)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Checking for outliers on difference score 

RPE_data <- RPE_data %>% 
  mutate(differences =  NP_RPE - Pref_RPE) 

RPE_data %>%
  identify_outliers(differences)

### Checking normality 

RPE_data %>% shapiro_test(differences) 

## RPE paired t-test -------------

RPE_results <- t.test(RPE ~ genre, RPE_data_long, 
                  alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
RPE_results

### RPE effect size -------

RPE_dz <- d.dep.t.diff.t(t=RPE_results$statistic, n=RPE_summary$count[1], a = 0.05) %>%
  as.data.frame()

# Rating paired t-test -----------------------------------------------------------

# Effect of genre on song rating

rating_data  <- data %>%
  select(ID, Pref_songrating, NP_songrating) %>%
  drop_na()

## Convert to long dataset 

rating_data_long <- rating_data %>%
  gather(key = "genre", value = "rating", Pref_songrating, NP_songrating)
head(rating_data_long, 3)

## Descriptives ------------

rating_summary <- rating_data_long  %>%
  group_by(genre) %>%
  summarise(count = n(),
            mean = mean(rating),
            sd = sd(rating))

## Resolving assumptions  -------
### Checking distribution 

ggplot(rating_data_long, aes(rating)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(rating_data_long, aes(genre, rating, color = genre)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Checking for outliers on difference score 

rating_data <- rating_data %>% 
  mutate(differences = NP_songrating - Pref_songrating) 

rating_data %>%
  identify_outliers(differences)

### Checking normality 

rating_data %>% shapiro_test(differences) 

## Rating paired t-test -----------------

rating_results <- t.test(rating ~ genre, rating_data_long, 
                  alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
rating_results

### Rating effect size -------

rating_dz <- d.dep.t.diff.t(t=rating_results$statistic, n=rating_summary$count[1], a = 0.05) %>%
  as.data.frame()
rating_dz

