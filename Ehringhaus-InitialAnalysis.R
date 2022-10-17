# █▀█ █▀▀ █▀ █▀▀ ▀█▀ █▀
# █▀▄ ██▄ ▄█ ██▄ ░█░ ▄█
# clear console
cat("\014")
# clear global environment
rm(list = ls())
# clear plots
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
# clear packages
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE)
# disables scientific notion for entire R session
options(scipen = 100)

# █▀█ ▄▀█ █▀▀ █▄▀ ▄▀█ █▀▀ █▀▀ █▀
# █▀▀ █▀█ █▄▄ █░█ █▀█ █▄█ ██▄ ▄█
library(pacman)
p_load(tidyverse) # the usual suite of packages
p_load(skimr)
p_load(glue)
p_load(onewaytests)
p_load(knitr)
p_load(cowplot)

# █░█ █▀▀ █░░ █▀█ █▀▀ █▀█ █▀
# █▀█ ██▄ █▄▄ █▀▀ ██▄ █▀▄ ▄█
# helper function for generating tibble with important values 
report_values <- function(test, alpha) {
  test.method <- ifelse(
    str_detect(test$method, "Chi-squared"), "chi.squared", "anova")
  switch(test.method,
         "chi.squared" = tibble(
           alpha = alpha,
           degrees = unname(test$parameter),
           cv = round(qchisq(p = alpha, df = degrees, 
                             lower.tail = FALSE), 3),
           test.value = round(unname(test$statistic), 3)),
         "anova" = tibble(
           alpha = alpha,
           df.N = test$parameter[1],
           df.D = test$parameter[2],
           cv = round(qf(p = alpha, 
                         df1 = df.N, df2 = df.D, 
                         lower.tail = FALSE), 2),
           test.value = round(test$statistic, 2)))}

# helper function for making the decision
decision <- function(values) {
  significant <- values$test.value > values$cv
  ifelse(significant == TRUE, 
         glue("Reject the null hypothesis. 
              The test value of {values$test.value} 
              is greater than the critical value of {values$cv}"), 
         glue("Do not reject the null hypothesis. 
              The test value of {values$test.value}
              is less than the critical value of {values$cv}"))}

# helper function for summarizing the result
results <- function(values, hypotheses) {
  significant <- values$test.value > values$cv
  claim <- gsub(" (claim)", "", hypotheses$h1, fixed = TRUE)
  ifelse(significant == TRUE, 
         glue("There is sufficient evidence to support 
              the claim that {claim}."), 
         glue("There is not enough evidence to support 
              the claim that {claim}."))}

# █▀▀ █░░ █▀▀ ▄▀█ █▄░█ █ █▄░█ █▀▀
# █▄▄ █▄▄ ██▄ █▀█ █░▀█ █ █░▀█ █▄█
# source: https://www.kaggle.com/datasets/ianmobbs/texas-death-row-executions-info-and-last-words
death.row1 <- 
  list.files('/Users/justin/Desktop/ALY 6015/Project/data1', full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  rename(`Inmate.Race` = 'Race',
         `Age.When.Executed` = 'Age',
         `Not.Executed.Reason` = 'Reason') %>% 
  select(-Link, -Photo)

# source: https://www.kaggle.com/datasets/mykhe1097/last-words-of-death-row-inmates
death.row2 <- read_csv('/Users/justin/Desktop/ALY 6015/Project/data2/Texas Last Statement.csv') %>% 
  select(`TDCJ Number` = 'TDCJNumber',
         `Age.When.Received` = 'AgeWhenReceived',
         `Education.Level` = 'EducationLevel',
         `Prior.Prison.Record` = 'PreviousCrime',
         `Num.White.Victim` = 'WhiteVictim',
         `Num.Hispanic.Victim` = 'HispanicVictim',
         `Num.Black.Victim` = 'BlackVictim',
         `Num.Other.Race.Victim` = 'VictimOther Races',
         `Num.Female.Victims` = 'FemaleVictim',
         `Num.Male.Victims` = 'MaleVictim')

# source: https://github.com/coreybobco/Texas-Death-Row-Inmate-Webscraper/blob/master/tdcj.csv
death.row3 <- read_csv('/Users/justin/Desktop/ALY 6015/Project/data3/tdcj.csv') %>% 
  select('TDCJ Number',
         Prior.Occupation = 'Prior Occupation')
death.row3 <- death.row3[-c(41),] # data not formatted correctly
death.row3$`TDCJ Number` <- as.numeric(as.character(death.row3$`TDCJ Number`))

death.row <- full_join(death.row1, death.row2, by = 'TDCJ Number')
death.row <- full_join(death.row, death.row3, by = 'TDCJ Number')
rm(death.row1, death.row2, death.row3)

# Checking unique key for duplicates
death.row$`TDCJ Number`[duplicated(death.row$`TDCJ Number`)]
# 21 duplicates identified, most of these came from `death.row1`
# For each pair of duplicates, the second is missing more information
death.row %>% filter(`TDCJ Number` == 999584)
# Group by primary key, keep only the head, then ungroup
death.row <- death.row %>% group_by(`TDCJ Number`) %>% slice_head() %>% ungroup()
death.row %>% filter(`TDCJ Number` == 999584)

# █▀▀ ▀▄▀ █▀█ █░░ █▀█ █▀█ ▄▀█ ▀█▀ █▀█ █▀█ █▄█
# ██▄ █░█ █▀▀ █▄▄ █▄█ █▀▄ █▀█ ░█░ █▄█ █▀▄ ░█░
# summary of dataset
skim(death.row) %>% select(-starts_with('numeric.p'))

# visualization of age received and age executed by race
visualization1 <- 
  death.row %>% 
  select(Age.When.Executed, Age.When.Received, Inmate.Race) %>% 
  drop_na() %>% 
  ggplot +
  aes(x = Inmate.Race, fill = Inmate.Race) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.5, show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        text = element_text(size = 24))
plot_grid(visualization1 %+% aes(y = Age.When.Received) %+% labs(title = "Age When Received by Race"),
          visualization1 %+% aes(y = Age.When.Executed) %+% labs(title = "Age When Executed by Race"))

# visualization of 
visualization2 <- 
  death.row %>% 
  filter(!Inmate.Race == "Other") %>% 
  rowwise %>% 
  summarize(Education.Level, Inmate.Race,
            Total.Victims = sum(Num.White.Victim, 
                                Num.Hispanic.Victim, 
                                Num.Black.Victim, 
                                Num.Other.Race.Victim)) %>% 
  # filter(!Inmate.Race == "Other") %>% 
  drop_na() %>% 
  ggplot +
  aes(x = Education.Level, y = Total.Victims) +
  ylim(1, 6) +
  xlim(4, 16) +
  geom_jitter(show.legend = FALSE, alpha = 0.5, size = 4) +
  stat_summary(geom= "line", fun = "mean", size = 3, color = "#00AFBB") +
  facet_wrap(~ Inmate.Race) +
  labs(title = "Total Number of Victims by Inmate Race by Years of Education") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        text = element_text(size = 24))
visualization2

visualization3 <- 
  death.row %>% 
  select(Inmate.Race, Age.When.Executed, Education.Level, `Last Statement`) %>%
  filter(!Inmate.Race == "Other", !Inmate.Race == "Asian") %>% 
  rowwise %>% 
  summarize(Age.When.Executed,
            Inmate.Race,
            `Last Statement`, 
            Last.Statement.Word.Count = str_count(`Last Statement`, "\\S+")) %>% 
  drop_na() %>% 
  ggplot +
  aes(x = Age.When.Executed, y = Last.Statement.Word.Count, fill = Inmate.Race) +
  geom_jitter(alpha = 0.5, size = 3) +
  stat_smooth(method = 'lm', size = 1) +
  labs(title = "Word Count of Last Statement by Age When Executed") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        text = element_text(size = 24))
visualization3

# █████
# █▀░██
# ██░██
# ▀▄▄▄▀
###### Log the known information
question1 <- death.row %>% 
  select(Inmate.Race, 
         Num.White.Victim, 
         Num.Hispanic.Victim, 
         Num.Black.Victim, 
         Num.Other.Race.Victim) %>% 
  drop_na() %>% 
  arrange(Inmate.Race) %>% 
  rowwise %>% 
  summarize(Inmate.Race,
            Total.Victims = sum(Num.White.Victim, 
                                Num.Hispanic.Victim, 
                                Num.Black.Victim, 
                                Num.Other.Race.Victim))
            
###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0 = "there is no relationship between the race of the inmate and the 
    total number of victims",
    h1 = "there is a relationship between the race of the inmate and the
    total number of victims (claim)")

###### Find the critical value and test value.
one.anova <- aov.test(Total.Victims ~ Inmate.Race, 
                      data = question1, 
                      alpha = 0.05)
values <- report_values(test = one.anova, alpha = one.anova$alpha)
kable(values)

###### Make the decision.
writeLines(decision(values))

###### Summarize the results.
writeLines(results(values, hypotheses))

# ██████
# █▀▄▄▀█
# ██▀▄██
# ▀▄▄▄▄▀
###### Log the known information
question2 <- death.row %>% 
  select(Inmate.Race, 
         Age.When.Executed,
         Age.When.Received) %>% 
  drop_na() %>% 
  arrange(Inmate.Race) %>% 
  rowwise %>% 
  summarize(Inmate.Race,
            Years.On.Death.Row = Age.When.Executed - Age.When.Received)

###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0 = "there is no relationship between the race of the inmate and the 
    length of time in years on death row",
    h1 = "there is a relationship between the race of the inmate and the 
    length of time in years on death row (claim)")

###### Find the critical value and test value.
one.anova <- aov.test(Years.On.Death.Row ~ Inmate.Race, 
                      data = question2, 
                      alpha = 0.05)
values <- report_values(test = one.anova, alpha = one.anova$alpha)
kable(values)

###### Make the decision.
writeLines(decision(values))

###### Summarize the results.
writeLines(results(values, hypotheses))

# ██████
# █▄▄▄░█
# ██▄▄░█
# ▀▄▄▄▄▀
###### Log the known information
question3 <- death.row %>% 
  select(Inmate.Race, 
         Num.White.Victim, 
         Num.Hispanic.Victim, 
         Num.Black.Victim, 
         Num.Other.Race.Victim) %>% 
  drop_na() %>% 
  group_by(Inmate.Race) %>% 
  summarize(Num.White.Victim = sum(Num.White.Victim),
            Num.Hispanic.Victim = sum(Num.Hispanic.Victim),
            Num.Black.Victim = sum(Num.Black.Victim),
            Num.Other.Race.Victim = sum(Num.Other.Race.Victim)) %>% 
  subset(select = -Inmate.Race)
question3 <- as.data.frame(question3)
rownames(question3) <- c('Black', 'Hispanic', 'Other', 'White')

###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0 = "the race of the inmate is independent of the race of the victims",
    h1 = "the race of the inmate is dependent upon the race of the victims (claim)")

###### Find the critical value and test value.
chi.sq <- chisq.test(question3)
values <- report_values(test = chi.sq, alpha = 0.05)
kable(values)

###### Make the decision.
writeLines(decision(values))

###### Summarize the results.
writeLines(results(values, hypotheses))

# ██████
# █░█░██
# █▄▄░██
# ▀▀▄▄▄▀
###### Log the known information
question4 <- death.row %>% 
  select(Prior.Prison.Record, Education.Level) %>% 
  drop_na() %>% 
  table() %>% 
  as.data.frame.matrix()
rownames(question4) <- c('No', 'Yes')

###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0 = "the existence of a previous offense is independent of the level of education",
    h1 = "the existence of a previous offense is dependent upon the level of education (claim)")

###### Find the critical value and test value.
chi.sq <- chisq.test(question4)
values <- report_values(test = chi.sq, alpha = 0.05)
kable(values)

###### Make the decision.
writeLines(decision(values))

###### Summarize the results.
writeLines(results(values, hypotheses))

# ██████
# █░▄▄▄█
# █▄▄▄▒█
# ▀▄▄▄▄▀
###### Log the known information
question5 <- death.row %>% 
  select(Inmate.Race, Education.Level) %>% 
  drop_na() %>% 
  table() %>% 
  as.data.frame.matrix()

###### State the hypotheses and identify the claim.
hypotheses <- 
  tibble(
    h0 = "the race of the inmate is independent of the level of education",
    h1 = "the race of the inmate is dependent upon the level of education (claim)")

###### Find the critical value and test value.
chi.sq <- chisq.test(question5)
values <- report_values(test = chi.sq, alpha = 0.05)
kable(values)

###### Make the decision.
writeLines(decision(values))

###### Summarize the results.
writeLines(results(values, hypotheses))
    
# █▀▄ █▀▀ █▀ ▀█▀ █▀█ █▀█ █▄█
# █▄▀ ██▄ ▄█ ░█░ █▀▄ █▄█ ░█░
rm(hypotheses, chi.sq, one.anova, values)