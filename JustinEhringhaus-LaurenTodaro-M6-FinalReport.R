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
p_load(glue)
p_load(onewaytests)
p_load(knitr)
p_load(tm)
p_load(wordcloud2)
p_load(tidytext)
p_load(textdata)
p_load(cowplot)
p_load(gridExtra)

# █░█ █▀▀ █░░ █▀█ █▀▀ █▀█ █▀
# █▀█ ██▄ █▄▄ █▀▀ ██▄ █▀▄ ▄█
# Outputs a (glue) string, prettified for console printing
pretty_glue <- function(string, misc = NULL) {
  border <- strrep('*', 80)
  eval.string <- glue(string)
  return(cat(border, eval.string, border, sep = '\n'))
}

interpret.results <- 
  function(misc) {
    misc$decision <- 
      ifelse(misc$significant,
             paste("Reject the null hypothesis.", 
                   "There is enough evidence to", 
                   ifelse(misc$claim == 'h0', "reject", "support"), 
                   "the claim "),
             paste("Do not reject the null hypothesis.", 
                   "\nThere is not enough evidence to", 
                   ifelse(misc$claim == 'h0', "reject", "support"),
                   "the claim "))
    pretty_glue("Hypotheses and claim:
                 h0: {misc$h0}
                 h1: {misc$h1}
                 claim: {misc$claim}\n
                 Alpha:
                 {misc$alpha}\n
                 Critical value(s):
                 {misc$cv}\n
                 Test statistic:
                 {misc$test.statistic}\n
                 Decision:
                 {misc$decision}{misc$claim}.", misc)
  }

wilcoxon.rank.sum.test <- 
  function(data, misc) {
    n1 <- length(data[[1]])
    n2 <- length(data[[2]])
    sort.rank.combine.numerical.vectors <- 
      function(data) {
        data <- setNames(unlist(data, use.names = F), 
                         rep(names(data), lengths(data)))
        sorted <- sort(data)
        ranked <- rank(sorted, ties.method = "average")
        combined <- rbind(sorted, ranked)
        return(combined)
      }
    combined <- sort.rank.combine.numerical.vectors(data)
    lesser <- with(plyr::count(names(combined[1,])), x[which.min(freq)])
    R <- sum(combined['ranked', ][names(combined['ranked', ]) == lesser])
    mu.R <- (n1 * (n1 + n2 + 1)) / 2
    sg.R <- sqrt(((n1 * n2) * (n1 + n2 + 1)) / 12)
    misc$test.statistic <- 
      round((R - mu.R) / sg.R, 2)
    misc$significant <- 
      switch(misc$alternative,
             "two.sided" = !between(misc$test.statistic, -misc$cv, misc$cv),
             "greater" = misc$test.statistic > misc$cv,
             "lesser" = misc$test.statistic < misc$cv)
    misc$cv <- 
      ifelse(misc$alternative == 'two.sided', 
             paste('±', misc$cv), 
             misc$cv)
    interpret.results(misc)
  }

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
death.row2 <- 
  read_csv('/Users/justin/Desktop/ALY 6015/Project/data2/Texas Last Statement.csv') %>% 
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
death.row3 <- 
  read_csv('/Users/justin/Desktop/ALY 6015/Project/data3/tdcj.csv') %>% 
  select('TDCJ Number',
         Prior.Occupation = 'Prior Occupation')
death.row3 <- death.row3[-c(41),] # data not formatted correctly
death.row3$`TDCJ Number` <- as.numeric(as.character(death.row3$`TDCJ Number`))

# source: assigning sentiment categories manually to each last statement
death.row4 <- 
  read_csv('/Users/justin/Desktop/ALY 6015/Project/data4/sentiments.csv') %>% 
  select('TDCJ Number', 'Sentiment')

# Joining disparate sources by primary key
death.row <- full_join(death.row1, death.row2, by = 'TDCJ Number')
death.row <- full_join(death.row, death.row3, by = 'TDCJ Number')
death.row <- full_join(death.row, death.row4, by = 'TDCJ Number')
rm(death.row1, death.row2, death.row3, death.row4)

# Checking unique key for duplicates
death.row$`TDCJ Number`[duplicated(death.row$`TDCJ Number`)]
# 21 duplicates identified, most of these came from `death.row1`
# For each pair of duplicates, the second is missing more information
death.row %>% filter(`TDCJ Number` == 999584)
# Group by primary key, keep only the head, then ungroup
death.row <- death.row %>% group_by(`TDCJ Number`) %>% slice_head() %>% ungroup()
death.row %>% filter(`TDCJ Number` == 999584)

# adding variable `Education.Bins`
death.row <- 
  death.row %>% 
  mutate(Education.Bins = cut(Education.Level, breaks = c(0, 10, 20),
                              labels = c("Less than 3-Years High School", 
                                         "3-Years High School or More")),
         .after = "Education.Level")

# adding variable `Years.On.Death Row`
death.row <- 
  death.row %>% 
  mutate(Years.On.Death.Row = Age.When.Executed - Age.When.Received,
         .after = "Age.When.Executed")
# fixing data entry error, should never be negative
death.row$Years.On.Death.Row[death.row$Years.On.Death.Row < 0] <- 0

# adding variable `Total.Victims`
death.row <- 
  death.row %>%  
  rowwise() %>% 
  mutate(Total.Victims = sum(Num.Male.Victims, Num.Female.Victims),
                             .after = `Num.Male.Victims`)

# Cleaning `Last Statement` for word cloud visualization
death.row$`Last Statement` <- iconv(death.row$`Last Statement`, 
                                    from = 'UTF-8', to = 'ASCII//TRANSLIT')
death.row$`Last Statement`[death.row$`Last Statement` == "None"] <- NA
# corpus
statements.corpus <- Corpus(VectorSource(death.row$`Last Statement`))
# before text cleaning
inspect(statements.corpus[85])
# Convert the text to lower case
statements.corpus <- tm_map(statements.corpus, tolower)
# Remove numbers
statements.corpus <- tm_map(statements.corpus, removeNumbers)
# Remove punctuations
statements.corpus <- tm_map(statements.corpus, removePunctuation)
# Eliminate extra white spaces
statements.corpus <- tm_map(statements.corpus, stripWhitespace)
# Remove common English stopwords
statements.corpus <- tm_map(statements.corpus, removeWords, stopwords("english"))
# Text stemming
statements.corpus <- tm_map(statements.corpus, stemDocument)
# after text cleaning
inspect(statements.corpus[85])

# █▀▀ ▀▄▀ █▀█ █░░ █▀█ █▀█ ▄▀█ ▀█▀ █▀█ █▀█ █▄█
# ██▄ █░█ █▀▀ █▄▄ █▄█ █▀▄ █▀█ ░█░ █▄█ █▀▄ ░█░
visualization <- 
  death.row %>% 
  select(Inmate.Race, Years.On.Death.Row, Total.Victims, Education.Bins,
         Age.When.Received, Age.When.Executed) %>% 
  drop_na() %>% 
  ggplot +
  aes() +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        text = element_text(size = 20),
        axis.text = element_text(size = 16))

# Dot plot, visualization #1
years.on.death.row.by.race <- 
  visualization %+% 
  aes(x = Inmate.Race, y = Years.On.Death.Row, fill = Inmate.Race) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.45, show.legend = FALSE) +
  ylim(0, 30) +
  labs(title = "Years on Death Row by Race")
years.on.death.row.by.race

# Density plot, visualization #2
age.executed.by.education <- 
  visualization %+% 
  aes(Age.When.Executed) +
  geom_density(aes(fill = Education.Bins), alpha = 0.4) +
  labs(title = "Density Plot of Ages at Time of Execution by Education Level") +
  geom_vline(data = . %>% 
               group_by(Education.Bins) %>%
               summarize(mean.age.exeucted.by.edu = mean(Age.When.Executed)),
             aes(xintercept = mean.age.exeucted.by.edu,
                 color = Education.Bins), 
             size = 1,
             linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 0))
age.executed.by.education

# Word cloud, visualization #3
TermDocumentMatrix(statements.corpus)
tdm <- TermDocumentMatrix(statements.corpus)
# a glimpse at the matrix
inspect(tdm[1:5, 1:5])
tdm <- as.matrix(tdm)
v <- sort(rowSums(tdm), decreasing=TRUE)
d <- data.frame(word = names(v), freq = v)
w <- data.frame(names(v), v)
colnames(w) <- c('word', 'freq')
set.seed(1234)
wordcloud2(w, size = 0.7, shape = 'triangle', rotateRatio = 0.5, minSize = 1)
rm(v, d, w, tdm)

# █▀█ █░█ █▀▀ █▀ ▀█▀ █ █▀█ █▄░█   █▀█ █▄░█ █▀▀
# ▀▀█ █▄█ ██▄ ▄█ ░█░ █ █▄█ █░▀█   █▄█ █░▀█ ██▄
# Parametric: Assumption MET on populations approximately normally distributed
death.row %>% 
  select(Inmate.Race, Years.On.Death.Row) %>% 
  drop_na() %>% 
  filter(!Inmate.Race == "Other") %>% 
  ggplot +
  aes(x = Years.On.Death.Row, fill = Inmate.Race) +
  geom_histogram(alpha = 0.33, position = "identity", bins = 10) +
  theme_classic() +
  theme(legend.position = "top")

# Parametric: Assumption NOT MET on variances of the populations being equal
death.row %>% 
  select(Inmate.Race, Years.On.Death.Row) %>% 
  drop_na() %>% 
  filter(!Inmate.Race == "Other") %>% 
  group_by(Inmate.Race) %>% 
  summarize(variances = var(Years.On.Death.Row))

# Exploring counts and population means
death.row %>% 
  select(Inmate.Race, Years.On.Death.Row) %>% 
  drop_na() %>% 
  group_by(Inmate.Race) %>% 
  summarize(count = n(),
            Mean.Years.On.Death.Row = sum(Years.On.Death.Row) / length(Years.On.Death.Row))

kruskal.test(Years.On.Death.Row ~ Inmate.Race, data = death.row)

# █▀█ █░█ █▀▀ █▀ ▀█▀ █ █▀█ █▄░█   ▀█▀ █░█░█ █▀█
# ▀▀█ █▄█ ██▄ ▄█ ░█░ █ █▄█ █░▀█   ░█░ ▀▄▀▄▀ █▄█
# Parametric: Assumption NOT MET on populations approximately normally distributed
death.row %>% 
  select(Education.Bins, Age.When.Executed) %>% 
  drop_na() %>%
  ggplot +
  aes(x = Age.When.Executed, fill = Education.Bins) +
  geom_histogram(alpha = 0.33, position = "identity", bins = 10) +
  theme_classic() +
  theme(legend.position = "top")

# Exploring counts and population means
death.row %>% 
  select(Education.Bins, Age.When.Received, Age.When.Executed) %>% 
  drop_na() %>% 
  group_by(Education.Bins) %>% 
  summarize(count = n(),
            Mean.Age.When.Received = sum(Age.When.Received) / length(Age.When.Received),
            Mean.Age.When.Executed = sum(Age.When.Executed) / length(Age.When.Executed))

less.than.three.years.hs <- death.row %>% 
  select(Education.Bins, Age.When.Executed) %>% 
  drop_na() %>% 
  filter(Education.Bins == "Less than 3-Years High School")
three.years.or.more.hs <- death.row %>% 
  select(Education.Bins, Age.When.Executed) %>% 
  drop_na() %>% 
  filter(Education.Bins == "3-Years High School or More")
question2 <- list(
  less.than.three.years.hs = as.vector(less.than.three.years.hs$Age.When.Executed),
  three.years.or.more.hs = as.vector(three.years.or.more.hs$Age.When.Executed)
)
setup <- list(alpha = 0.05, 
              cv = 1.96,
              alternative = 'two.sided',
              h0 = 'There is no difference in ages at time of execution between those with less than three years of high school and those with three or more years of high school',
              h1 = 'There is a difference in ages at time of execution between those with less than three years of high school and those with three or more years of high school',
              claim = 'h1')
wilcoxon.rank.sum.test(question2, setup)

# █▀█ █░█ █▀▀ █▀ ▀█▀ █ █▀█ █▄░█   ▀█▀ █░█ █▀█ █▀▀ █▀▀
# ▀▀█ █▄█ ██▄ ▄█ ░█░ █ █▄█ █░▀█   ░█░ █▀█ █▀▄ ██▄ ██▄
# Parametric: Assumption NOT MET on populations approximately normally distributed
death.row %>% 
  select(Sentiment, Total.Victims) %>% 
  drop_na() %>% 
  ggplot +
  aes(x = Total.Victims, fill = Sentiment) +
  geom_histogram(alpha = 0.33, position = "identity", bins = 10) +
  theme_classic() +
  theme(legend.position = "top")

# Exploring counts and population means
total.victim.by.sentiment <- death.row %>%
  select(Sentiment, Total.Victims) %>%
  drop_na() %>%
  group_by(Sentiment) %>%
  summarize(Count = n(),
            Mean.Total.Victims = sum(Total.Victims) / length(Total.Victims))
plot.new()
grid.table(total.victim.by.sentiment)
title("Sentiments Bucketed with Counts and Mean Total Victims", line = -8)

kruskal.test(Total.Victims ~ Sentiment, data = death.row)