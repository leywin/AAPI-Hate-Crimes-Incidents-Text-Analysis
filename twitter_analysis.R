library(tidyverse)
library(academictwitteR)
library(tidytext)
library(ggplot2)
library (devtools)
library(qdapRegex)
library(lubridate)
library(proxy)
library(wordcloud)
library(RColorBrewer)
library(glmnet)
library(ranger)

# =============================================================================
#                           PART A: OBTAINING DATA  
# =============================================================================

# testing set
# uncomment the one of the three sets of users to pull the data
# users <- c("nytimes", "WSJ")
# users <- c("CNN", "FoxNews")
# users <- c("TIME", "washingtonpost")
tweets_df <- get_user_tweets(users, "2020-03-17T00:00:00Z", 
                             "2021-03-31T23:59:59Z", 
                             bearer_token)
# store the data
write_rds(tweets_df,"data/nytimes_WJS.rds")
# store the data
write_rds(tweets_df,"data/TIME_washingtonpost.rds")
# store the data
write_rds(tweets_df,"data/CNN_FoxNews.rds")

# training set 
# uncomment the one of the three sets of users to pull the data
# users <- c("StopAAPIHate", "CAAsanfrancisco")
# users <- c("aaldef", "WashTheHate")
# users <- c("Nextshark", "AAPIData")

# DECEMBER
tweets_df <- get_user_tweets(users, "2020-12-01T00:00:00Z", 
                             "2020-12-31T23:59:59Z", 
                             bearer_token)
# store the data
write_rds(tweets_df,"data/Dec_2020/StopAAPIHate_CAAsanfrancisco.rds")
# store the data
write_rds(tweets_df,"data/Dec_2020/Nextshark_AAPIData.rds")
# store the data
write_rds(tweets_df,"data/Dec_2020/aaldef_WashTheHate.rds")

# JANUARY
tweets_df <- get_user_tweets(users, "2021-01-01T00:00:00Z", 
                             "2021-01-31T23:59:59Z", 
                             bearer_token)
# store the data
write_rds(tweets_df,"data/Jan_2021/StopAAPIHate_CAAsanfrancisco.rds")
# store the data
write_rds(tweets_df,"data/Jan_2021/Nextshark_AAPIData.rds")
# store the data
write_rds(tweets_df,"data/Jan_2021/aaldef_WashTheHate.rds")

# =============================================================================
#                             PART B: MERGE DATASETS 
# =============================================================================

# --------------
# media datasets
# --------------
# The New York Times AND The Wall Street Journal
nyt_WSJ <- readr::read_rds("data/nytimes_WJS.rds")
nyt_WSJ <- nyt_WSJ %>%
  select(author_id, created_at, text) %>%
  mutate(
    author = case_when(
      author_id == 3108351 ~ "The Wall Street Journal",
      author_id == 807095 ~ "The New York Times"
    ),
    year = year(parse_date_time(created_at, "YmdHMS")),
    month = month(parse_date_time(created_at, "YmdHMS")),
    day = day(parse_date_time(created_at, "YmdHMS")),
    hour = hour(parse_date_time(created_at, "YmdHMS")),
    minute = minute(parse_date_time(created_at, "YmdHMS")),
    second = second(parse_date_time(created_at, "YmdHMS"))
  ) %>%
  cbind(nyt_WSJ$public_metrics)

# CNN AND Fox News
cnn_fox <- readr::read_rds("data/CNN_FoxNews.rds")
cnn_fox <- cnn_fox %>%
  select(author_id, created_at, text) %>%
  mutate(
    author = case_when(
      author_id == 759251 ~ "CNN",
      author_id == 1367531 ~ "Fox News"
    ),
    year = year(parse_date_time(created_at, "YmdHMS")),
    month = month(parse_date_time(created_at, "YmdHMS")),
    day = day(parse_date_time(created_at, "YmdHMS")),
    hour = hour(parse_date_time(created_at, "YmdHMS")),
    minute = minute(parse_date_time(created_at, "YmdHMS")),
    second = second(parse_date_time(created_at, "YmdHMS"))
  ) %>%
  cbind(cnn_fox$public_metrics)


# The Washington Post AND TIME
time_wp <- readr::read_rds("data/TIME_washingtonpost.rds")
time_wp <- time_wp %>%
  select(author_id, created_at, text) %>%
  mutate(
    author = case_when(
      author_id == 2467791 ~ "The Washington Post",
      author_id == 14293310 ~ "TIME"
    ),
    year = year(parse_date_time(created_at, "YmdHMS")),
    month = month(parse_date_time(created_at, "YmdHMS")),
    day = day(parse_date_time(created_at, "YmdHMS")),
    hour = hour(parse_date_time(created_at, "YmdHMS")),
    minute = minute(parse_date_time(created_at, "YmdHMS")),
    second = second(parse_date_time(created_at, "YmdHMS"))
  ) %>%
  cbind(time_wp$public_metrics)

# merge the media data
media <- rbind(nyt_WSJ, time_wp, cnn_fox) %>% arrange(created_at)

# store as csv file
write_csv(media, "data/media.csv")

# --------------------
# asian based datasets 
# --------------------

# DECEMBER
# ---------
aaldef_WashTheHate_dec <- readr::read_rds("data/Dec_2020/aaldef_WashTheHate.rds")
aaldef_WashTheHate_dec <- aaldef_WashTheHate_dec %>%
  select(author_id, created_at, text) %>%
  mutate(
    author = case_when(
      author_id == 30128506 ~ "Asian American Legal",
      author_id == 1238181941070962688 ~ "WashTheHate"
    ),
    year = year(parse_date_time(created_at, "YmdHMS")),
    month = month(parse_date_time(created_at, "YmdHMS")),
    day = day(parse_date_time(created_at, "YmdHMS")),
    hour = hour(parse_date_time(created_at, "YmdHMS")),
    minute = minute(parse_date_time(created_at, "YmdHMS")),
    second = second(parse_date_time(created_at, "YmdHMS"))
  ) %>%
  cbind(aaldef_WashTheHate_dec$public_metrics)


Nextshark_AAPIData_dec <- readr::read_rds("data/Dec_2020/Nextshark_AAPIData.rds")
Nextshark_AAPIData_dec <- Nextshark_AAPIData_dec %>%
  select(author_id, created_at, text) %>%
  mutate(
    author = case_when(
      author_id == 1424846803 ~ "NextShark",
      author_id == 1122110059 ~ "AAPI Data"
    ),
    year = year(parse_date_time(created_at, "YmdHMS")),
    month = month(parse_date_time(created_at, "YmdHMS")),
    day = day(parse_date_time(created_at, "YmdHMS")),
    hour = hour(parse_date_time(created_at, "YmdHMS")),
    minute = minute(parse_date_time(created_at, "YmdHMS")),
    second = second(parse_date_time(created_at, "YmdHMS"))
  ) %>%
  cbind(Nextshark_AAPIData_dec$public_metrics)


AAPI_CAA_dec <- readr::read_rds("data/Dec_2020/StopAAPIHate_CAAsanfrancisco.rds")
AAPI_CAA_dec <- AAPI_CAA_dec %>%
  select(author_id, created_at, text) %>%
  mutate(
    author = case_when(
      author_id == 1295455922777501696 ~ "Stop AAPI Hate",
      author_id == 279201057 ~ "CAA sanfrancisco"
    ),
    year = year(parse_date_time(created_at, "YmdHMS")),
    month = month(parse_date_time(created_at, "YmdHMS")),
    day = day(parse_date_time(created_at, "YmdHMS")),
    hour = hour(parse_date_time(created_at, "YmdHMS")),
    minute = minute(parse_date_time(created_at, "YmdHMS")),
    second = second(parse_date_time(created_at, "YmdHMS"))
  ) %>%
  cbind(AAPI_CAA_dec$public_metrics)

# JANUARY
# -------
aaldef_WashTheHate_jan <- readr::read_rds("data/Jan_2021/aaldef_WashTheHate.rds")
aaldef_WashTheHate_jan <- aaldef_WashTheHate_jan %>%
  select(author_id, created_at, text) %>%
  mutate(
    author = case_when(
      author_id == 30128506 ~ "Asian American Legal",
      author_id == 1238181941070962688 ~ "WashTheHate"
    ),
    year = year(parse_date_time(created_at, "YmdHMS")),
    month = month(parse_date_time(created_at, "YmdHMS")),
    day = day(parse_date_time(created_at, "YmdHMS")),
    hour = hour(parse_date_time(created_at, "YmdHMS")),
    minute = minute(parse_date_time(created_at, "YmdHMS")),
    second = second(parse_date_time(created_at, "YmdHMS"))
  ) %>%
  cbind(aaldef_WashTheHate_jan$public_metrics)


Nextshark_AAPIData_jan <- readr::read_rds("data/Jan_2021/Nextshark_AAPIData.rds")
Nextshark_AAPIData_jan <- Nextshark_AAPIData_jan %>%
  select(author_id, created_at, text) %>%
  mutate(
    author = case_when(
      author_id == 1424846803 ~ "NextShark",
      author_id == 1122110059 ~ "AAPI Data"
    ),
    year = year(parse_date_time(created_at, "YmdHMS")),
    month = month(parse_date_time(created_at, "YmdHMS")),
    day = day(parse_date_time(created_at, "YmdHMS")),
    hour = hour(parse_date_time(created_at, "YmdHMS")),
    minute = minute(parse_date_time(created_at, "YmdHMS")),
    second = second(parse_date_time(created_at, "YmdHMS"))
  ) %>%
  cbind(Nextshark_AAPIData_jan$public_metrics)


AAPI_CAA_jan <- readr::read_rds("data/Jan_2021/StopAAPIHate_CAAsanfrancisco.rds")
AAPI_CAA_jan <- AAPI_CAA_jan %>%
  select(author_id, created_at, text) %>%
  mutate(
    author = case_when(
      author_id == 1295455922777501696 ~ "Stop AAPI Hate",
      author_id == 279201057 ~ "CAA sanfrancisco"
    ),
    year = year(parse_date_time(created_at, "YmdHMS")),
    month = month(parse_date_time(created_at, "YmdHMS")),
    day = day(parse_date_time(created_at, "YmdHMS")),
    hour = hour(parse_date_time(created_at, "YmdHMS")),
    minute = minute(parse_date_time(created_at, "YmdHMS")),
    second = second(parse_date_time(created_at, "YmdHMS"))
  ) %>%
  cbind(AAPI_CAA_jan$public_metrics)

# combine all the asian twitter accounts
asian_account <- rbind(
  aaldef_WashTheHate_dec, aaldef_WashTheHate_jan,
  Nextshark_AAPIData_dec, Nextshark_AAPIData_jan,
  AAPI_CAA_dec, AAPI_CAA_jan
) %>%
  arrange(created_at)

# store as csv file
write_csv(asian_account, "data/asian_account.csv")

# =============================================================================
#                           PART C: CREATING FEATURES
# =============================================================================

# manually label on `asian_account.csv` and
# save as `asian_account_with_labels.csv`

# (1)
# Import data
tweets <- readr::read_csv("data/asian_account_with_labels.csv")

# (2)
# clean the text by remove the twitter url from tweets body
# install_github("trinker/qdapRegex")
tweets$text <- rm_url(tweets$text,
  pattern = pastex("@rm_twitter_url", "@rm_url")
) %>%
  tolower()
# only get the word with AAPI_related_indicator == 1
tweets_with_labels <- tweets %>% filter(AAPI_related_indicator == 1)

# (3)
# break down by words
tweets_words <- tweets_with_labels %>%
  unnest_tokens(word, text)

# (4)
# remove stop words
tweets_words <- tweets_words %>%
  anti_join(stop_words)

# (5)
# count the 20 most frequent uni-gram used by the Asian twitter accounts
frequent_words <- tweets_words %>%
  count(word) %>%
  arrange(desc(n)) %>%
  slice(1:20)


# (6)
# let's look at bigrams now

# create all bigrams
bigrams_tweets <- tweets_with_labels %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# filter out NAs that come from spaces
bigrams_tweets <- bigrams_tweets %>%
  filter(!is.na(bigram))

# need to remove stop words from each component word
bigrams_tweets <- bigrams_tweets %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ")

# count the 20 most frequent bi-gram used by the Asian twitter accounts
frequent_bigrams <- bigrams_tweets %>%
  count(bigram) %>%
  arrange(desc(n)) %>%
  slice(1:20)

# (7)
# Finalize the list of features

# we end up with 10 unigrams and 10 bigrams
unigram <- c(
  "racist", "aapi", "report", "stopaapihate", "community",
  "incident(s)*", "racism", "asian(s)*", "china", "hate"
)
bigram <- c(
  "asian american(s)*", "aapi hate", "anti asian(s)*",
  "hate crime(s)*", "anti aapi", "china virus", "asian hate",
  "asian racism", "hate incident(s)*", "aapi community"
)

# (8)
# create columns for each the 20 words in tweets
# if they are existed in the text, it's coded as 1 in that column.
# Otherwise, it's coded as 0

# add unigram
for (i in 1:10) {
  # for training set
  tweets[, unigram[i]] <- tweets %>%
    mutate(ifelse(grepl(unigram[i], text), 1, 0)) %>%
    select(tail(names(.), 1))
}

# add bigram
for (i in 1:10) {
  # for training set
  tweets[, bigram[i]] <- tweets %>%
    mutate(ifelse(grepl(bigram[i], text), 1, 0)) %>%
    select(tail(names(.), 1))
}

# store the data for training set
write_csv(tweets, "data/tweets_with_features.csv")

# =============================================================================
#                               PART D: MODELING
# =============================================================================
set.seed(1234)

# (1)
# import data
asian <- readr::read_csv("data/tweets_with_features.csv")
media <- readr::read_csv("data/media.csv")

# (2)
# code to get the feature columns for national news dataset
media$text <- rm_url(media$text,
  pattern = pastex("@rm_twitter_url", "@rm_url")
) %>%
  tolower()

# (3)
# create columns for each the 20 words in media
for (i in 1:10) {
  media[, unigram[i]] <- media %>%
    mutate(ifelse(grepl(unigram[i], text), 1, 0)) %>%
    select(tail(names(.), 1))
}

# add bigram
for (i in 1:10) {
  media[, bigram[i]] <- media %>%
    mutate(ifelse(grepl(bigram[i], text), 1, 0)) %>%
    select(tail(names(.), 1))
}

# correct column names
colnames(media) <- gsub("(s)*", ".s", colnames(media), fixed = T)
colnames(media) <- gsub(" ", "_", colnames(media), fixed = T)

# training set
# only select variables used in the model and correct variable names
train <- asian[, 15:35]
colnames(train) <- gsub("(s)*", ".s", colnames(train), fixed = T)
colnames(train) <- gsub(" ", "_", colnames(train), fixed = T)


# ---------------------------- Lasso Regression --------------------------------

### fit lasso regression with both unigram and bigram predictors
model_lasso_bi <- glmnet(as.matrix(train[, 2:ncol(train)]),
  as.matrix(train[, 1]),
  family = "binomial",
  alpha = 1
)
plot(model_lasso_bi, xvar = "lambda", label = TRUE)

# cross validation to select lambda
cvfit.bi <- cv.glmnet(
  x = as.matrix(train[, 2:ncol(train)]),
  y = as.matrix(train[, 1]),
  family = "binomial",
  type.measure = "class"
)
plot(cvfit.bi)

# get the table of coefficients
coef(cvfit.bi, s = "lambda.min")


# corrplot::corrplot(cor(train[,-1], method = c("spearman")))
### predicting on the news data
test_lasso <- media
test_lasso$predicted.probability.logit.bi <-
  predict(cvfit.bi,
    newx = as.matrix(media[, 15:34]),
    s = "lambda.min",
    type = "response"
  )

# ---------------------------------- Random Forest ----------------------------

# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry = seq(2, 20, by = 2),
  node_size = seq(3, 10, by = 2),
  OOB_RMSE = 0
)

# total number of combinations
nrow(hyper_grid)

# grid search
for (i in 1:nrow(hyper_grid)) {

  # train model
  model <- ranger(
    formula = AAPI_related_indicator ~ .,
    data = train, num.trees = 1000,
    respect.unordered.factors = T,
    probability = T, replace = T,
    mtry = hyper_grid$mtry[i],
    min.node.size = hyper_grid$node_size[i],
    seed = 1234
  )

  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

# select the top 10 models based on the RMSE
hyper_grid %>%
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

# the best model selected
optimal_ranger <- ranger(
  formula = AAPI_related_indicator ~ .,
  data = train, num.trees = 1000,
  respect.unordered.factors = T,
  probability = T, replace = T,
  mtry = 6, min.node.size = 7,
  importance = "impurity"
)

# plot the feature importance
theme_set(theme_bw())
p <- optimal_ranger$variable.importance %>%
  generics::tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(20) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  labs(x = "", y = "Feature Importance") +
  theme(
    panel.grid.major.x = element_blank()
  )
# save the plot
ggsave(
  plot = p, file = "./figures/feature_importance.png",
  height = 5, width = 10
)

### predicting on the news data
test_rf <- media
pred <- predict(optimal_ranger, data = media[, 15:34])
test_rf$predicted.probability.rf.bi <- pred$predictions[, 2]

# write the result tables
write_csv(test_lasso, "data/test_lasso.csv")
write_csv(test_rf, "data/test_rf.csv")

# =============================================================================
#                             PART E: MODEL SELECTION
# =============================================================================

# LASSO Regression
# -----------------

# arrange the `test_lasso.csv` in descending order using the 
# `predicted.probability` column, and manually label the top 100 tweets 
# and called the column as `manual_label`,
# then save the file as `test_lasso_manuallylabel.csv`

# import the data for lasso regression
lasso <- readr::read_csv("data/test_lasso_manuallylabel.csv")

# check the number of 0 and 1
table(lasso$manual_label)
# 0 to 1: 48 vs 52


# Random Forest
# --------------

# arrange the `test_rf.csv` in descending order using the 
# `predicted.probability` column, and manually label the top 100 tweets 
# and called the column as `manual_label`,
# then save the file as `test_rf_manuallylabel.csv`

# import the data for lasso regression
rf <- readr::read_csv("data/test_rf_manuallylabel.csv")

# check the number of 0 and 1
table(rf$manual_label)
# 0 to 1: 30 vs 70

# we go with random forest!

# =============================================================================
#                             PART F: VISUALIZATION
# =============================================================================

# DATA EXPLORATORY PLOT
# ---------------------
# import raw data
twitter <- readr::read_csv("data/twitter.csv")

# clean the text by remove the twitter url from tweets body
# install_github("trinker/qdapRegex")
twitter$text <- rm_url(twitter$text,
                       pattern = pastex("@rm_twitter_url", "@rm_url")
) %>%
  tolower()

# PLOT 1: number of times the tweets containing "Asian(s)" appear over time

# unnest the tweets and remove stop word
twitter_words <- twitter %>%
  select(text, created_at) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# store the incident in a dataframe
incident <- data.frame(
  date = as.Date(c(
    "2020-04-04", "2020-06-17",
    "2020-08-06", "2021-01-28",
    "2021-02-03", "2021-03-16"
  )),
  incident = c(
    "Edison, N.J.", "Wyckoff, N.J.",
    "Philadelphia, PA", "San Francisco,CA",
    "Brooklyn, NY", "Atlanta Shooting"
  ),
  x = as.Date(c(
    "2020-03-04", "2020-05-17",
    "2020-09-06", "2020-12-28",
    "2021-03-03", "2021-02-15"
  )),
  yend = c(5, 5, 5, 3, 5, 50),
  y = c(10, 10, 10, 7, 10, 45)
)

# count the word "asian(s)"
asian_over_time <- twitter_words %>%
  mutate(post_date = as.Date.POSIXct(created_at)) %>%
  filter(word %in% c("asian", "asians")) %>%
  group_by(post_date) %>%
  dplyr::count()

# plot
theme_set(theme_bw())
figure1 <- ggplot(asian_over_time, aes(x = post_date, y = n)) +
  geom_line() +
  geom_segment(
    data = incident, aes(
      x = x,
      y = y,
      xend = date,
      yend = yend
    ),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed")
  ) +
  ggplot2::annotate(
    geom = "text", size = 3, x = as.Date("2021-01-10"), y = 44,
    label = "03/16\nAtlanta Shooting", hjust = "left"
  ) +
  ggplot2::annotate(
    geom = "text", size = 3, x = as.Date("2020-04-20"), y = 15,
    label = "06/17\nWyckoff, N.J.", hjust = "left"
  ) +
  ggplot2::annotate(
    geom = "text", size = 3, x = as.Date("2020-03-01"), y = 13,
    label = "04/04\nEdison, N.J.", hjust = "left"
  ) +
  ggplot2::annotate(
    geom = "text", size = 3, x = as.Date("2020-12-15"), y = 10,
    label = "01/28\nS.F., CA", hjust = "left"
  ) +
  ggplot2::annotate(
    geom = "text", size = 3, x = as.Date("2021-03-15"), y = 13,
    label = "02/03\nBrooklyn, NY", hjust = "right"
  ) +
  ggplot2::annotate(
    geom = "text", size = 3, x = as.Date("2020-09-15"), y = 13,
    label = "08/06\nPhil, PA", hjust = "right"
  ) +
  labs(
    y = "Frequency",
    x = ""
  ) +
  theme(
    panel.grid.major.x = element_blank()
  )

# save plot
ggsave(figure1,
       file = "figures/figure1.png",
       width = 10, height = 5
)

# RESULT VISUALIZATION
 # -------------------

# import data
test.rf <- readr::read_csv("data/test_rf.csv")

# categorize tweets:
#       - probability >= 0.5 to group 1 (related to AAPI related crime)
#       - probability < 0.5 to group 0 (NOT related to AAPI related crime)
test.rf <- test.rf %>% 
  mutate(outcome = ifelse(predicted.probability.rf.bi >= 0.5, 1, 0))

# PLOT 3: Propotion of Predicted Relevant Tweets by News Outlet over Time
figure3 <- test.rf %>%
  mutate(
    post_date = as.Date.POSIXct(created_at),
    month_year = format(post_date, format = "%b %Y")
  ) %>%
  group_by(author, month_year) %>%
  mutate(total_tweets = n()) %>%
  group_by(author, month_year, outcome) %>%
  mutate(
    Frequency = n(),
    proportion = ifelse(outcome == 1, Frequency / total_tweets, 0)
  ) %>%
  filter(outcome == 1) %>%
  ggplot(., aes(
    x = reorder(month_year, post_date),
    y = proportion, group = author, colour = author
  )) +
  geom_line() +
  guides(colour = guide_legend(title = "News Outlets")) +
  scale_colour_manual(
    labels = c(
      "CNN", "Fox News", "The New York Times", "The Wall Street Journal",
      "The Washington Post", "TIME"
    ),
    values = c("black", "blue", "red", "green", "yellow", "orange")
  ) +
  labs(
    y = "Proportion",
    x = ""
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.2, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(face = "bold")
  )

# save plot
ggsave(figure3,
       file = "figures/figure3.png",
       width = 10, height = 5
)


# PLOT 4: top 5 used words by each news account on the tweets 
#         that are AAPI hate crime related

# clean the tweets
test_words <- test.rf  %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  filter(word != "rt")

# plot
figure4 <- test_words %>%
  filter(outcome == 1) %>% 
  group_by(author, word) %>% dplyr::count() %>% 
  arrange(desc(n)) %>% group_by(author) %>% slice(1:5) %>% 
  ggplot(., aes(reorder(word, -n), n))+
  geom_col() +
  xlab(NULL) +
  coord_flip() + theme_bw() +
  facet_wrap(~author, scales = "free_y") +
  theme(
    plot.title = element_text(size = 15),
    panel.grid.major.x = element_blank()
  )+ labs (y = "Frequency") +
  theme(
    panel.grid.major.x = element_blank()
  )

# save plot
ggsave(figure4,
       file = "figures/figure4.png",
       width = 10, height = 5
)


# PLOT 5: wordcloud of outcome == 1 from national news outlet dataset

# count for most used words among group 1
test_words_count <- test_words %>% 
  filter(outcome == 1) %>% 
  select(word) %>% 
  dplyr::count(word, sort=TRUE)

# wordcloud
wordcloud(words = test_words_count$word, 
          scale=c(3.5,0.5),
          freq = test_words_count$n, 
          min.freq = 1,           
          max.words=200, 
          random.order=FALSE,        
          colors=brewer.pal(8, "Dark2"))




















