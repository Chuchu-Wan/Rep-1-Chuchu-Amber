# Load required libraries
library(quanteda)
library(quanteda.textstats)
library(ggplot2)
library(dplyr)
library(syuzhet)  
library(readr)
library(tidyr)
setwd("/Users/chuchuwan/Desktop/georgetown/2025Spring/PPOL-6801-Data as Text/rep 1/new")

# Read dataset
df <- read_csv("comb_corpus.csv")

# Ensure text data has no missing values
df <- df %>% filter(!is.na(text))

# Compute sentiment scores
df$sentiment <- get_sentiment(df$text, method = "syuzhet")

# Replace missing country names with "Unknown"
df$country <- ifelse(is.na(df$country), "Unknown", df$country)

# ======= Visualization with Sentiment Explanation =======

# 1️⃣ Sentiment distribution by institution (Boxplot)
ggplot(df, aes(x = reorder(institution, sentiment, FUN = median), y = sentiment)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  coord_flip() +
  labs(
    title = "Sentiment Score Distribution by Institution",
    x = "Institution",
    y = "Sentiment Score",
    caption = "Sentiment Score: Positive values indicate more positive sentiment, 
               negative values indicate more negative sentiment."
  ) +
  theme_minimal()

# 2️⃣ Sentiment distribution by country (Boxplot)
ggplot(df, aes(x = reorder(country, sentiment, FUN = median), y = sentiment)) +
  geom_boxplot(fill = "tomato", alpha = 0.6) +
  coord_flip() +
  labs(
    title = "Sentiment Score Distribution by Country",
    x = "Country",
    y = "Sentiment Score",
    caption = "Sentiment Score: Positive values indicate more positive sentiment, 
               negative values indicate more negative sentiment. Missing country names are labeled as 'Unknown'."
  ) +
  theme_minimal()

# 3️⃣ Sentiment distribution by 10 random speakers (Boxplot)
set.seed(18)  # Set seed for reproducibility
random_speakers <- sample(unique(df$speaker), 10)  # Select 10 random speakers

ggplot(df %>% filter(speaker %in% random_speakers), aes(x = reorder(speaker, sentiment, FUN = median), y = sentiment)) +
  geom_boxplot(fill = "darkgreen", alpha = 0.6) +
  coord_flip() +
  labs(
    title = "Sentiment Score Distribution by 10 Random Speakers",
    x = "Speaker",
    y = "Sentiment Score",
    caption = "Sentiment Score: Positive values indicate more positive sentiment, 
               negative values indicate more negative sentiment. Speakers are randomly selected."
  ) +
  theme_minimal()

# 4️⃣ Histogram of Sentiment Scores
ggplot(df, aes(x = sentiment)) +
  geom_histogram(binwidth = 0.1, fill = "purple", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Sentiment Scores",
    x = "Sentiment Score",
    y = "Frequency",
    caption = "Sentiment Score: > 0 = Positive, < 0 = Negative, ~0 = Neutral."
  ) +
  theme_minimal()


# ========== 📌 2. Read Data ==========
df_nrc <- read_csv("comb_corpus.csv")

# Remove missing text values
df_nrc <- df_nrc %>% filter(!is.na(text))

# Replace missing country values with "Unknown"
df_nrc$country <- ifelse(is.na(df_nrc$country), "Unknown", df_nrc$country)

# Replace missing speaker values with "Unknown"
df_nrc$speaker <- ifelse(is.na(df_nrc$speaker), "Unknown", df_nrc$speaker)

# ========== 📌 3. Compute NRC Sentiment Scores ==========
nrc_sentiments <- get_nrc_sentiment(df_nrc$text)

# Merge sentiment scores with the dataset
df_nrc <- cbind(df_nrc, nrc_sentiments)

# Compute total emotion scores per text (avoid division by zero)
df_nrc$total_emotions <- rowSums(df_nrc[, c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")])
df_nrc$total_emotions[df_nrc$total_emotions == 0] <- 1  # Prevent division by zero

# Compute proportions for each emotion category
emotion_cols <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
df_nrc[emotion_cols] <- df_nrc[emotion_cols] / df_nrc$total_emotions

# Compute positive vs. negative sentiment proportions
df_nrc$positive_proportion <- df_nrc$positive / (df_nrc$positive + df_nrc$negative)
df_nrc$negative_proportion <- df_nrc$negative / (df_nrc$positive + df_nrc$negative)

# ========== 📊 4. Visualization ==========

# ------ 4.1 Overall Emotion Proportions ------
emotion_proportions <- colMeans(df_nrc[, emotion_cols])
emotion_df <- data.frame(
  emotion = names(emotion_proportions),
  proportion = as.numeric(emotion_proportions)
)

ggplot(emotion_df, aes(x = reorder(emotion, proportion), y = proportion, fill = emotion)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Proportion of Emotions in Text (NRC)",
    x = "Emotion",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ------ 4.2 Emotion Word Proportion by Institution ------
sentiment_by_institution <- df_nrc %>%
  group_by(institution) %>%
  summarise(total_emotion_words = mean(total_emotions, na.rm = TRUE))

ggplot(sentiment_by_institution, aes(x = reorder(institution, total_emotion_words), y = total_emotion_words, fill = institution)) +
  geom_col(alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Proportion of Emotional Words by Institution",
    x = "Institution",
    y = "Proportion of Emotion Words"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ------ 4.3 Emotion Proportions Within Each Institution ------
sentiment_by_institution_emotions <- df_nrc %>%
  group_by(institution) %>%
  summarise(across(all_of(emotion_cols), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = emotion_cols, names_to = "emotion", values_to = "proportion")

ggplot(sentiment_by_institution_emotions, aes(x = institution, y = proportion, fill = emotion)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Proportion of Different Emotions Within Each Institution",
    x = "Institution",
    y = "Proportion",
    fill = "Emotion"
  ) +
  theme_minimal()

# ------ 4.4 Positive vs. Negative Sentiment Proportion by Institution ------
sentiment_by_institution_polarity <- df_nrc %>%
  group_by(institution) %>%
  summarise(
    positive = mean(positive_proportion, na.rm = TRUE),
    negative = mean(negative_proportion, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c("positive", "negative"), names_to = "sentiment", values_to = "proportion")

ggplot(sentiment_by_institution_polarity, aes(x = institution, y = proportion, fill = sentiment)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("positive" = "blue", "negative" = "red")) +
  labs(
    title = "Proportion of Positive vs. Negative Sentiment Within Each Institution",
    x = "Institution",
    y = "Proportion",
    fill = "Sentiment"
  ) +
  theme_minimal()

# ------ 4.5 Positive vs. Negative Sentiment Proportion by Country ------
sentiment_by_country_polarity <- df_nrc %>%
  group_by(country) %>%
  summarise(
    positive = mean(positive_proportion, na.rm = TRUE),
    negative = mean(negative_proportion, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c("positive", "negative"), names_to = "sentiment", values_to = "proportion")

ggplot(sentiment_by_country_polarity, aes(x = country, y = proportion, fill = sentiment)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("positive" = "blue", "negative" = "red")) +
  labs(
    title = "Proportion of Positive vs. Negative Sentiment Within Each Country",
    x = "Country",
    y = "Proportion",
    fill = "Sentiment"
  ) +
  theme_minimal()

# ------ 4.6 Random 10 Speakers - Emotion Word Proportion ------

set.seed(2111)  # Set a dynamic seed for randomness (remove for fixed selection)
random_speakers <- sample(unique(df_nrc$speaker), min(10, length(unique(df_nrc$speaker))))  # Select 10 random speakers

sentiment_by_speaker <- df_nrc %>%
  filter(speaker %in% random_speakers) %>%
  group_by(speaker) %>%
  summarise(
    total_emotion_words = sum(total_emotions, na.rm = TRUE),
    total_words = sum(str_count(text, "\\S+"), na.rm = TRUE)  # Count total words spoken
  ) %>%
  mutate(emotion_word_proportion = total_emotion_words / total_words)

# Plot
ggplot(sentiment_by_speaker, aes(x = reorder(speaker, emotion_word_proportion), y = emotion_word_proportion, fill = speaker)) +
  geom_col(alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Random 10 Speakers by Proportion of Emotional Words",
    x = "Speaker",
    y = "Proportion of Emotion Words"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ------ 4.7 Random 10 Speakers - Emotion Category Proportions ------

# Filter dataset to only include those random 10 speakers
sentiment_by_speaker_emotions <- df_nrc %>%
  filter(speaker %in% random_speakers) %>%
  group_by(speaker) %>%
  summarise(across(all_of(emotion_cols), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = emotion_cols, names_to = "emotion", values_to = "proportion")

# Plot
ggplot(sentiment_by_speaker_emotions, aes(x = speaker, y = proportion, fill = emotion)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Emotion Category Proportions for Random 10 Speakers",
    x = "Speaker",
    y = "Proportion",
    fill = "Emotion"
  ) +
  theme_minimal()

# ------ 4.8 Random 10 Speakers - Positive vs. Negative Sentiment ------

# Filter dataset to only include the random 10 speakers
sentiment_by_speaker_polarity <- df_nrc %>%
  filter(speaker %in% random_speakers) %>%
  group_by(speaker) %>%
  summarise(
    positive = mean(positive_proportion, na.rm = TRUE),
    negative = mean(negative_proportion, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c("positive", "negative"), names_to = "sentiment", values_to = "proportion")

# Plot
ggplot(sentiment_by_speaker_polarity, aes(x = speaker, y = proportion, fill = sentiment)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("positive" = "blue", "negative" = "red")) +
  labs(
    title = "Proportion of Positive vs. Negative Sentiment for Random 10 Speakers",
    x = "Speaker",
    y = "Proportion",
    fill = "Sentiment"
  ) +
  theme_minimal()

# ------ 4.9 Emotion Category Proportions by Country ------

sentiment_by_country_emotions <- df_nrc %>%
  group_by(country) %>%
  summarise(across(all_of(emotion_cols), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = emotion_cols, names_to = "emotion", values_to = "proportion")

# Plot
ggplot(sentiment_by_country_emotions, aes(x = country, y = proportion, fill = emotion)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Emotion Category Proportions by Country",
    x = "Country",
    y = "Proportion",
    fill = "Emotion"
  ) +
  theme_minimal()
