###################################
# Script to generate Figure 1 in:
# Friends With Text as Data Benefits:
# Assessing and Extending the Use of Automated Text Analysis in Political Science and Political Psychology
# Martijn Schoonvelde, Gijs Schumacher, Bert Bakker
# Contact: mschoonvelde@gmail.com
####################################

rm(list=ls())

# load libraries
library(lsr) 
library(foreign)
library(dplyr)
library(car)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
setwd("/Users/chuchuwan/Desktop/georgetown/2025Spring/PPOL-6801-Data as Text/rep 1/new")

# read in Slatcher et al. data
data <- read.spss("Election2004.sav", quote = FALSE, sep = ",", to.data.frame=TRUE)

# replicate means Table 1 paper

data.means <- data %>%
  select(SPEAKER, SOURCGEN, COGCMPLX, SEX, DEPRESS, AGING, PRESIDEN, HONESTY) %>%
  group_by(SOURCGEN, SPEAKER) %>%
  summarise(
    count = n(),
    mean.COGCMPLX = mean(COGCMPLX, na.rm = TRUE),
    mean.SEX = mean(SEX, na.rm = TRUE),
    mean.DEPRESS = mean(DEPRESS, na.rm = TRUE),
    mean.AGE = mean(AGING, na.rm = TRUE),
    mean.PRESIDEN = mean(PRESIDEN, na.rm = TRUE),
    mean.HONESTY = mean(HONESTY, na.rm = TRUE)
      ) %>%
  gather(key = style, value = mean, -c(SPEAKER, SOURCGEN, count)) %>%
  mutate(style = sub("mean.", "", style))


data.ses <- data %>%
  select(SPEAKER, SOURCGEN, COGCMPLX, SEX, DEPRESS, AGING, PRESIDEN, HONESTY) %>%
  group_by(SOURCGEN, SPEAKER) %>%
  summarise(
    count = n(),
    se.COGCMPLX = sd(COGCMPLX, na.rm = TRUE) / sqrt(count),
    se.SEX = sd(SEX, na.rm = TRUE) / sqrt(count),
    se.DEPRESS = sd(DEPRESS, na.rm = TRUE) / sqrt(count),
    se.AGE = sd(AGING, na.rm = TRUE) / sqrt(count),
    se.PRESIDEN = sd(PRESIDEN, na.rm = TRUE) / sqrt(count),
    se.HONESTY = sd(HONESTY, na.rm = TRUE) / sqrt(count)
  ) %>%
  gather(key = style, value = se, -c(SPEAKER, SOURCGEN, count))


data.means$se <- data.ses$se

# select data for town hall meeting, network interview, press conference 

data.means <- subset(data.means, SOURCGEN != "Debate")

# compute ANOVA and effect sizes, example depression

aov.depress <- aov(DEPRESS ~ SOURCGEN + SPEAKER, data = data)
etaSquared( aov.depress, type = 2, anova = TRUE )


# plot figure 1

pd = position_dodge(.4)

data.means$style <- as.factor(data.means$style)

names(data.means)[1] <- "Source"
names(data.means)[2] <- "Speaker"

levels(data.means$style)[1] <- "Aging"
levels(data.means$style)[2] <- "Complexity"
levels(data.means$style)[3] <- "Depression"
levels(data.means$style)[4] <- "Honesty"
levels(data.means$style)[5] <- "Presidentiality"
levels(data.means$style)[6] <- "Feminity"

# subset on having at least 10 speeches, and Kerry and Bush

data.means <- subset(data.means, count >= 10)
data.means <- subset(data.means, Speaker == "Kerry" | Speaker == "Bush")

# Create plot with distinct colors
plot <- ggplot(data.means, aes(x = Source, y = mean, fill = Source)) +  
  facet_grid(cols = vars(style), rows = vars(Speaker), margins = FALSE) +
  
  # Bar plot with distinct colors
  geom_bar(position = position_dodge(), stat = "identity", alpha = 0.6, 
           colour = "black",  # Black outline
           size = 0.3) +      # Thinner lines
  
  # Error bars matching bar colors
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                size = 0.3, width = 0.2, position = position_dodge(.9)) +
  
  # Apply custom colors to match your reference image
  scale_fill_manual(values = c("Network Interview" = "pink",  # Red/Pink
                               "Press Conference" = "red",   # Green
                               "Town Hall meeting" = "black")) +  # Blue
  
  # Minimalist theme
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(2, "lines"),
        legend.title = element_blank()) +
  
  # Labels
  ylab("Mean standardized LIWC score") +
  xlab("") +
  ggtitle("")

# Print the plot
print(plot)

ggsave(file="Fig_1.png", plot = plot, width=9, height=6)

