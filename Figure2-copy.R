####################################################################################################
# Script to generate Figures 2 and  3 and OLS regression results in Table A2 in:
# Friends With Text as Data Benefits:
# Assessing and Extending the Use of Automated Text Analysis in Political Science and Political Psychology
# Martijn Schoonvelde, Gijs Schumacher, Bert Bakker
# Contact: mschoonvelde@gmail.com
####################################################################################################

###############
#load libraries
###############

rm(list=ls())
library(quanteda)
install.packages("quanteda.textmodels")
library(quanteda.textmodels)
library(ggplot2)
library(stringr)
library(stargazer)
library(tidyverse)
library(hrbrthemes)
library(ggrepel)
library(tm)
library(lme4)
library(dplyr)


#set working directory to folder which includes supercorpus.Rdata
setwd("/Users/chuchuwan/Desktop/georgetown/2025Spring/PPOL-6801-Data as Text/rep 1/new")

load("comb.corpus.RData")
corpus$eucount <- corpus$eupres <- corpus$eusentence <- corpus$eusentences <- corpus$eutermwin <- NULL
corpus$length <- unname(ntoken(corpus$text))

# Add manifesto lef-right scale  as per Lowe et al. (2011) (lr)
# Add manifesto progressive-conservative scale (pc)
# Add manifesto EU positive-negative (EU)
# NB: pc includes items per108 and per 110
###########################################################
source("cmp-copy.R")

corpus$date <- as.Date(corpus$date, format = "%d-%m-%Y")
corpus <- subset(corpus, date != "2099-01-01")
corpus$year <- format(corpus$date, "%Y")
corpus$lr <- NA
corpus$pc <- NA

#link corpus to cmp data, and extract lr, pc, and EU scores
for(i in 1:nrow(corpus)){
  which.line <- subset(cmp.data, cmp.data$party == corpus$manifcode[i])
  if(nrow(which.line) > 0){
    which.line <- which.line[max(which(corpus$date[i] >= which.line$edate), na.rm = TRUE),]
    corpus$lr[i] <- -1*which.line$lr
    corpus$pc[i] <- -1*which.line$pc
  }}

corpus.speaker <- corpus %>%
  group_by(speaker, institution, lr, pc) %>%
  summarise(text = paste(text, collapse = " ")) %>%
  ungroup()

supercorpus <- corpus(corpus)
supercorpus <- corpus_subset(supercorpus, transl == FALSE)
supercorpus <- corpus_subset(supercorpus, length >= 200)
supercorpus <- corpus_subset(supercorpus, institution == "EP" | institution == "Nat. leader")

#create dtm
dtm <- dfm(tokens(supercorpus))

#create stop word use as a proportion of text

dtm_no_stopwords <- dfm_remove(dtm, stopwords("english"))
docvars(supercorpus)$perc.stopwords <- ntoken(dtm_no_stopwords) / ntoken(dtm)

#create use of numbers as a proportion of text

texts_supercorpus <- as.character(supercorpus)
docvars(supercorpus)$perc.numbers <- str_count(texts_supercorpus, '\\d+') / ntoken(dfm(tokens(supercorpus)))

#create stemmed words as a proportion of text
dtm_stemmed <- dfm_wordstem(dtm)
docvars(supercorpus)$perc.stemming <- ntype(dtm_stemmed) / ntype(dtm)

#create punctuation as a proportion of text
tokens_no_punct <- tokens(supercorpus, remove_punct = TRUE)
dtm_no_punct <- dfm(tokens_no_punct)
docvars(supercorpus)$perc.punctuation <- 1 - (ntoken(dtm_no_punct) / ntoken(dtm))


#print Figure 3

# averages
averages <- docvars(supercorpus) %>%
            group_by(speaker, country) %>%
                      summarize(mean_stopwords = mean(perc.stopwords, na.rm = TRUE),
                                mean_numbers = mean(perc.numbers, na.rm = TRUE),
                                mean_stemming = mean(perc.stemming, na.rm = TRUE),
                                mean_punctuation = mean(perc.punctuation, na.rm = TRUE),
                                n = n(), sd_stopwords = sd(perc.stopwords, na.rm = TRUE),
                                se = sd_stopwords/sqrt(n), mean_lr = mean(lr, na.rm = TRUE),
                                mean_pc = mean(pc, na.rm = TRUE)
                      )
              
averages <- na.omit(averages)

#plot averages against left-right

fig.stopwords <- ggplot(averages, aes(x=mean_lr, y=mean_stopwords)) +
  geom_point(shape = 1) +
  geom_smooth(size=0.5, alpha = 1/10, se = TRUE, color = "black") +
  theme_minimal() + 
  theme(axis.text.x = element_text(vjust=1, hjust=.5)) + 
  xlab("left-right position") +
  ylab("% stopwords") +
  theme(#legend.position="none",
        panel.spacing = unit(1.5, "lines"),
        legend.title = element_blank(),
        text = element_text(size=20)
  )
    
ggsave(fig.stopwords, file="Fig2d_lr.jpg", height=6, width=9, dpi=900)
print(fig.stopwords)

fig.numbers <- ggplot(averages, aes(x=mean_lr, y=mean_numbers)) +
  geom_point(shape = 1) +
  geom_smooth(size=0.5, alpha = 1/10, se = TRUE, color = "black") +
  theme_minimal() + 
  theme(axis.text.x = element_text(vjust=1, hjust=.5)) + 
  xlab("left-right position") +
  ylab("% numbers") +
  theme(#legend.position="none",
        panel.spacing = unit(1.5, "lines"),
        legend.title = element_blank(),
        text = element_text(size=20)
  )
    
ggsave(fig.numbers, file="Fig_2a_lr.jpg", height=6, width=9, dpi=900)
print(fig.numbers)


fig.stemming <- ggplot(averages, aes(x=mean_lr, y=mean_stemming)) +
  geom_point(shape = 1) +
  geom_smooth(size=0.5, alpha = 1/10, se = TRUE, color = "black") +
  theme_minimal() + 
  theme(axis.text.x = element_text(vjust=1, hjust=.5)) + 
  xlab("left-right position") +
  ylab("% stemming") +
  theme(#legend.position="none",
        panel.spacing = unit(1.5, "lines"),
        legend.title = element_blank(),
        text = element_text(size=20)
  )
    
ggsave(fig.stemming, file="Fig2c_lr.jpg", height=6, width=9, dpi=900)
print(fig.stemming)

fig.punctuation <- ggplot(averages, aes(x=mean_lr, y=mean_punctuation)) +
  geom_point(shape = 1) +
  geom_smooth(size=0.5, alpha = 1/10, se = TRUE, color = "black") +
  theme_minimal() + 
  theme(axis.text.x = element_text(vjust=1, hjust=.5)) + 
  xlab("left-right position") +
  ylab("% punctuation") +
  theme(#legend.position="none",
        panel.spacing = unit(1.5, "lines"),
        legend.title = element_blank(),
        text = element_text(size=20)
  )
    
ggsave(fig.punctuation, file="Fig2b_lr.jpg", height=6, width=9, dpi=900)
print(fig.punctuation)

#plot averages against progressive-conservative

fig.stopwords <- ggplot(averages, aes(x=mean_pc, y=mean_stopwords)) +
  geom_point(shape = 1) +
  geom_smooth(size=0.5, alpha = 1/10, se = TRUE, color = "black") +
  theme_minimal() + 
  theme(axis.text.x = element_text(vjust=1, hjust=.5)) + 
  xlab("progressive-conservative position") +
  ylab("% stopwords") +
  theme(#legend.position="none",
        panel.spacing = unit(1.5, "lines"),
        legend.title = element_blank(),
        text = element_text(size=20)
  )
    
ggsave(fig.stopwords, file="Fig2d_pc.jpg", height=6, width=9, dpi=900)
print(fig.stopwords)


fig.numbers <- ggplot(averages, aes(x=mean_pc, y=mean_numbers)) +
  geom_point(shape = 1) +
  geom_smooth(size=0.5, alpha = 1/10, se = TRUE, color = "black") +
  theme_minimal() + 
  theme(axis.text.x = element_text(vjust=1, hjust=.5)) + 
  xlab("progressive-conservative position") +
  ylab("% numbers") +
  theme(#legend.position="none",
        panel.spacing = unit(1.5, "lines"),
        legend.title = element_blank(),
        text = element_text(size=20)
  )
    
ggsave(fig.numbers, file="Fig2a_pc.jpg", height=6, width=9, dpi=900)
print(fig.numbers)

fig.stemming <- ggplot(averages, aes(x=mean_pc, y=mean_stemming)) +
  geom_point(shape = 1) +
  geom_smooth(size=0.5, alpha = 1/10, se = TRUE, color = "black") +
  theme_minimal() + 
  theme(axis.text.x = element_text(vjust=1, hjust=.5)) + 
  xlab("progressive-conservative position") +
  ylab("% stemming") +
  theme(#legend.position="none",
        panel.spacing = unit(1.5, "lines"),
        legend.title = element_blank(),
        text = element_text(size=20)
  )
    
ggsave(fig.stemming, file="Fig2c_pc.jpg", height=6, width=9, dpi=900)
print(fig.stemming)

fig.punctuation <- ggplot(averages, aes(x=mean_pc, y=mean_punctuation)) +
  geom_point(shape = 1) +
  geom_smooth(size=0.5, alpha = 1/10, se = TRUE, color = "black") +
  theme_minimal() + 
  theme(axis.text.x = element_text(vjust=1, hjust=.5)) + 
  xlab("progressive-conservative position") +
  ylab("% punctuation") +
  theme(#legend.position="none",
        panel.spacing = unit(1.5, "lines"),
        legend.title = element_blank(),
        text = element_text(size=20)
  )
    
ggsave(fig.punctuation, file="Fig2b_pc.jpg", height=6, width=9, dpi=900)
print(fig.punctuation)
# Bivariate regressions with country fixed effects (Appendix)

model.stopwords.lr <- lm(perc.stopwords ~ lr + factor(country), data = docvars(supercorpus))
model.stopwords.pc <- lm(perc.stopwords ~ pc + factor(country), data = docvars(supercorpus))
model.stopwords.lr.pc <- lm(perc.stopwords ~ lr*pc + factor(country), data = docvars(supercorpus))

model.numbers.lr <- lm(perc.numbers ~ lr + factor(country), data = docvars(supercorpus))
model.numbers.pc <- lm(perc.numbers ~ pc + factor(country), data = docvars(supercorpus))
model.numbers.lr.pc <- lm(perc.numbers ~ lr*pc + factor(country), data = docvars(supercorpus))

model.stemming.lr <- lm(perc.stemming ~ lr + factor(country), data = docvars(supercorpus))
model.stemming.pc <- lm(perc.stemming ~ pc + factor(country), data = docvars(supercorpus))
model.stemming.lr.pc <- lm(perc.stemming ~ lr*pc + factor(country), data = docvars(supercorpus))

model.punctuation.lr <- lm(perc.punctuation ~ lr + factor(country), data = docvars(supercorpus))
model.punctuation.pc <- lm(perc.punctuation ~ pc + factor(country), data = docvars(supercorpus))
model.punctuation.lr.pc <- lm(perc.punctuation ~ lr*pc + factor(country), data = docvars(supercorpus))


coef.names <- c("Left Right Position", "Progressive Convervative Position"," Belgium", "Czech Republic", "Denmark", "Spain", "France", "Greece", "Ireland", "Italy", "Netherlands", "Portugal", "United Kingdom", "Left Right X \n Progressive Conservative")

output.stopwords <- stargazer(model.stopwords.lr, model.stopwords.pc, model.stopwords.lr.pc, 
                    type = "latex", title = "OlS Model Predicting Stopword Use", no.space=TRUE, single.row = F,
                    covariate.labels = coef.names,
                    model.names = FALSE, dep.var.labels.include = FALSE, dep.var.caption = "XXX",
                    notes = "Standardized coefficients, standard errors in parentheses",
                    style = "io",
                    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                    star.char = c("+", "*", "**", "***"),
                    notes.append = FALSE)

write(output.stopwords, file ="appendix_table_stopwords.tex")


output.numbers <- stargazer(model.numbers.lr, model.numbers.pc, model.numbers.lr.pc, 
                    type = "latex", title = "OlS Model Predicting Proportion of Numbers", no.space=TRUE, single.row = F,
                    covariate.labels = coef.names,
                    model.names = FALSE, dep.var.labels.include = FALSE, dep.var.caption = "XXX",
                    notes = "Standardized coefficients, standard errors in parentheses",
                    style = "io",
                    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                    star.char = c("+", "*", "**", "***"),
                    notes.append = FALSE)

write(output.numbers, file ="appendix_table_numbers.tex")

output.stemming <- stargazer(model.stemming.lr, model.stemming.pc, model.stemming.lr.pc, 
                    type = "latex", title = "OlS Model Predicting Proportion of Unique Words", no.space=TRUE, single.row = F,
                    covariate.labels = coef.names,
                    model.names = FALSE, dep.var.labels.include = FALSE, dep.var.caption = "XXX",
                    notes = "Standardized coefficients, standard errors in parentheses",
                    style = "io",
                    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                    star.char = c("+", "*", "**", "***"),
                    notes.append = FALSE)

write(output.stemming, file ="appendix_table_stemming.tex")


output.punctuation <- stargazer(model.punctuation.lr, model.punctuation.pc, model.punctuation.lr.pc, 
                    type = "latex", title = "OlS Model Predicting Proportion of Punctuation", no.space=TRUE, single.row = F,
                    covariate.labels = coef.names,
                    model.names = FALSE, dep.var.labels.include = FALSE, dep.var.caption = "XXX",
                    notes = "Standardized coefficients, standard errors in parentheses",
                    style = "io",
                    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                    star.char = c("+", "*", "**", "***"),
                    notes.append = FALSE)

 write(output.punctuation, file ="appendix_table_punctuation.tex")

