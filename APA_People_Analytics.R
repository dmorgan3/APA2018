## ===== An introduction to R =====

# Here is a comment in a line

# R has an extensive online help system
help.start()  # .opens your browser to R help
help(mean)  # .help for specific functions
?mean
help.search("regression")  # .help with concepts
??regression
find("t.test")  # .what package to search in for this function?
apropos("glm")  # .names all objects that (partially) match your query

# Some native R functions have demos:
demo(Hershey)  # .fonts and characters
demo(plotmath)  # .equation editor
demo(persp)  # .density plots
demo(graphics)  # .plots and charts
demo()  # All available demos

# R Console
# You can type commands directly into the console
# The R interpreter will evaluate expressions and respond with a result (or an error)
# You will see the default command prompt >
17 + 3
# If you enter an incomplete command, R will prompt you with a +
1 * 2 * 3 * 4 * 5 *  # 6 * 7 * 8 * 9 * 10
# Scroll up or down to see history, or type history()
history()

# R as a Calculator
print("Hello world!")
"Hello world!"  # Case sensitive
1 + pi
234 / 87754
(1 + 0.05)^8
23.76*log(8) / (23 + tan(9))

# Vectors
c(0, 1, 1, 2, 7, 9)
1:30
c(1, 2, 3) + c(11, 13, 15)  # Perform an operation on two vectors
"APA is awesome!"  # Enter expressions using characters
  
# Functions (in R, functions perform all of the work!)
exp(1)
3 == 4  # Some functions are in the form of operators
my.function <- function(x, y) {x + y}  # Let's define our own function, and "call" it
my.function(1, 2)

# Variables
# R lets you assign values to variables or data frames and refer to them by name
# In R, the assignment operator is <-
w <- 1
x <- 2
y <- 3
z <- c(w, x, y)
z  #  .evaluate z to see what stores as z; Once z is assigned, even if you change the value of x or y, z will not change
z[3]  # Pick-off the 3rd item in vector z

# Data Structures
# Matrices
m <- matrix(data = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrow = 3, ncol = 4)
m
# Lists
# A list is a built-in data type for mixing objects of different types, i.e., multiple data types
my.list <- list(thing = "hat", size = "8.25") # A list containing a number and a string
my.list
# A list can even contain other lists:
my.list2 <- list("this list references another list", my.list)
my.list2
# Data frames
# A data frame is a list that contains multiple vectors of the same length
teams <- c("Finance", "Sales", "HR", "Engineering")
managers <- c(10, 15, 5, 24)
individual_contributors <-c(54, 122, 18, 99)
employee_data <-data.frame(teams, managers, individual_contributors)
employee_data
employee_data$managers
# Find the number of managers in HR
employee_data$teams == "HR"
employee_data$managers[employee_data$teams == "HR"]

# Models and formulas
# Example of linear model (lm) to predict pay: pay.model <- lm(formula = salary ~ job + tenure + performance, data = employee_roster)
# Example using Iris (data from Ronald Fisher from 1936: three plant species (setosa, virginica, versicolor) and four features measured in centimeters for each sample)
iris.model <- lm(formula = Petal.Width ~ Petal.Length + Sepal.Length, data = iris)
summary(iris.model)




## ===== Analyze data (employee engagement survey) =====

library(tidyverse)
library(data.table)
library(multilevel)  # item.total() 
library(irr)  # icc()
library(psych)  # ICC() ... Note: For IRT specifically, more extensive packages include the ltm and eRm, and should be used for serious IRT analysis
library(lme4)
library(psy)  # icc()
library(car)  # Recode()
library(boot)
library(GPArotation)
library(nFactors)
library(FactoMineR)
library(sem)
library(corrr)  # correlate()
library(lavaan)  # cfa()
library(semTools)  # reliability()
library(gbm)
library(readtext)
library(tm)
library(qdap)
library(SnowballC)
library(qdapDictionaries)
library(devtools)
library(koRpus)
library(textstem)
library(slam)
library(text2vec)

setwd("~/Professional Conferences/American Psychological Association (APA)/2018")

## Read-in the Data
items <- read_csv("survey_items.csv")
View(items)

survey_data <- read_csv("survey_data.csv")
View(survey_data)
dim(survey_data)
       
## Some Descriptive Statistics
df <- survey_data[, colnames(survey_data) %in% paste0("Q", 1:28)]
str(df)
summary(df)

correlate(df)
df %>% correlate()  # dplyr version

## Inter-rater Reliability
ICC_out <- psych::ICC(t(df), missing = FALSE, alpha = 0.05)  # missing = FALSE to include all cases
show(ICC_out)  # Look at Average: how reliable is the average score across all raters?

## Psychometric properties of the survey
# Inter-item correlations
inter_item <- df %>% correlate() %>% dplyr::select(-rowname) %>% colMeans(na.rm = TRUE)
show(inter_item)
mean(inter_item)  # Mean of the inter-item correlations

# Average item-total correlation (a correlation between the question score and the overall survey score)
df$score <- rowMeans(df, na.rm = T)
item_total <- df %>% correlate() %>% focus(score)
show(item_total)
mean(item_total$score)

# Chronbach's alpha (We're especially interested in std.alpha, which is "the standardized alpha based upon the correlations")
# Also note that we get "the average inter-item correlation", average_r, and various versions of "the correlation of each item with the total score" such as raw.r, whose values should match our earlier calculations
df$score <- NULL
psych::alpha(df)

# Spearman-Brown split-half reliability
score_e <- rowMeans(df[, seq(1, ncol(df), by = 2)])   # Calculating total score with even items
score_o <- rowMeans(df[, seq(2, ncol(df), by = 2)])  # Calculating total score with odd items
r_split_scores <- cor(score_e, score_o, use = "complete.obs")  # Correlating scores from even and odd items
show(r_split_scores)
(2 * r_split_scores) / (1 + r_split_scores)  # Adjusting with the Spearman-Brown prophecy formula

# Split half reliability estimates
splitHalf(df)
suppressWarnings(splitHalf(df))

# Cluster Correlations -- cluster.cor() -- to find correlations of multiple clusters, score multiple scales and report multiple statistics
View(items)
keys_df <- list(Job = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9"),
                           Company = c("Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21"),
                           People = c("Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28"))
keys <- make.keys(cor(df, use = "complete.obs"), keys_df)
cluster.cor(keys, cor(df, use = "complete.obs"))

## Cluster (unsupervised) Analyses

# Explore the data a bit more...
unsupervised_df <- df
headTail(unsupervised_df)  # quickView(unsupervised_df)
(cor_unsupervised_df <- cor(unsupervised_df, use = "complete.obs"))  # use = "na.or.complete"
lowerCor(unsupervised_df)
cor.plot(unsupervised_df, numbers = TRUE, main = "Engagement")
dev.off()

# Maximum Likelihood Factor Analysis (exploratory)
# Determine number of factors to extract...
# Henry Kaiser once said that "a solution to the number-of-factors problem in factor analysis is "easy,"and that he "used to make up one every morning before breakfast" :)
# But the problem, of course, is to find "the" solution, or at least a solution that others will regard quite highly (even if it's not necessarily the best)
# FA with five alternative algorithms:
# 1. minres factor analysis
# 2. principal axis factor analysis
# 3. weighted least squares factor analysis
# 4. generalized least squares factor analysis
# 5. maximum likelihood factor analysis
fa_fit <- fa(unsupervised_df, rotation = "promax", fm = "ml")  # fm = "minres", fm = "pa", fm = "wls", fm= "gls" and fm = "ml"
show(fa_fit)  # print results
fa.plot(fa_fit)  # Plot the loadings from a factor, principal components, or cluster analysis
fa.diagram(fa_fit)

# Very Simple Structure: Applies a goodness-of-fit test to determine the optimal number of factors to extract
# Included in vss is the MAP (Minimum Absolute Partial) correlation criterion
# Look for maximum fit
vsimp <- vss(unsupervised_df, title = "Very Simple Structure of Engage!")
show(vsimp)
summary(vsimp)

# Hierarchical clusters with 'rooted dendritic structure'
ic <- iclust(unsupervised_df)  # iclust
show(ic)
summary(ic)
ic <- iclust(unsupervised_df, nclusters = 4)
show(ic)
summary(ic)

# k means
unsupervised_df_km <- unsupervised_df %>% na.omit() %>% t() 
# unsupervised_df_km <- scale(unsupervised_df_km)  # optional
set.seed(110)
km <- kmeans(unsupervised_df_km, centers = 4, nstart = 25)
sort(km$cluster)




## ===== Text Analysis =====

source('APA_People_Analytics_UDFs.R')

article1 <- readtext("Living Up To Our Potential_How to Make Progress in HR.txt")
nchar(article1)
tail(article1[, 2])
article2 <- readtext("What Is Workplace Inclusion and Can You Measure It.txt")
nchar(article2)
tail(article2[, 2])
article3 <- readtext("Its not just about AI but TI.txt")
nchar(article3)
tail(article3[, 2])
article4 <- readtext("HR is hitting a second wall.txt")
nchar(article4)
tail(article4[, 2])
article5 <- readtext("Evolution Of People Analytics Over The Last Five Years.txt")
nchar(article5)
tail(article5[, 2])
article6 <- readtext("Diversity.txt")
nchar(article6)
tail(article6[, 2])
article7 <- readtext("Agility_The New Response to Dynamic Change.txt")
nchar(article7)
tail(article7[, 2])
article8 <- readtext("Why diversity is personal for me.txt")
nchar(article8)
tail(article8[, 2])
article9 <- readtext("The role of Organisational Network Analysis in People Analytics.txt")
nchar(article9)
tail(article9[, 2])
article10 <- readtext("How Slack Got Ahead in Diversity.txt")
nchar(article10)
tail(article10[, 2])
article11 <- readtext("How Atlassian Uses Data To Attract More Qualified And Diverse Graduate Candidates.txt")
nchar(article11)
tail(article11[, 2])
article12 <- readtext("How AI Will Make Our Workplace Fair With Diversity Tech Stack.txt")
nchar(article12)
tail(article12[, 2])
article13 <- readtext("Why are women better leaders than men.txt")
nchar(article13)
tail(article13[, 2])
article14 <- readtext("10 Trends in Workforce Analytics.txt")
nchar(article14)
tail(article14[, 2])
article15 <- readtext("Getting your organization's advanced analytics program right.txt")
nchar(article15)
tail(article15[, 2])
article16 <- readtext("How to Make Sure Agile Teams Can Work Together.txt")
nchar(article16)
tail(article16[, 2])
article17 <- readtext("What Does GDPR Really Mean For HR Teams.txt")
nchar(article17)
tail(article17[, 2])
article18 <- readtext("WorkTrends_The Power of People Data.txt")
nchar(article18)
tail(article18[, 2])
article19 <- readtext("Want to Debias Hiring_Change What Hiring Managers Focus On.txt")
nchar(article19)
tail(article19[, 2])
article20 <- readtext("Why Using Data Is Your Key to Increasing Diversity, According to This Tech Leader.txt")
nchar(article20)
tail(article20[, 2])
article21 <- readtext("Diversity and inclusion at work_facing up to the business case.txt")
nchar(article21)
tail(article21[, 2])
article22 <- readtext("You cant have organisational agility without these 5 building blocks.txt")
nchar(article22)
tail(article22[, 2])
article23 <- readtext("AI in HR.txt")
nchar(article23)
tail(article23[, 2])
article24 <- readtext("To Retain New Hires_Make Sure You Meet with Them in Their First Week.txt")
nchar(article24)
tail(article24[, 2])
article25 <- readtext("New Report_Four Ways to Advance Your People Analytics.txt")
nchar(article25)
tail(article25[, 2])
article26 <- readtext("People analytics_driving business performance with people data.txt")
nchar(article26)
tail(article26[, 2])
article27 <- readtext("Organizational Network Analytics and the Future of Work.txt")
nchar(article27)
tail(article27[, 2])

# A collection of documents forms a corpus
article_corpus <- c(article1[, 2], article2[, 2], article3[, 2],
                  article4[, 2], article5[, 2], article6[, 2],
                  article7[, 2], article8[, 2], article9[, 2],
                  article10[, 2], article11[, 2], article12[, 2],
                  article13[, 2], article14[, 2], article15[, 2],
                  article16[, 2], article17[, 2], article18[, 2],
                  article19[, 2], article21[, 2], article22[, 2],
                  article23[, 2], article24[, 2], article25[, 2],
                  article26[, 2], article27[, 2])
article_corpus <- enc2utf8(article_corpus)  # Encode to UTF-8
article_corpus_original <- article_corpus

# Tasks: Tokenization, vocabulary-building, and pre-processing (e.g., lowering string case, removing stopwords, performing lemmatization, etc.)
# Tokenization is the task of chopping up character sequences into pieces, called tokens, perhaps at the same time throwing away certain characters, such as punctuation
# Handle contractions
contraction_table <- qdapDictionaries::contractions
contraction_table$contraction <- tolower(contraction_table$contraction)
contraction_table$expanded <- tolower(contraction_table$expanded)
contraction_table <- rbind(contraction_table, c("cannot", "can not"), c("haven't", "have not"), c("hadn't", "had not"), c("here's", "here is"))
article_corpus <- replace_contraction(article_corpus, contraction = contraction_table, sent.cap = FALSE)
print(article_corpus[1])
# Handle abbreviations
abbreviation_table <- qdapDictionaries::abbreviations
abbreviation_table$abv <- tolower(abbreviation_table$abv)
abbreviation_table$rep <- tolower(abbreviation_table$rep)
article_corpus <- replace_abbreviation(article_corpus, abbreviation = abbreviation_table)
print(article_corpus[1])
# Remove or replace punctuation and handle special characters (like bullets)
# Remove punctuation (version 1)
article_corpus1 <- str_replace_all(article_corpus, "[[:punct:]]", "")
print(article_corpus1[1])
# Remove punctuation (version 2)
article_corpus2 <- paste("", article_corpus, "")
article_corpus2 <- gsub("([[:punct:]])+", "\\1", article_corpus2)  # Multiple-to-single punctuation
puncts <- "?!&"
keep <- "_-"  # Remove extraneous 'keep' characters
keep_puncts <- strsplit(keep, split = "")[[1]]
article_corpus2 <- gsub(paste(paste0(puncts, "{2,}|\\s", puncts, "|", puncts, "\\s"), collapse="|" ), " ", article_corpus2)  # Remove all puncts except between \\w
# Retain fractions e.g., 1/2 or 11/22
if ("/" %in% puncts) {
  article_corpus2 = gsub("/{2,}|\\s/|/\\s", " ", article_corpus2)  # Remove all / except between \\w
  article_corpus2 = gsub("([a-zA-Z]+)/([a-zA-Z]+)", "\\1 \\2", article_corpus2)
}
article_corpus2 <- gsub(paste0("[^[:alnum:][:blank:]", keep, "]"), " ", article_corpus2)  # Remove all other puncts
print(article_corpus2[1])
# Replace punctuation (e.g., ?, !, & is replaced by "mark_question", "mark_exclaim", "mark_ampersand")
puncts <- "?!&"
puncts <- strsplit(puncts, split = "")[[1]]
dict_puncts_final <- dict_puncts[dict_puncts$punct %in% puncts,]
patterns_multiple <- dict_puncts_final$pattern_multiple
patterns <- dict_puncts_final$pattern
article_corpus3 <- article_corpus2
for (i in seq(patterns)) {
  article_corpus3 = gsub(patterns_multiple[i], dict_puncts_final$punct[i], article_corpus3)
  article_corpus3 = gsub(patterns[i], dict_puncts_final$replacement[i], article_corpus3)
}
print(article_corpus3[1])

# Remove HTML information
article_corp <- article_corpus3
article_corp <- gsub("http.+? ", " ", article_corp)
article_corp <- gsub("http.+?$", " ", article_corp)
article_corp <- gsub("www.+\\.com", " ", article_corp)
article_corp <- gsub("www.+\\.net", " ", article_corp)
article_corp <- gsub("www.+\\.org", " ", article_corp)
print(article_corp[1])

# Handle numbers
# tm package:
# mycorpus <- tm_map(mycorpus, removeNumbers)
article_corp <- gsub("\\d{1,2}:\\d{1,2}\\s?(a|p)?m?", " datetime_time ", article_corp)  # 12:00pm datetime_time
article_corp <- gsub("\\d{1,2}\\s?(a|p)m", " datetime_time ", article_corp)  # 12pm datetime_time
article_corp <- gsub("\\d{4}-\\d{1,2}-\\d{1,2}|\\d{1,2}-\\d{1,2}-\\d{4}", " datetime_date ", article_corp) # 2018-08-10 datetime_date
article_corp <- gsub("\\b\\d+/\\d+", " math_number ", article_corp)  # 1/2 math_number
article_corp <- gsub("\\b\\d+\\.\\d+", " math_number ", article_corp)  # 1.2 math_number
article_corp <- gsub("\\b\\d+\\b", " math_number ", article_corp)  # Convert singular numbers
article_corp <- qdap::replace_number(article_corp)  # Translate
article_corp <- tm::removeNumbers(article_corp)  # Remove
print(article_corp[1])

# Remove leading and trailing spaces
# tm package:
# mycorpus <- tm_map(mycorpus, stripWhitespace)
article_corp <- trimws(article_corp)
article_corp <- gsub("\\s+", " ", article_corp)  # Remove intervening spaces
print(article_corp[1])

# Lemmatize
# Stemming usually refers to a crude heuristic process that chops off the ends of words, and often includes the removal of derivational affixes
# Lemmatization usually refers to more properly analyzing vocabulary and morphological characteristics of words, normally aiming to remove inflectional endings only and returning the base or dictionary form of a word, which is known as the lemma
# For example, if confronted with the token saw, stemming might return just s, whereas lemmatization would attempt to return either see or saw depending on whether the use of the token was as a verb or a noun
# tm package:
# mycorpus <- tm_map(mycorpus, stemDocument)
article_corp1 <- textstem::lemmatize_strings(article_corp)  # V1: Default lexicon::hash_lemmas dictionary
article_corp  %>%  # dplyr version
  lemmatize_strings() %>%
  head()
lemma_dictionary <- textstem::make_lemma_dictionary(article_corp1, engine = 'hunspell')  # V2: Hunspell dictionary
article_corp2 <- lemmatize_strings(article_corp1, dictionary = lemma_dictionary)

# Remove stopwords
# tm package:
# mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
article_corpus_final <- article_corp2
keep_stop_words <- c("no", "but", "would", "amp")
keep_puncts <- "_-"
stopwords <- tm::stopwords("en")
stopwords_list <- list(no = c("no", "nor", "neither", "without", "against"),
                       but = c("however", "although", "though", "yet", "nevertheless"),
                       would = c("would", "should", "could", "ought", "might", "must", "need"),
                       amp = c("so", "too", "very", "super", "more", "most"))
keep_stop_words <- as.vector(unlist(stopwords_list[keep_stop_words]))  # Stopwords to keep
stopwords_final <- setdiff(stopwords, keep_stop_words)
stopwords_final <- c(stopwords_final, letters)
article_corpus_final <- Corpus(VectorSource(article_corpus_final))
article_corpus_final <- tm_map(article_corpus_final, removeWords, stopwords_final)
article_docs_final <- article_corpus_final$content
print(article_docs_final[1])

# Feature engineering: Create a Document-Term (DT) Matrix (a matrix that describes the frequency of terms that occur in a collection of documents; rows correspond to documents in the collection, and columns correspond to terms)
options(scipen = 999)  # Suppress scientific notation
article_dtm <- DocumentTermMatrix(article_corpus_final)
show(article_dtm)
class(article_dtm)
findFreqTerms(article_dtm, lowfreq = 75, highfreq = 100)  # Get terms between a specified frequency range
findFreqTerms(article_dtm, lowfreq = 100, highfreq = Inf)
article_dtm <- removeSparseTerms(article_dtm, sparse = 0.99)  # Remove rare terms that are over [100*sparse]% (inclusive) empty
colnames(article_dtm)
head(findFreqTerms(article_dtm, lowfreq = 1, highfreq = 10))

# Sentiment Analysis
positive_words <- c(qdapDictionaries::positive.words, "answer", "solution", "concept", "familiar", "familiarity", 
              "insight", "insights", "experience", "experienced", "thorough", "thoroughly", "genuine", "genuinely", "interested")
negative_words = c(qdapDictionaries::negative.words, "scatter", "scattered", "devolve")

article_polarity <- polarity(article_corp1, polarity.frame = sentiment_frame(positive_words, negative_words))$all
mean(article_polarity$polarity)
article_corp1$polscore <- article_polarity$polarity
# Sentiment score descriptives
mean_sentiment = mean(article_corp1$polscore, na.rm = T)
sd_sentiment = sd(article_corp1$polscore, na.rm = T)
min_sentiment = min(article_corp1$polscore, na.rm = T)
max_sentiment = max(article_corp1$polscore, na.rm = T)
breaks <- c(min_sentiment, mean_sentiment - 1.5*sd_sentiment, mean_sentiment - 0.5*sd_sentiment, mean_sentiment + 0.5*sd_sentiment, mean_sentiment + 1.5*sd_sentiment, max_sentiment)
labels <- c("Strongly Negative", "Negative", "Neutral", "Positive", "Strongly Positive")
article_corp1$sentiment_category <- cut(article_corp1$polscore, breaks, labels, include.lowest = TRUE)
table(article_corp1$sentiment_category)
strong_positive_articles <- which(article_corp1$sentiment_category %in% 'Strongly Positive')
strong_negative_articles <- which(article_corp1$sentiment_category %in% 'Strongly Negative')
unlist(lapply(1:length(strong_positive_articles),
              function(i) {
                eval(as.name(paste0("article", strong_positive_articles[i])))[[1]]
              }
))
unlist(lapply(1:length(strong_negative_articles),
              function(i) {
                eval(as.name(paste0("article", strong_negative_articles[i])))[[1]]
              }
))
