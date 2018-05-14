library(tm)
library(readtext)
library(SnowballC)
library(printr)
library(Matrix)
library(plyr)
library(e1071)
setwd("P:/StudiaPL/mow")
hamname <- file.path("P:/StudiaPL/mow/pliki", "ok_3")
spamname <- file.path("P:/StudiaPL/mow/pliki", "neg_3")
docs_ham <- VCorpus(DirSource(hamname))
docs_spam <- VCorpus(DirSource(spamname))
docs_ham <- tm_map(docs_ham, removeWords, stopwords("english"))
docs_ham <- tm_map(docs_ham, removePunctuation)
docs_ham <- tm_map(docs_ham, tolower)
docs_ham <- tm_map(docs_ham, PlainTextDocument)
docs_spam <- tm_map(docs_spam, removeWords, stopwords("english"))
docs_spam <- tm_map(docs_spam, removePunctuation)
docs_spam <- tm_map(docs_spam, tolower)
docs_spam <- tm_map(docs_spam, PlainTextDocument)
dtm_ham <- DocumentTermMatrix(docs_ham)
dtm_spam <- DocumentTermMatrix(docs_spam)
m_ham <- as.matrix(dtm_ham)
m_spam <- as.matrix(dtm_spam)
m_ham_df <- as.data.frame(m_ham, stringsAsFactors = False, row.names = NULL)
m_ham_classes_df <- as.list(rep("ham",nrow(m_ham_df)))
m_spam_df <- as.data.frame(m_spam, stringsAsFactors = False, row.names = NULL)
m_spam_classes_df <- as.list(rep("spam",nrow(m_spam_df)))
rownames(m_ham_df) <- c()
rownames(m_spam_df) <- c()
m_df <- rbind.fill(m_ham_df,m_spam_df)
m_df[is.na(m_df)] <- 0
m_df[] <- lapply(m_df, factor)
m_classes_df <- c(m_ham_classes_df,m_spam_classes_df)
m_classes_df <- as.factor(m_classes_df)
m_classes_df <- unlist(m_classes_df)
nb_classifier <- naiveBayes(m_df, m_classes_df)
prediction <- predict(nb_classifier, m_df, type="raw")
write.csv(prediction, file="predict.csv")








classes_set <- m_classes_df
train_set <- m_df

p_class <- prop.table(table(classes_set))

total <- cbind(train_set, classes_set)
unique_classes <- unique(classes_set)
first_class <- subset(total, classes_set == unique_classes[1])
second_class <- subset(total, classes_set == unique_classes[2])
first_class <- subset(first_class, select = -c(classes_set))
second_class <- subset(second_class, select = -c(classes_set))

freq_zero_per_column_first_class <- ldply(first_class, function(c) sum(c==0))
freq_one_per_column_first_class <- ldply(first_class, function(c) sum(c==1))
p_zero_first_class <- freq_zero_per_column_first_class$V1 / (freq_zero_per_column_first_class$V1+freq_one_per_column_first_class$V1)
p_one_first_class<- freq_one_per_column_first_class$V1 / (freq_zero_per_column_first_class$V1+freq_one_per_column_first_class$V1)

freq_zero_per_column_second_class <- ldply(second_class, function(c) sum(c==0))
freq_one_per_column_second_class <- ldply(second_class, function(c) sum(c==1))
p_zero_second_class <- freq_zero_per_column_second_class$V1 / (freq_zero_per_column_second_class$V1+freq_one_per_column_second_class$V1)
p_one_second_class<- freq_one_per_column_second_class$V1 / (freq_zero_per_column_second_class$V1+freq_one_per_column_second_class$V1)
cols <- colnames(first_class)
probabilities <- cbind(p_one_first_class, p_zero_first_class, p_one_second_class, p_zero_second_class)
rownames(probabilities) <- cols

train <- function(train_set, classes_set) {
  p_class <- prop.table(table(classes_set))
  
  total <- cbind(train_set, classes_set)
  unique_classes <- unique(classes_set)
  first_class <- subset(total, classes_set == unique_classes[1])
  second_class <- subset(total, classes_set == unique_classes[2])
  first_class <- subset(first_class, select = -c(classes_set))
  second_class <- subset(second_class, select = -c(classes_set))
  
  freq_zero_per_column_first_class <- ldply(first_class, function(c) sum(c==0))
  freq_one_per_column_first_class <- ldply(first_class, function(c) sum(c==1))
  p_zero_first_class <- freq_zero_per_column_first_class$V1 / (freq_zero_per_column_first_class$V1+freq_one_per_column_first_class$V1)
  p_one_first_class<- freq_one_per_column_first_class$V1 / (freq_zero_per_column_first_class$V1+freq_one_per_column_first_class$V1)
  
  freq_zero_per_column_second_class <- ldply(second_class, function(c) sum(c==0))
  freq_one_per_column_second_class <- ldply(second_class, function(c) sum(c==1))
  p_zero_second_class <- freq_zero_per_column_second_class$V1 / (freq_zero_per_column_second_class$V1+freq_one_per_column_second_class$V1)
  p_one_second_class<- freq_one_per_column_second_class$V1 / (freq_zero_per_column_second_class$V1+freq_one_per_column_second_class$V1)
  cols <- colnames(first_class)
  probabilities <- cbind(p_one_first_class, p_zero_first_class, p_one_second_class, p_zero_second_class)
  rownames(probabilities) <- cols
  result <- list(p_class=p_class, probabilities=probabilities)
  return (result)
}

bayes <-train(m_df, m_classes_df)
bayes$probabilities["wasted",]

