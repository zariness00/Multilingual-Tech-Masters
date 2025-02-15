install.packages("gutenbergr")
install.packages("tidytext")
library(gutenbergr)
library(tidytext)
library(dplyr)

my_mirror <- "http://mirrors.xmission.com/gutenberg/"


unique(df$author)[startsWith(unique(df$author), "Be")]


#Step 1 Selecting the book
my_book <- gutenberg_download(1576, mirror = my_mirror)


#Step 2 Counting words and bigrams 
words_my_book <- unnest_tokens(my_book, words, text)
x <- count(words_my_book, words, sort = TRUE)
bigrams_my_book <- unnest_tokens(my_book, words, text, token = "ngrams", n =2)
countbigrams <- count(bigrams_my_book, words, sort = TRUE)

#Eliminating NA, they are placed on the top
countbigrams <- countbigrams[-1,]

#Examining the bigram miss lind ans testing dependence using a contigency table
countbigrams[startsWith(countbigrams$words, "miss lind"),]
countbigrams[startsWith(countbigrams$words, "miss "),]
countbigrams[endsWith(countbigrams$words, " lind"),]

m.l <- countbigrams[startsWith(countbigrams$words, "miss lind"),]$n
m.notl <- sum(countbigrams[startsWith(countbigrams$words, "miss "),]$n) - m.l
notm.l <- sum(countbigrams[endsWith(countbigrams$words, " lind"),]$n) - m.l
notm.notl <- sum(countbigrams$n) - m.notl - notm.l - m.l

#Visualizing and computing chi square test
freq <- matrix(c(m.l, m.notl, notm.l, notm.notl), ncol =2, byrow = T)
mosaicplot(freq) #more frequent miss lind than miss smtg, not equally distributed; dependant 
chisq.test(freq)


sum(countbigrams[startsWith(countbigrams$words, "best museum"),])
sum(countbigrams[startsWith(countbigrams$words, "best "),]$n)
sum(countbigrams[endsWith(countbigrams$words, " museum"),]$n)

b.m <- sum(countbigrams[startsWith(countbigrams$words, "best museum"),]$n)
print(b.m)
b.notm <- sum(countbigrams[startsWith(countbigrams$words, "best "),]$n) - b.m
print(b.notm)
notb.m <- sum(countbigrams[endsWith(countbigrams$words, " museum"),]$n) - b.m
notb.notm <- sum(countbigrams$n) - b.notm - notb.m - b.m
freq1 <- matrix(c(b.m, b.notm, notb.m, notb.notm), ncol =2, byrow = T)
mosaicplot(freq1) 
chisq.test(freq1)


#Step 5 Computing entropy for 1000 words

entropy<-c()
for(i in 0:132)
{
  entr <- words_my_book[(i*1000+1):(i*1000+1000), 2]
  char <- unnest_tokens(entr, token, words, token = "characters")
  df.char <- as.data.frame(count(char, token, sort = TRUE))
  df.char$relfreq <- df.char$n/sum(df.char$n)
  df.char$ent <- df.char$relfreq*log2(df.char$relfreq)
  entropy <- c(entropy, -sum(df.char$ent))
}

entropy
plot(entropy)

#Step 7 Confidence Interval 
mean_entropy <- mean(entropy)
sd_entropy <- sd(entropy)
n <- length(entropy)
error_margin <- qt(0.975, df = n-1) * (sd_entropy / sqrt(n))
lower_bound <- mean_entropy - error_margin
upper_bound <- mean_entropy + error_margin
c(lower_bound, upper_bound)

#Step 8 Naive Bayes Classifier
#The sentence to examine "Beasts are not often troubled with imagination"

my_book1 <- my_book[1:3774,]
my_book2 <- my_book[(3774+1):(2*3774),]
my_book3 <- my_book[(2*3774+1):(3*3774),]
my_book4 <- my_book[(3*3774+1):15098,]

compute_probabilities <- function(sentence, sections) {
  sentence_words <- unlist(strsplit(tolower(sentence), "\\s+"))
  class_probs <- c()
  
  for (i in 1:length(sections)) {
    section <- sections[[i]]
    
    words_selection <- unnest_tokens(section, word, text)
    word_freq <- count(words_selection, word, sort = TRUE)
    total_words <- sum(word_freq$n)
    
    prob <- 1
    for (word in sentence_words) {
      word_count <- word_freq %>% filter(word == !!word) %>% pull(n)
      word_count <- ifelse(length(word_count) == 0,1, word_count)
      prob <- prob * (word_count / total_words)
    }
    
    prob <- prob * (1/length(sections))
    class_probs <- c(class_probs, prob)
  }
  class_probs <- class_probs / sum(class_probs)
  return(class_probs)

}
sections <- list(
  my_book1,
  my_book2,
  my_book3,
  my_book4
)
sentence <- "Beasts are not often troubled with imagination."

probabilities <- compute_probabilities(sentence, sections)

probabilities
