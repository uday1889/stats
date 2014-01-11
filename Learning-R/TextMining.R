library(twitteR)
setwd("~/work/Insofe/Jan-11/TextProcessing_BigData")
rm(list = ls())
load("rdmTweets-201306.RData")
rdmTweets <- tweets

#Get Auth working for yourself to use the following
# counsumer.key <- "ITUgWICgzIzfbs32CvPJew"
# consumer.secret <- "nRu0hdXydPpeP6OayO2YkkEbC0yepgfSABQkkLl1XZ8"
# cred <- getTwitterOAuth(counsumer.key, consumer.secret)
# registerTwitterOAuth(cred)
# rdmTweets <- userTimeline("rdatamining", n=100)

#Use the following object loaded from .RData file shared by instructor
head(rdmTweets)

#Create a data.frame from the tweets
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))
dim(df)

library(tm) 
# build a corpus, which is a collection of text documents 
# VectorSource specifies that the source is character vectors. 
myCorpus <- Corpus(VectorSource(df$text))

#Changing the character case, removing punctuations and removing stop words
myCorpus <- tm_map(myCorpus, tolower) 
# remove punctuation 
myCorpus <- tm_map(myCorpus, removePunctuation) 
# remove numbers 
myCorpus <- tm_map(myCorpus, removeNumbers) 

# keep "r" by removing it from stopwords 
myStopwords <- c(stopwords('english'), "available", "via") 
#idx <- which(myStopwords == "r") 
myStopwords <- myStopwords[which(myStopwords != "r") ] 
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)


#Stemming Words:
dictCorpus <- myCorpus 
# stem words in a text document with the snowball stemmers, 
# which requires packages Snowball, RWeka, rJava, RWekajars 
myCorpus <- tm_map(myCorpus, stemDocument) 
# inspect the first three ``documents" 
inspect(myCorpus[1:3])

#Building a Document-Term Matrix
myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1)) 
inspect(myDtm[266:270,31:40])


#Frequent Terms and Associations
findFreqTerms(myDtm, lowfreq=10) 
# which words are associated with "r"? 
findAssocs(myDtm, 'seri', 0.30)


# which words are associated with "mining"? 
# Here "miners" is used instead of "mining", 
# because the latter is stemmed and then completed to "miners". :-( 
findAssocs(myDtm, 'mine', 0.30)


#Building a Word Cloud:

library(wordcloud) 
m <- as.matrix(myDtm) 
# calculate the frequency of words 
v <- sort(rowSums(m), decreasing=TRUE) 
myNames <- names(v) 
k <- which(names(v)=="miners") 
myNames[k] <- "mining" 
d <- data.frame(word=myNames, freq=v) 
wordcloud(d$word, d$freq, min.freq=3)









#Activity 2: Perform the text processing and using clustering of documents on the Reuters data set.
## Text mining with the R package tm
rm(list = ls())

library(tm)
## get a sample (10 documents) of the Reuters dataset (comes with package tm)
reut21578 <- system.file("texts", "crude", package = "tm")

reuters <- Corpus(DirSource(reut21578),
                  readerControl = list(reader = readReut21578XML))
### download reuters21578 data first (use first 1000 documents; 1984/85)
#file <- "reut2-000.xml"
#reuters <- Corpus(ReutersSource(file), readerControl = list(reader = readReut21578XML))
reuters
reuters[[1]]
## Convert to Plain Text Documents
reuters <- tm_map(reuters, as.PlainTextDocument)
reuters[[1]]
## Convert to Lower Case
reuters <- tm_map(reuters, tolower)
reuters[[1]]
## Remove Stopwords
reuters <- tm_map(reuters, removeWords, stopwords("english"))
reuters[[1]]
## Remove Punctuations
reuters <- tm_map(reuters, removePunctuation)
reuters[[1]]
## Stemming
reuters <- tm_map(reuters, stemDocument)
reuters[[1]]
## Remove Numbers
reuters <- tm_map(reuters, removeNumbers)
reuters[[1]]
## Eliminating Extra White Spaces
reuters <- tm_map(reuters, stripWhitespace)
reuters[[1]]
## create a term document matrix
dtm <- DocumentTermMatrix(reuters)
inspect(dtm[1:10, ])

findFreqTerms(dtm, 30)
findAssocs(dtm, "washington", .4)
#washington secretari political reagan republican white regan
# 1.00 0.49 0.46 0.45 0.45 0.42 0.41
#staff strategist
#0.41 0.41
## do tfxidf
dtm_tfxidf <- weightTfIdf(dtm)
inspect(dtm_tfxidf[1:10,])
## do document clustering
### k-means (this uses euclidean distance)
m <- as.matrix(dtm_tfxidf)
rownames(m) <- 1:nrow(m)
### don't forget to normalize the vectors so Euclidean makes sense
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
### cluster into 10 clusters
cl <- kmeans(m_norm, 10)
cl
table(cl$cluster)
### show clusters using the first 2 principal components
plot(prcomp(m_norm)$x, col=cl$cl)
findFreqTerms(dtm[cl$cluster==1], 50)
inspect(reuters[which(cl$cluster==1)])
## hierarchical clustering
library(proxy)
### this is going to take 4-ever (O(n^2))
d <- dist(m, method="cosine")
hc <- hclust(d, method="average")
plot(hc)
cl <- cutree(hc, 20)
table(cl)
findFreqTerms(dtm[cl==1],3)
              
              

