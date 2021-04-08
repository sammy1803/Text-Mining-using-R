getwd()
setwd("C:/Users")
#installing tm package
install.packages('tm')
library(tm)

#Create Corpus

docs <- Corpus(DirSource("C:/Users/TextMining"))  
docs

#inspect a particular document
writeLines(as.character(docs[[30]]))
getTransformations()

#Content in the docs have for eg. colons and hyphens without spaces between the words, 
#If I used removepunctuation transformer now, the words can combine

#Create the toSpace content transfomer

toSpace<- content_transformer(function(x, pattern) {return(gsub(pattern, " ", x))})

docs<- tm_map(docs, toSpace, "-")
docs<- tm_map(docs, toSpace, ":")

#inspect a particular document if this works
writeLines(as.character(docs[[30]]))

#remove punctuation - replace punctuation marks with " "

docs<- tm_map(docs, removePunctuation)


#inspect a particular document if this works
writeLines(as.character(docs[[30]]))

#non-standard punctuation marks are still remaining
docs<- tm_map(docs, toSpace, "' ")
docs<- tm_map(docs, toSpace, " '")
docs<- tm_map(docs, toSpace, " -")


#Transform to lower case

docs<- tm_map(docs,content_transformer(tolower))

#Strip digits 

docs<- tm_map(docs, removeNumbers)

#remove stop words using the standard list in tm 

docs<- tm_map(docs, removeWords,stopwords("english"))


#Strip whitespaces
docs<-tm_map(docs, stripWhitespace)

#Stemming
install.packages('SnowballC')
#load library
library(SnowballC)
#Stem document
docs <- tm_map(docs,stemDocument)
writeLines(as.character(docs[[30]]))


docs <- tm_map(docs, content_transformer(gsub), pattern = "organiz", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "organis", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub), pattern = "inenterpris", replacement = "enterpris")
docs <- tm_map(docs, content_transformer(gsub), pattern = "team-", replacement = "team")

#Create a document term matrix 

dtm  <- DocumentTermMatrix(docs)


#Summary 

dtm
#This is a 30 x 4209 dimension matrix in which 89% of the rows are zero.

inspect(dtm[1:2,100:1005])
#freq 

freq <- colSums(as.matrix(dtm))

freq

#length of freq 

length(freq)

#create sort order (descending)
ord <- order(freq,decreasing=TRUE)

#inspect most frequently occurring terms
freq[head(ord)]
#inspect least frequently occurring terms
freq[tail(ord)] 

#to include only those words that occur in  3 to 27 documents,also enforced  lower and upper limit to length of the words included (between 4 and 20 characters).

dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20), bounds = list(global = c(3,27))))
dtmr
#freq of new document term matrix
freqr <- colSums(as.matrix(dtmr))

ordr <- order(freqr,decreasing=TRUE)

freqr[head(ordr)]


#list of terms that occur at least a  80 times in the entire corpus

findFreqTerms(dtmr,lowfreq=80)

#correlations
findAssocs(dtmr,"project",0.6)


#graphical analysis using ggplot2

#Barchart analysis

wf= data.frame(term=names(freqr),occurrences=freqr)

library(ggplot2)
p<-ggplot(subset(wf, freqr>100), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#wordcloud analysis
install.packages('wordcloud')
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freqr),freqr, min.freq=70)

#.add color
install.packages('RColorBrewer')
wordcloud(names(freqr),freqr,min.freq=70,colors=brewer.pal(6,"Dark2"))