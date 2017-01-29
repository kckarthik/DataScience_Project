setwd("C:\\Users\\KC\\Desktop\\Text_Mining\\Assignments\\Topic 2 _ Tm Package\\Dataset")

#Read the NY Times data

nytimes<-read.csv("nytimes.csv",h=FALSE)
str(nytimes)
names(nytimes)
#Converting to a dataframe ; only second column
nytimes_2 <- data.frame(nytimes$V4)
names(nytimes_2)<- "nytimes_subject"
str(nytimes_2)

#Data Pre-Processing using TM Package
library(tm)
#Building a Text Corpus
#Source for the Corpus
nytimes.corpus <- Corpus(VectorSource(nytimes_2$nytimes_subject))
summary(nytimes.corpus)
inspect(nytimes.corpus[1:5]) #Inspecting elements in Corpus

#Data Transformations
nytimes.corpus<-tm_map(nytimes.corpus,tolower) #Converting to lower case
nytimes.corpus<-tm_map(nytimes.corpus,PlainTextDocument)
nytimes.corpus<-tm_map(nytimes.corpus,stripWhitespace) #Removing extra white space
nytimes.corpus<-tm_map(nytimes.corpus,removePunctuation) #Removing punctuations
nytimes.corpus<-tm_map(nytimes.corpus,removeNumbers) #Removing numbers
my_stopwords<-c(stopwords('english'),'http*') #Can add more words apart from standard list
nytimes.corpus<-tm_map(nytimes.corpus,removeWords,my_stopwords)

#Building term document matrix
nytimes.tdm<-TermDocumentMatrix(nytimes.corpus)
nytimes.tdm
dim(nytimes.tdm) #Dimensions of term document matrix
inspect(nytimes.tdm[1:10,1:10]) #Inspecting the term document matrix

#Removing sparse terms
#Words that occur infrequenctly 
#This function call removes those terms which have 
#at least a 97 percentage of sparse (i.e., terms occurring 0 times in a document) elements
nytimes.imp<-removeSparseTerms(nytimes.tdm,0.97)
nytimes.imp
inspect(nytimes.imp)


#Finding word and frequencies
temp<-inspect(nytimes.imp)
wordFreq<-data.frame(apply(temp, 1, sum))
wordFreq<-data.frame(ST = row.names(wordFreq), Freq = wordFreq[, 1])
head(wordFreq)

wordFreq<-wordFreq[order(wordFreq$Freq, decreasing = T), ]
row.names(wordFreq) <- NULL
View(wordFreq)


##Basic Analyses
#Finding the most frequent terms/words
findFreqTerms(nytimes.tdm,10) #Occuring minimum of 10 times
findFreqTerms(nytimes.tdm,30) #Occuring minimum of 30 times
findFreqTerms(nytimes.tdm,50) #Occuring minimum of 50 times
findFreqTerms(nytimes.tdm,70) #Occuring minimum of 70 times

#Finding association between terms/words
findAssocs(nytimes.tdm,"bush",0.2)
findAssocs(nytimes.tdm,"iraq",0.1)
findAssocs(nytimes.tdm,"new",0.1)

library(wordcloud)
library(RColorBrewer)

display.brewer.all() #Gives you a chart
brewer.pal #Helps you identify the groups of pallete colors
display.brewer.pal(8,"Dark2")

display.brewer.pal(8,"Purples")
display.brewer.pal(3,"Oranges")
pal2<-brewer.pal(8,"Dark2")
wordcloud(nytimes.corpus,min.freq=10,
          max.words=100,
          random.order=T,colors=pal2)
wordcloud(nytimes.corpus,min.freq=10,max.words=70, 
          random.order=T, 
          colors=pal2,vfont=c("script","plain"))


