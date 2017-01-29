setwd("C:\\Users\\KC\\Desktop\\Text_Mining\\Assignments\\Topic 7 _ Clustering\\Dataset")
nytimes <- read.csv("nytimes.csv",h=FALSE)
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
nytimes.imp<-removeSparseTerms(nytimes.tdm,0.99)
nytimes.imp
inspect(nytimes.imp)
nytimes.matrix<-as.matrix(nytimes.imp)
#Finding the Distance metric between terms in the document
distmatrix<-dist(scale(nytimes.matrix),method="euclidean")
#Applying the hierarchcal clusterting algorithm
nytimes.h<-hclust(distmatrix,method="ward")

cut<-cutree(nytimes.h, k = 5)
#plot dendogram
plot(nytimes.h,cex=0.1,hang=-1,which.plots = 2,main="Word cluster Dendogram")
install.packages("ggdendro")
library(ggdendro)
ggdendrogram(nytimes.h,cex=0.9)
ggdendrogram(nytimes.h, rotate = TRUE, size = 4, theme_dendro = FALSE, color = "tomato")
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
op = par(bg = "#EFEFEF")
A2Rplot(nytimes.h, k = 7, cex=0.5,boxes = FALSE, col.up = "grey50", col.down = c("green", 
                                                                                "blue", "black","red","yellow","orange","brown"))