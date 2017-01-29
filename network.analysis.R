setwd("C:\\Users\\KC\\Desktop\\Text_Mining\\Assignments\\Topic 6 _ Network analysis\\dataset")
library(igraph)
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

#Removing sparse terms
#Words that occur infrequenctly 
#This function call removes those terms which have 
#at least a 97 percentage of sparse (i.e., terms occurring 0 times in a document) elements
nytimes.imp<-removeSparseTerms(nytimes.tdm,0.99)
nytimes.imp
inspect(nytimes.imp)
#nytimes.tdm <- TermDocumentMatrix(nytimes.corpus, 
                                #control=list(wordLengths=c(1, Inf)))
nytimes.m <- as.matrix(nytimes.imp)
nytimes.m[nytimes.m>=1] <- 1
#Build a Term Adjacency matrix
nytimes.m2 <- nytimes.m %*% t(nytimes.m)
nytimes.m2[1:20,1:20]
nytimes.m3<-nytimes.m2[1:20,1:20]
nytimes.g <- graph.adjacency(nytimes.m3,weighted="TRUE",mode="undirected")
#Remove Loops
nytimes.g<-simplify(nytimes.g)
#Vertices
V(nytimes.g)
E(nytimes.g)
#Number of edges  & vertices
ecount(nytimes.g)
vcount(nytimes.g)

#List of connections
E(nytimes.g)$weight
E(nytimes.g)$weight <- runif(ecount(nytimes.g))

# Number of Connections for each person
degree(nytimes.g,mode="out",loops = TRUE)
# set labels and degrees of vertices
V(nytimes.g)$label <- V(nytimes.g)$name
V(nytimes.g)$degree <- degree(nytimes.g)
# plot layout fruchterman.reingold
layout1 <- layout.fruchterman.reingold(nytimes.g,dim=2)
plot(nytimes.g,layout=layout1, vertex.size=2, vertex.label.color="darkred")

plot(nytimes.g,layout=layout.kamada.kawai)
#Adding more modifications

V(nytimes.g)$label.cex <- 2 * V(nytimes.g)$degree/ max(V(nytimes.g)$degree)
V(nytimes.g)$label.color <- rgb(0, 0, .2, .8)
V(nytimes.g)$label.color <- "blue"
V(nytimes.g)$frame.color <- NA

plot(nytimes.g, layout=layout1,
     vertex.color="red")



