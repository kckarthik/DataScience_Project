tweets.1 <- read.csv("C:\\Users\\KC\\Desktop\\Text_Mining\\Assignments\\Topic 4 _ Sentiment Analysis\\dataset\\tweets.csv",stringsAsFactors = FALSE)
tweets <- tweets.1[,c(2,3)]
names(tweets)
dim(tweets)

#Build a Text Corpus
library(tm)
tweets.corpus<-Corpus(VectorSource(tweets$text))
summary(tweets.corpus)
inspect(tweets.corpus[1:5])#Build a Text Corpus

#Data Transformations -Cleaning
tweets.corpus<-tm_map(tweets.corpus,tolower) #Converting to lower case
tweets.corpus<-tm_map(tweets.corpus,stripWhitespace) #Removing extra white space
tweets.corpus<-tm_map(tweets.corpus,removePunctuation) #Removing punctuations
tweets.corpus<-tm_map(tweets.corpus,removeNumbers) #Removing numbers
my_stopwords<-c(stopwords('english'),'available') #Can add more words apart from standard list
tweets.corpus<-tm_map(tweets.corpus,removeWords,my_stopwords)

setwd("C:\\Users\\KC\\Desktop\\Text_Mining\\Assignments\\Topic 4 _ Sentiment Analysis\\dataset")
tweets.text<-tweets$text
#Read the dictionaries
pos = scan('positive-words.txt',what='character',comment.char=';')
neg = scan('negative-words.txt',what='character',comment.char=';')
#Adding words to dictionaries
pos[2007:2013]<-c("spectacular","everyday","better","top","thumbs","four","five")
neg[4784:4789]<-c("one","two","careful","sync","Beware","suck")


#Famous Jeffreybreen Algorithm to "Tag" sentiments to sentences

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  #we got a vector of sentences. plyr will handle a list
  #or a vector as an "l" for us
  #we want a simple array of scores back, so we use
  #"l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    #clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence) #removes punctuations
    sentence = gsub('[[:cntrl:]]', '', sentence) #removes control characters
    sentence = gsub('\\d+', '', sentence) #removes digits
    
    #and convert to lower case:
    sentence = tolower(sentence)
    
    #split sentences into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    
    #sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    #compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    #match() returns the position of the matched term or NA
    #we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    #and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
analysis<-score.sentiment(tweets.text, pos, neg, .progress="text")
names(analysis)
View(analysis)
str(analysis)

table(analysis$score)
mean(analysis$score)
hist(analysis$score)

analysis$text<-as.character(analysis$text)
str(analysis)
analysis$sentiment<-ifelse(analysis$score>0,"positive"
                           ,ifelse(analysis$score<0,"negative","negative"))
table(analysis$sentiment)

#-------------------------Training and Test Data --------------------------------------#

#Split the data into training and test
sampling<-sort(sample(nrow(analysis), nrow(analysis)*.7))
length(sampling)

head(analysis)
names(analysis)
train_tweets = analysis[sampling,]
test_tweets = analysis[-sampling,]
prop.table(table(train_tweets$sentiment))
prop.table(table(test_tweets$sentiment))

#----------------------------------Classification Algorithms -------------------------------
set.seed(2000)

#Tagging the testing and training datasets
dim(test_tweets)
dim(train_tweets)
names(train_tweets)
names(test_tweets)
train_tweets$type="train"
test_tweets$type="test"
tweets<-rbind(train_tweets,test_tweets)
head(tweets)


#building a dtm

matrix= create_matrix(tweets[,2], language="english",removeNumbers=TRUE, removePunctuation=TRUE, 
                      removeSparseTerms=0, 
                      removeStopwords=TRUE, stripWhitespace=TRUE, toLower=TRUE)
#Convert to a matrix data type
mat = as.matrix(matrix)
#Build the data to specify response variable, training set, testing set.
container = create_container(mat, as.numeric(as.factor(tweets[,3])),
                             trainSize=1:2194, testSize=2195:3135,virgin=FALSE)
#Train the model
#Algorithms used to train the data : SVM,RF,TREE,MAXENT,BAGGING
models = train_models(container, algorithms=c("MAXENT", "SVM"))

#Test the model
results = classify_models(container, models)
class(results)
head(results)
# Accuracy Table
# Confusion Matrix
table(as.numeric(as.factor(tweets[2195:3135,3])), results[,"MAXENTROPY_LABEL"])
table(as.numeric(as.factor(tweets[2195:3135,3])), results[,"SVM_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[2195:3135,3])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[2195:3135,3])), results[,"SVM_LABEL"])


#----------------------------------MODEL SUMMARY---------------------------------------

analytics = create_analytics(container, results)
summary(analytics)
head(analytics@document_summary)

#----------------------------------ENSEMBLE AGREEMENT----------------------------------

#Coverage: simply refers to the percentage of documents that meet the recall accuracy 
#threshold

analytics@ensemble_summary
#--------------------------------CROSS VALIDATION----------------------------
N=4
set.seed(2014)
cross_validate(container,N,"MAXENT")
cross_validate(container,N,"SVM")
results1<-results[,c(1,3)]
#For each row
results1$majority=NA
for(i in 1:nrow(results1))
{
  #Getting the frequency distribution of the classifications 
  print(i)
  p<-data.frame(table(c(results1$MAXENTROPY_LABEL[i],results1$SVM_LABEL[i])))
  #Choosing the classification that occurs maximum
  #Putting this value into the new column "majority"
  
  results1$majority[i]<-paste(p$Var1[p$Freq==max(p$Freq)])
  rm(p)
}
results1$majority<-as.numeric(results1$majority)
table(results1$majority)

