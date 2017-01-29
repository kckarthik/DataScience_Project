library(RTextTools)
library(topicmodels)
us_congress <- read.csv("C:\\Users\\KC\\Desktop\\Text_Mining\\Assignments\\Topic 5 _ Topic modelling\\Dataset\\USCongress.csv",stringsAsFactors = FALSE)

#Split the data into training and test
sampling<-sort(sample(nrow(us_congress), nrow(us_congress)*.7))
length(sampling)


train_data = us_congress[sampling,]
test_data = us_congress[-sampling,]

matrix= create_matrix(as.vector(us_congress$text), 
                      language="english",removeNumbers=TRUE, removePunctuation=TRUE, 
                      removeSparseTerms=0, 
                      removeStopwords=TRUE, stripWhitespace=TRUE, toLower=TRUE)
inspect(matrix[1:10,1:10])
inspect(matrix[1:10,1:10])

#Choose number of topics already existing
k <- length(unique(us_congress$major))
Divide Data into training and testing

train <- matrix[1:3114,]
test <- matrix[3115:4449,]

#Building model on train data
train.lda <- LDA(train,k)
get_topics(train.lda,5)
get_terms(train.lda,5)

topics(train.lda)
terms(train.lda)
setwd("C:\\Users\\KC\\Desktop\\Text_Mining\\Assignments\\Topic 5 _ Topic modelling\\Dataset")
write.csv(data.frame(get_terms(train.lda,27)),"terms_uscongress.csv",row.names=F)
#Get the top topics 
train.topics <- topics(train.lda)
#Testing the model
test.topics <- posterior(train.lda,test)

test.topics$topics[1:10,1:5]
test.topics <- apply(test.topics$topics, 1, which.max)
#Joining the predicted Topic number to the original test Data
test<-us_congress[3115:4449,]
final<-data.frame(Title=test$text,Pred_topic=test.topics)
View(final)
#Analysis
table(final$Pred_topic)
View(final[final$Pred_topic==2,])
