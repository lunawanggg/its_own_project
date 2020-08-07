getwd()
setwd('/Users/yuxuanpeng/Desktop/r')
d1=read.csv('wc.csv')

str(d1)

RNGversion(vstr = 3.6)

d <- d1[d1$Review.Text!='',]


################################################################################
# Data loading
################################################################################
# coerce feature types to their correct types for analysis
d$X <- as.numeric(d$X) 
d$Clothing.ID <- as.numeric(d$Clothing.ID) 
d$Age <- as.numeric(d$Age) 
d$Title <- as.factor(d$Title) 
d$Review.Text <- as.character(d$Review.Text) 
d$Rating <- as.numeric(d$Rating) 
d$Recommended.IND <- as.numeric(d$Recommended.IND) 
d$Positive.Feedback.Count <- as.numeric(d$Positive.Feedback.Count) 
d$Division.Name <- as.factor(d$Division.Name) 
d$Department.Name <- as.factor(d$Department.Name)
d$Class.Name <- as.factor(d$Class.Name)


str(d)

################################################################################
## Handling missing values
################################################################################

dim(d[!complete.cases(d),])[[1]]/nrow(d)*100
source("DataQualityReport.R")
DataQualityReport(d)


#we just want to sign the column which is irrelevant
d$X <- NULL
d$Title <- NULL
d$Recommended.IND <- NULL
d$Positive.Feedback.Count <- NULL

library(mice)
imputedValues <- mice(data=d, m=3, method="cart", seed=2016)
rm(imputedValues)
DataQualityReport(d)

save(d,file = ' WC clean data.Rda')
submissionFile = data.frame(d)
write.csv(submissionFile, 'WC clean.csv',row.names = F)



################################################################################
## packages 
################################################################################
install.packages('stringr')
library(stringr)

install.packages('tidytext')
library(tidytext)

install.packages('dplyr')
library(dplyr)

install.packages('tidyr')
library(tidyr)
install.packages('broom')
library(broom)
#install.packages('Zelig')
#library(Zelig)
install.packages('pander')
library(pander)
install.packages('radiant.data')
library(radiant.data)
install.packages('visreg')
library(visreg)


install.packages('purrr')
library(purrr)

install.packages('igraph')
library(igraph)
install.packages('ggplot2')
library(ggplot2)


install.packages('wordcloud2')
library(wordcloud2)
install.packages('ggraph')
library(ggraph)
install.packages('topicmodels')
library(topicmodels)

install.packages('grid')
install.packages('libcoin')
install.packages('mvtnorm')
library(grid)
library(libcoin)
library(mvtnorm)

install.packages('partykit')
library(partykit)
install.packages('gtrendsR')
library(gtrendsR)
install.packages('NLP')
library(NLP)
install.packages('tm')
library(tm)
install.packages('SnowballC')
library(SnowballC)
install.packages('wordcloud')
library(wordcloud)
install.packages('RColorBrewer')
library(RColorBrewer)
install.packages('e1071')
library(e1071)
install.packages('syuzhet')
library(syuzhet)

# Install
install.packages("wesanderson")
# Load
library(wesanderson)
library(ggthemes)

################################################################################
## EDA
################################################################################
##1.Age distribution Ans: Age between 30-45's are more likely to leave reviews at the e-commerce store
ggplot(data = d, aes(x = Age)) + geom_histogram( fill = "blue") 

str(d)
median(d$Rating)#5
mean(d$Rating)#4.183561

#Distribution of Review
ggplot(data=d,aes(x=d$Rating))+
  geom_histogram(color="darkblue", fill="lightblue")+
  theme_economist()+
  coord_flip()

# longest review and shortest review
summary(str_count(string = d$Review.Text,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   3.000   5.000   4.719   6.000  17.000 

cor.test(str_count(string = d$Review.Text,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),d$Rating)
#cor=0.01550835  not cor shows between the length and rating

cor.test(str_count(string = d$Division.Name),d$Rating)
#cor=0.01092212  not cor shows between the length and Division.Name

cor.test(str_count(string = d$Department.Name),d$Rating)
#cor=0.02329851  not cor shows between the length and Department.Name

cor.test(str_count(string = d$Class.Name),d$Rating)
#cor=-0.00564575  not cor shows between the length and Class.Name

cor.test(d$Age,d$Rating)
#cor=0.02996163   not cor shows between the length and Class.Name

# most common words
CW = d
CW%>%
  unnest_tokens(input = Review.Text, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(10)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()+
  geom_bar(stat="identity", colour="black")



##Emotions in Reviews
nrc = get_sentiments('nrc')
CW%>%
  group_by(Clothing.ID)%>%
  unnest_tokens(output = word, input = Review.Text)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=reorder(sentiment,X = n)))+geom_col()+guides(fill=F)+coord_flip()+ 
  geom_bar(stat="identity", colour="black")+
  scale_fill_manual(values=c("#FBEEE6", "#F6DDCC", "#EDBB99",
                            "#E59866","#DC7633","#D35400","#BA4A00","#A04000","#873600","#6E2C00"))
 

##Ratings of all Reviews based on Emotion Expressed
CW%>%
  group_by(Clothing.ID)%>%
  unnest_tokens(output = word, input =Review.Text)%>%
  inner_join(nrc)%>%
  group_by(Clothing.ID,sentiment,Rating)%>%
  count()%>%
  group_by(sentiment, Rating)%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=Rating,y=n,fill=Rating))+
  geom_col()+
  facet_wrap(~sentiment)+ 
  guides(fill=F)+coord_flip()+geom_bar(stat="identity", colour="black")


#Wordcloud
library(wordcloud)
wordcloudData = 
  CW%>%
  group_by(Clothing.ID)%>%
  unnest_tokens(output=word,input=Review.Text)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()
set.seed(2020)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(3,1),max.words = 100,colors=brewer.pal(5, "Reds"))

#Comparison Cloud

wordcloudData = 
  CW%>%
  group_by(Clothing.ID)%>%
  unnest_tokens(output=word,input=Review.Text)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()
rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]

set.seed(2020)
comparison.cloud(term.matrix = wordcloudData,scale = c(4,1),max.words = 100, rot.per=0, colors=brewer.pal(6, "Set1"))








################################################################################
## Predictive analysis --- Prepare Data
################################################################################


##Clean text :convert to lower case,remove urls, punctuation, stopwords,whitespace.
corpus = Corpus(VectorSource(d$Review.Text))
corpus[[4607]][1]
corpus = tm_map(corpus,
                FUN = content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*',
                                                                replacement = ' ',x = x)))
corpus[[4607]][1]
corpus = tm_map(corpus,FUN = removePunctuation)
corpus[[4607]][1]
corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))
corpus[[4607]][1]
corpus = tm_map(corpus,FUN = stripWhitespace)
corpus[[4607]][1]


################################################################################
## Create a dictionary
################################################################################

dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(d$Review.Text))),
                     lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))
#Stem document
corpus = tm_map(corpus,FUN = stemDocument)
corpus[[4607]][1]


################################################################################
## Document term matrix (tokenize)(tf)
################################################################################

dtm = DocumentTermMatrix(corpus)
dtm
inspect(dtm[4607,])
  ##Remove Sparse Terms
xdtm = removeSparseTerms(dtm,sparse = 0.9)
xdtm
  #Complete Stems
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))
sort(colSums(xdtm),decreasing = T)


################################################################################
## Document term matrix (tokenize)(tfidf)
################################################################################

dtm_tfidf = DocumentTermMatrix(x=corpus,
            control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf = removeSparseTerms(dtm_tfidf,sparse = 0.9)
xdtm_tfidf = as.data.frame(as.matrix(xdtm_tfidf))
colnames(xdtm_tfidf) = stemCompletion(x = colnames(xdtm_tfidf),
                                      dictionary = dict_corpus,
                                      type='prevalent')

colnames(xdtm_tfidf) = make.names(colnames(xdtm_tfidf))
sort(colSums(xdtm_tfidf),decreasing = T)


################################################################################
## TF VS TFIDF
################################################################################

#combine tf and tfidf together
d_idf = cbind(review_rating = d$Rating,xdtm)
d_tfidf = cbind(review_rating = d$Rating,xdtm_tfidf)

library(tidyr); library(dplyr); library(ggplot2); library(ggthemes)
data.frame(term = colnames(xdtm),tf = colMeans(xdtm), tfidf = colMeans(xdtm_tfidf))%>%
  arrange(desc(tf))%>%
  top_n(20)%>%
  gather(key=weighting_method,value=weight,2:3)%>%
  ggplot(aes(x=term,y=weight,fill=weighting_method))+
  geom_col(position='dodge')+
  coord_flip()+
  theme_economist()


################################################################################
## TF modeling
################################################################################

set.seed(2020)
split = sample(1:nrow(d_idf),size = 0.7*nrow(d_idf))
train_idf = d_idf[split,]
test_idf = d_idf[-split,]

#CART
library(rpart); library(rpart.plot)
tree_idf = rpart(review_rating~.,train_idf)
rpart.plot(tree_idf)

pred_tree_idf = predict(tree_idf,newdata=test_idf)
rmse_tree_idf = sqrt(mean((pred_tree_idf - test_idf$review_rating)^2)); rmse_tree_idf#1.050775

#idf_reg
reg_idf = lm(review_rating~.,train_idf)
summary(reg_idf)
pred_reg_idf = predict(reg_idf, newdata=test_idf)
rmse_reg_idf = sqrt(mean((pred_reg_idf-test_idf$review_rating)^2)); rmse_reg_idf#0.9965185

################################################################################
## TFIDF modeling
################################################################################

split = sample(1:nrow(d_tfidf),size = 0.7*nrow(d_tfidf))
train_tfidf = d_tfidf[split,]
test_tfidf = d_tfidf[-split,]
#CART
library(rpart); library(rpart.plot)
tree_tfidf = rpart(review_rating~.,train_tfidf)
rpart.plot(tree_tfidf)
pred_tree_tfidf = predict(tree_tfidf,newdata=test_tfidf)
rmse_tree_tfidf = sqrt(mean((pred_tree_tfidf - test_tfidf$review_rating)^2)); rmse_tree_tfidf#1.060996
#tfidf_reg 
reg_tfidf = lm(review_rating~.,train_tfidf)
summary(reg_tfidf)
pred_reg_tfidf = predict(reg_tfidf, newdata=test_tfidf)
rmse_reg_tfidf = sqrt(mean((pred_reg_tfidf-test_tfidf$review_rating)^2)); rmse_reg_tfidf#1.003808





















