library(caret)
library(rpart)


data = read.csv("/Users/giulio/Desktop/Coursera/Machine Learning/week 3/pml-training.csv", na.strings = c("NA", ""))
dim(data)
data_test <- read.csv("/Users/giulio/Desktop/Coursera/Machine Learning/week 3/pml-testing.csv",na.strings = c("NA", ""))
summary(data$classe)
set.seed(100)
library(caret)
inTrain = createDataPartition(y=data$classe, p=0.75, list=FALSE)
training = data[inTrain,]
testing = data[-inTrain,]
dim(training)
# we don't want columns full of NAs
na_test = sapply(training, function(x) {sum(is.na(x))})
table(na_test)

bad_columns = names(na_test[na_test==14400])
training = training[, !names(training) %in% bad_columns]
str(training)

#removing other useless variables
training = training[,-c(1:7)]

# we try with decision trees
model = train(classe~., method="rpart", data=training)
# accuracy seems low, we expect lower on the testing set.

# the model does not seem to predict well, on the 
mean(predict(model, testing) == testing$classe) * 100


answers = rep("A", 20)        
        pml_write_files = function(x){
                n = length(x)
                for(i in 1:n){
                        filename = paste0("problem_id_",i,".txt")
                        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
                }
        }
      
        pml_write_files(answers)
