# I checked where are the files it will read with the 'getwd' command
getwd()

# I set to where the files are with the 'setwd' command
setwd("/Users/HP/Documents")

#read csv file
# %70 train and %30 test data   
# I split all data

diabetes_dataset <- read.csv('diabetes.csv', header = FALSE, sep = ',')
set.seed(2)
K = 9

SAheart_dataset <- read.csv('SAheart.csv', header = FALSE, sep = ',')
set.seed(2)
K = 9


#Diabet dataset
diabetes_dataset <- diabetes_dataset[sample(nrow(diabetes_dataset)),]
diabetes_training <- diabetes_dataset[1:as.integer(0.7*30),]
diabetes_test <- diabetes_dataset[as.integer(0.7*30 + 1):30,]
actual_outcomes <- diabetes_test$V9

#SAheart dataset
SAheart_dataset <- SAheart_dataset[sample(nrow(SAheart_dataset)),]
SAheart_training <- SAheart_dataset[1:as.integer(0.7*30),]
SAheart_test <- SAheart_dataset[as.integer(0.7*30 + 1):30,]
actual_outcomes_SAheart <- SAheart_test$V10

#my model

KNN_Model <- function(testSet, trainingSet, k){
  predictions <- c() 
  
  for(i in c(1:nrow(testSet))){
    YES = 0
    NO = 0
    distances = c()          
    classifications = c()
    
    for(j in c(1:nrow(trainingSet))){
      
      #I calculated the distance of each data point to each other
      distances <- c(distances, euclidean_distance(testSet[i,], trainingSet[j,]))
      
      #After taking the distances, I took the closest points and ordered them
      classifications <- c(classifications, as.numeric(trainingSet[j,][[9]]))
    }
    
    #The first 9 is taken after taking the distances and sorting them. Because the value of k is 9
    distance_table <- data.frame(classifications, distances)
    distance_table <- distance_table[order(distance_table$distances),]       
    distance_table <- distance_table[1:k,]               
    
    for(k in c(1:nrow(distance_table))){
      if(as.numeric(distance_table[k,"classifications"]) == "1"){
        YES = YES + 1
      }
      else 
        NO = NO + 1
    }
    
    
    
    if(YES > NO){          
      predictions <- c(predictions, "1")
    }
    else if(YES < NO){
      predictions <- c(predictions, "0")
    }
    
  }
  
  return(predictions) 
  
}

#Teacher, I tried with different distances while I was calculating.
#and i calculated the accuracys of each but i used euclidean in my main model
#because in others accuracy output 0.22222

#manhattan_distance <- function(test, train){
 # n <- nrow(test)
  #data <- rbind(test,train)
  #distance.1 <- as.matrix(dist(data,method="manhattan"))
  #distance.m <- matrix(distance.1[1:n,(n+1):ncol(distance.1)],nrow=n,ncol=nrow(train))
  #return(distance.m)
#}

euclidean_distance <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

#minkowski_distance <- function(test, train){
  #n <- nrow(test)
  #data <- rbind(test,train)
  #distance.1 <- as.matrix(dist(data,method="minkowski",p=3))
  #distance.m <- matrix(distance.1[1:n,(n+1):ncol(distance.1)],nrow=n,ncol=nrow(train))
  #return(distance.m)
#}


#FINAL RESULTS
predictions <- KNN_Model(diabetes_test, diabetes_training, K)
predictions_SAheart <- KNN_Model(SAheart_test, SAheart_training, K)

#I find confusion matrix actual_outcomes --> $V9 actual_outcomes_SAheart --> $V10
confusion_matrix = as.matrix(table(Actual = actual_outcomes, Predicted = predictions))
confusion_matrix_SAheart = as.matrix(table(Actual = actual_outcomes_SAheart, Predicted = predictions_SAheart))
print(confusion_matrix)
print(confusion_matrix_SAheart)

#I calculated accuracy for diabetes and SAheart
accuracy = sum(diag(confusion_matrix))/length(actual_outcomes)
accuracy_SAheart = sum(diag(confusion_matrix_SAheart))/length(actual_outcomes_SAheart)
print(accuracy)
print(accuracy_SAheart)

