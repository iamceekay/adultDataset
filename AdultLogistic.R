setwd("C:/Users/chand/Desktop/DataScienceClass/AdultDataSet")
getwd()
library(data.table)



#returning column  list
getColumnName <- function(inputFile,splitter){
  con  <- file(inputFile, open = "r")
  dataList <- list()
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0){
    myVector <- unlist(strsplit(oneLine, splitter))[1]
    dataList <- c(dataList,myVector)
  }
  close(con)
  return (dataList)
}
#read data from URL
readfromURL <- function(urlPath){
  test <- read.csv(
    urlPath,
    header=FALSE)
  return(test)
}

#reading Multiple CSV file with Header
multipleCSVFile <- function(folderPath){
file_list <- list.files(path=folderPath, pattern="*.csv")
for (i in 1:length(file_list)){
  assign(file_list[i],
         read.csv(paste(folder, file_list[i], sep=''))
  )}
}

#Running Chisquare for input column
allChiSqData <- function(adultDataWorking,chisqdata){
  significantColumn <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors=FALSE)
  significantColumnTemps <- NA
for (i in chisqdata) {
   myChiTable<-table(as.factor(adultDataWorking$finalSalary), adultDataWorking[,i])
                     dataRLList<- chisq.test(myChiTable)
                     pVal<- round(dataRLList$p.value,digits = 2)
                     if(pVal < 0.05)
                     {
                       significantColumnTemps <- c(i,pVal,"Significant")
                       print(paste(i,"Significant"))
                     }
                     else{
                       significantColumnTempi <- c(i,pVal,"INSignificant")
                     }
                     significantColumn <- rbind(significantColumn,significantColumnTemps)
                    
   
 }
  colnames(significantColumn) <- c("columnName","pval","Type")
  return(significantColumn)
  
}
#function to find Cutoff
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

inputFile <- "C:/Users/chand/Desktop/DataScienceClass/AdultDataSet/columndata.txt"
adultData <- readfromURL(urlPath = "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
colnames(adultData) <- getColumnName(inputFile = inputFile,splitter = ":")
adultDataWorking <- adultData
colnames(adultDataWorking)[ncol(adultDataWorking)] <- 'finalSalary'
#Check for incomplete row
colSums(is.na(adultDataWorking))
incompleteRows <- nrow(adultDataWorking[!complete.cases(adultDataWorking) , ])
missingTotal <- incompleteRows/nrow(adultDataWorking)*100
chisqdata <- c("workclass","education","age","sex","occupation","hours-per-week","native-country")
#check All Significant Value
significantData<- allChiSqData(adultDataWorking,chisqdata)
levels(adultDataWorking$workclass)
levels(as.factor(adultDataWorking$age))
table(adultDataWorking$occupation)
gsub(" ", "", adultDataWorking, fixed = TRUE)
adultDataWorking[, "education"] <- as.factor(ifelse(adultDataWorking[ ,"education"] == " Bachelors","Bachelor",
                                                    ifelse(adultDataWorking[ ,"education"] == " HS-grad","HighSchool"
                                                    ,ifelse(adultDataWorking[ ,"education"] == " Masters","Master",
                                                            ifelse(adultDataWorking[ ,"education"] == " Some-college","Bachelor" ,"Preschool")))))

                                             
adultDataWorking[, "workclass"] <- as.factor(ifelse(adultDataWorking[ ,"workclass"] ==  " Private","Private", 
                                                    ifelse(adultDataWorking[ ,"workclass"] == " Local-gov","Gov",
                                                           ifelse(adultDataWorking[ ,"workclass"] == " Self-emp-inc" ,"SelfEmp","Others"))))
prop.table(table(Test$finalSalary))
newDataTest <- adultDataWorking[,c("education", "workclass", "sex" ,"finalSalary")]
install.packages(pkgs = "caret", dependencies = c("Depends", "Imports"))
install.packages('magic', dependencies=TRUE)
library(caret)
index <- createDataPartition(newDataTest$finalSalary , p =0.7,list = FALSE)
Train <- newDataTest[index, ]
Test <- newDataTest[-index, ]
## # Building my adultData model. including all the variables
adult_model <- glm(finalSalary ~ . , data = Train[1:1000,] , family=binomial(link='logit'))
summary(adult_model)
install.packages('ResourceSelection', dependencies=TRUE)
library(ResourceSelection)
#fitness Test higher p better the value

hoslem.test(adult_model$y, fitted(adult_model))
install.packages('proto', dependencies=TRUE)

install.packages(pkgs = "Deducer", dependencies = c("Depends", "Imports"))
install.packages(pkgs = "ROCR", dependencies = c("Depends", "Imports"))
library(ROCR)
library(nls2)
library(Deducer)
bruteforce(adult_model)
install.packages(pkgs = "effects", dependencies = c("Depends", "Imports"))
install.packages("effects", dependencies=TRUE, repos='http://cran.rstudio.com/')
Test$rankP <- predict(adult_model, newdata = Test, type = "response")
Test$rankV <- ifelse(Test$rankP >= 0.23 ,1,0)
Train$rankv <- predict(adult_model, newdata = Train, type = "response")
Train$rankv <- ifelse(Train$rankv >= 0.23 ,1,0)
mytable_Test<- table(Test$finalSalary, Test$rankV)
mytable_Train<- table(Train$finalSalary, Train$rankV)
paste("The accuracy of model is Test : ", round(sum(diag(mytable_Test))/sum(mytable_Test)*100, 2), "%", sep = "")
paste("The accuracy of model is Train : ", round(sum(diag(mytable_Train))/sum(mytable_Train)*100, 2), "%", sep = "")
pr <- prediction(Train$rankP, Train$finalSalary)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
pr <- prediction(Train$rankv, Train$finalSalary)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

