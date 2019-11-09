#converting "?" to NAs
data[data=="?"]<-Na
#converting 0 and 1 to male and female
data[data$sex==0,]$sex<-"F"
data[data$sex==1,]$sex<-"M"
data$sex<-as.factor(data$sex)


mydata1=read.csv(file.choose(),header = TRUE)
mydata1=na.omit(mydata1)
colnames(mydata1)
colnames(mydata)



mydata$IssuedDateLoan=as.Date(mydata$IssuedDateLoan,format="%d/%m/%Y  %H:%M")
mydata$TransactionStartTime=as.Date(mydata$TransactionStartTime,format="%d/%m/%Y  %H:%M")
Timetopay=mydata$TransactionStartTime-mydata$IssuedDateLoan
View(Timetopay)
mydata=mutate(mydata,Timetopay)
test=mutate(test,IsDefaulted=0)
View(test)
train <- train[, c(1, 2, 3, 4, 5,6,8,7)]
train
colnames(test)
colnames(train)
test <- rbind(train[1, ] , test)
test <- test[-1,]

sum(is.na(df$col))
na_count <-sapply(x, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

time <- as.POSIXct(strptime(c("15:03", "08:01", "11:59", "23:47", "14:20"),"%H:%M"),"UTC")

x=as.POSIXct(strptime(c("050000","105959","110000","155959","160000",
                        "185959"),"%H%M%S"),"UTC")
library(tidyverse)
case_when(
between(time,x[1],x[2]) ~"morning",
between(time,x[3],x[4]) ~"afternoon",
between(time,x[5],x[6]) ~"evening",
TRUE ~"night")



#4. NEURAL NETWORK
library(neuralnet)
nn <- neuralnet(consumption ~ capacity + gasoline + hours,
	data=trainset, hidden=c(2,1), linear.output=TRUE, threshold=0.01)
nn$result.matrix


results <- data.frame(actual = testset$consumption, prediction = nn.results$net.result)
results