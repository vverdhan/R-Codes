

#########

install.packages("xlsx")
library(xlsx)

rm(list=ls())
setwd("D:/Vaibhav/Zhe Consulting/Upwork/Credit Creditibility")

#create empty data frame here

consolidatedData <- read.xlsx("Book1 - Self.xlsx", 1, header = TRUE)

#consolidatedData <- read.xlsx("Book98.xlsx", 1, header = TRUE)

#tempFileName<-paste("Book",98,".xlsx", sep="")

file.exists(tempFileName)

nrow(consolidatedData)  
for (i in 1:264)

{
  tempFileName<-paste("Book",i,".xlsx", sep="")
  #print(tempFileName)
  if(file.exists(tempFileName))
  {
    myData <- read.xlsx(tempFileName, 1, header = TRUE)
    
    customerDetails <- myData[nrow(myData),]
    
    myData<- myData[-nrow(myData),]
    
    customerDetails<-customerDetails[2]
    
    customerDetailsRepeat<-rep(customerDetails,length(nrow(myData)))
    
    tempData<-cbind(myData,customerDetailsRepeat)
    
    tempData<-subset(tempData, tempData$Detail.!="Closing Balance ")
    
    tempData<-subset(tempData, tempData$Detail.!="Closing Balance")
    
    finalData <- tempData[,c(1,2,3,4,7)]
    
    
    if(nrow(consolidatedData) == 1)
    {
      #print(nrow(finalData))
      consolidatedData <- finalData
      #print(consolidatedData)
      #print(nrow(consolidatedData))
    }
    else
    {
      #print(nrow(finalData))
      consolidatedData <- rbind(consolidatedData,finalData)
      #print(nrow(consolidatedData))
    }
    
  }
  
}


write.csv(consolidatedData, file = "consolidatedDataFile.csv")

#do some manual pre-processing here

