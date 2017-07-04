library(shiny)
library(forecast)
library(ggplot2)
#Finding out the working directory and setting that directory to that contains the files
#reading files using the read.table function
getwd()
setwd("/home/marvin/Documents/use stockprices/csvfiles")
#Assigning the vector containing the list of the stock files to the variable called Stockfiles
Stockfiles<- list.files(path = "/home/marvin/Documents/use stockprices/csvfiles...")
Stockfiles<- list.files()
FileContent <- do.call(rbind,lapply(Stockfiles, function(x)read.csv(x , header = TRUE, colClasses=c("character") ,quote="\"", stringsAsFactors= TRUE, strip.white = TRUE)))
######################################################################
#security variable for company names
#remove all the missing columns and values from the security column
names(FileContent)
###########################################################
dim(FileContent)
str(FileContent)
nrow(FileContent)
ncol(FileContent)
is.na(FileContent)



##################################################################


FileContent$X
Xm <- matrix(FileContent$X, nrow=19 ) 
SecurityCol<-tail(Xm,-5)
Security<-head(SecurityCol,-6)
Security

#####################################################################
#Shares Traded variable
#remove the head and the tail valuesof the shares trade
Xm <- matrix(FileContent$X.1, nrow=19 )
SharesTrade<-tail(Xm,-5)
SharesTraded<-head(SharesTrade,-6)

SharesTraded
#SharesTraded[SharesTraded=='NIL'] <- 0 this comand also replaces
#replace the values with of NIL with 0 and assign them to a variable called Replace_var
Replace_vary<-replace(SharesTraded, SharesTraded=="NIL", 0)
Replace_var<-replace(Replace_vary, Replace_vary=="Nil", 0)
Replace_var

#convert the vector  from character to numeric and assign it to a variable called TradedShares
TradedShare<-as.numeric(Replace_var)
TradedShare
TradedShares<-structure(TradedShare, dim = c(8L,82L))

BATUSales <- sapply(1:82,function(i) TradedShares[,i][1])
plot(BATUSales, type = "o" , col="blue", xlim=c(1,82))
title(main="Year Stock traded by BATU", col.main="Blue", font.main=4)

BOBUSales <- sapply(1:82,function(i) TradedShares[,i][2])
plot(BOBUSales, type = "o" , col="blue", xlim=c(1,82))
title(main="Year Stock traded by BOBU", col.main="Blue", font.main=4)

DFCUSales <- sapply(1:82,function(i) TradedShares[,i][3])
plot(DFCUSales, type = "o" , col="blue", xlim=c(1,82))
title(main="Year stock tradings by DFCU", col.main="Blue", font.main=4)

NICSales <- sapply(1:82,function(i) TradedShares[,i][4])
plot(NICSales, type = "o" , col="blue", xlim=c(1,82))
title(main="Year stock tradings by NIC", col.main="Blue", font.main=4)

NVLSales <- sapply(1:82,function(i) TradedShares[,i][5])
plot(NVLSales, type = "o" , col="blue", xlim=c(1,82))
title(main="Year stock tradings by NVL", col.main="Blue", font.main=4)

SBUSales <- sapply(1:82,function(i) TradedShares[,i][6])
plot(SBUSales, type = "o" , col="blue", xlim=c(1,82))
title(main="Year stock tradings by SBU", col.main="Blue", font.main=4)

UCLSales <- sapply(1:82,function(i) TradedShares[,i][7])
plot(UCLSales, type = "o" , col="blue", xlim=c(1,82))
title(main="Year stock tradings by UCL", col.main="Blue", font.main=4)

UMEMESales <- sapply(1:82,function(i) TradedShares[,i][8])
plot(UMEMESales, type = "o" , col="blue", xlim=c(1,82))
title(main="Year stock tradings by UMEME", col.main="Blue", font.main=4)

AverageSales<-c(mean(BATUSales),mean(BOBUSales),mean(NICSales),mean(DFCUSales),mean(NVLSales),mean(SBUSales),mean(UCLSales),mean(UMEMESales))
barplot(AverageSales, main= "Average Sales of the companies",border = "blue", xlab= "Securities", ylab="Average Price", names.arg= Security[,1],density=c(10,20,30,40,50))

###################################################
#opening price variable
Xm <- matrix(FileContent$X.3, nrow=19 )
Price<-tail(Xm,-5)
OpenPrice<-head(Price,-6)
OpenPrice

#convert the vector  from character to numeric and assign it to a variable called TradedShares
OpeningPrice<-as.numeric(OpenPrice)
OpeningPrices<-structure(OpeningPrice, dim = c(8L,82L))


BATUPrices<- sapply(1:82,function(i) OpeningPrices[,i][1])
BATU<-mean(BATUPrices)

BOBUPrices <- sapply(1:82,function(i) OpeningPrices[,i][2])
BOBU<-mean(BOBUPrices)

DFCUPrices <- sapply(1:82,function(i) OpeningPrices[,i][3])
DFCU<-mean(DFCUPrices)

NICPrices <- sapply(1:82,function(i) OpeningPrices[,i][4])
NIC<- mean(NICPrices)

NVLPrices <- sapply(1:82,function(i) OpeningPrices[,i][5])
NVL<- mean(NVLPrices)

SBUPrices <- sapply(1:82,function(i) OpeningPrices[,i][6])
SBU <- mean(SBUPrices)

UCLPrices <- sapply(1:82,function(i) OpeningPrices[,i][7])
UCL<- mean(UCLPrices)

UMEMEPrices <- sapply(1:82,function(i) OpeningPrices[,i][8])
UMEME<- mean(UMEMEPrices)

AveragePrices<-rbind(BATU,BOBU,DFCU,NIC,NVL,SBU,UCL,UMEME)
AveragePrice<-as.vector(AveragePrices) 
barplot(AveragePrice, main= "Average Prices of the stock",border = "blue", xlab= "Securities", ylab="Average Price", names.arg= Security[,1],density=c(10,20,30,40,50))

###################################################
#prediction of sales and prices for the BATU stock

m<-cbind(BATUSales,BATUPrices)
p<-as.vector(m)
p
BATUData<-structure(p, dim = c(82L,2L), .Dimnames = list(NULL, c("Sales", "Prices")),.Tsp = c(2009, 2015.75, 12),class = c("mts", "ts", "matrix"))
plot(BATUData)

#############################################################
m<-cbind(BATUSales,BATUPrices)
p<-as.vector(m)
p
BATUData<-structure(p, dim = c(82L,2L), .Dimnames = list(NULL, c("Sales", "Prices")),.Tsp = c(2009, 2015.75, 12),class = c("mts", "ts", "matrix"))
plot(BATUData)

#method one for predicting sales

moving_average = forecast(ma(BATUData[1:77,1], order=3), h=5)
moving_average_accuracy = accuracy(moving_average, BATUData[78:82])
moving_average; moving_average_accuracy
plot(moving_average, ylim=c(0,65700))
lines(BATUData[1:82,1])
max(BATUData[1:82,1])

#method two for predicting sales

exp <- ses(BATUData[1:77,1], 5, initial="simple")
exp_accuracy = accuracy(exp, BATUData[78:82])
exp; exp_accuracy
plot(exp, ylim=c(0,65700))
lines(BATUData[1:82,1])

#method three for predicting sales

train = BATUData[1:77,1]
test = BATUData[1:82,1]
arma_fit <- auto.arima(train)
arma_forecast <- forecast(arma_fit, h = 5)
arma_fit_accuracy <- accuracy(arma_forecast, test)
arma_fit; arma_forecast; arma_fit_accuracy
plot(arma_forecast, ylim=c(0,65700))
lines(BATUData[1:82,1])

#method one for predicting price

moving_average = forecast(ma(BATUData[1:77,2], order=3), h=5)
moving_average_accuracy = accuracy(moving_average, BATUData[78:82])
moving_average; moving_average_accuracy
plot(moving_average, ylim=c(0,30000))
lines(BATUData[1:82,2])


#method two for predicting sales

exp <- ses(BATUData[1:77,2], 5, initial="simple")
exp_accuracy = accuracy( exp, BATUData[78:82])
exp; exp_accuracy
plot(exp, ylim=c(0,30000))
lines(BATUData[1:82,2])

#method three for predicting sales

train = BATUData[1:77,2]
test = BATUData[1:82,2]
arma_fit <- auto.arima(train)
arma_forecast <- forecast(arma_fit, h = 5)
arma_fit_accuracy <- accuracy(arma_forecast, test)
arma_fit; arma_forecast; arma_fit_accuracy
plot(arma_forecast, ylim=c(0,30000))
lines(BATUData[1:82,2])
#########################################################################
r<-cbind(BOBUSales,BOBUPrices)
y<-as.vector(r)
BOBUData<-structure(y, dim = c(82L,2L), .Dimnames = list(NULL, c("Sales", "Prices")),.Tsp = c(2009, 2015.75, 12),class = c("mts", "ts", "matrix"))
plot(BOBUData)

########################################################################################
m<-cbind(DFCUSales,DFCUPrices)
p<-as.vector(m)
DFCUData<-structure(p, dim = c(82L,2L), .Dimnames = list(NULL, c("Sales", "Prices")),.Tsp = c(2009, 2015.75, 12),class = c("mts", "ts", "matrix"))
plot(DFCUData)


#####################################################################################
m<-cbind(NICSales,NICPrices)
p<-as.vector(m)
NICData<-structure(p, dim = c(82L,2L), .Dimnames = list(NULL, c("Sales", "Prices")),.Tsp = c(2009, 2015.75, 12),class = c("mts", "ts", "matrix"))
plot(NICData)




########################################################################

m<-cbind(NVLSales,NVLPrices)
p<-as.vector(m)
NVLData<-structure(p, dim = c(82L,2L), .Dimnames = list(NULL, c("Sales", "Prices")),.Tsp = c(2009, 2015.75, 12),class = c("mts", "ts", "matrix"))
plot(NVLData)


####################################################################
m<-cbind(SBUSales,SBUPrices)
p<-as.vector(m)
SBUData<-structure(p, dim = c(82L,2L), .Dimnames = list(NULL, c("Sales", "Prices")),.Tsp = c(2009, 2015.75, 12),class = c("mts", "ts", "matrix"))
plot(SBUData)


############################################################################
m<-cbind(UCLSales,UCLPrices)
p<-as.vector(m)
UCLData<-structure(p, dim = c(82L,2L), .Dimnames = list(NULL, c("Sales", "Prices")),.Tsp = c(2009, 2015.75, 12),class = c("mts", "ts", "matrix"))
plot(UCLData)

#########################################################
m<-cbind(UMEMESales,UMEMEPrices)
p<-as.vector(m)
UMEMEData<-structure(p, dim = c(82L,2L), .Dimnames = list(NULL, c("Sales", "Prices")),.Tsp = c(2009, 2015.75, 12),class = c("mts", "ts", "matrix"))
plot(UMEMEData)


####################################################
#closing price variable
Xm <- matrix(FileContent$X.5, nrow=19 )
CPrice<-tail(Xm,-5)
ClosePrice<-head(Price,-6)
ClosePrice

#convert the vector  from character to numeric and assign it to a variable called ClosingPrice
ClosingPrice<-as.numeric(ClosePrice)
ClosingPrices<-structure(ClosingPrice, dim = c(8L,82L))


BATUCPrices<- sapply(1:82,function(i) ClosingPrices[,i][1])
BATUCPrices

BOBUCPrices <- sapply(1:82,function(i) ClosingPrices[,i][2])
BOBUCPrices

DFCUCPrices <- sapply(1:82,function(i) ClosingPrices[,i][3])
DFCUCPrices

NICCPrices <- sapply(1:82,function(i) ClosingPrices[,i][4])
NICCPrices

NVLCPrices <- sapply(1:82,function(i) ClosingPrices[,i][5])
NVLCPrices

SBUCPrices <- sapply(1:82,function(i) ClosingPrices[,i][6])
SBUCPrices

UCLCPrices <- sapply(1:82,function(i) ClosingPrices[,i][7])
UCLCPrices

UMEMECPrices <- sapply(1:82,function(i) ClosingPrices[,i][8])
UMEMECPrices

########################################################
#Low price variable
Xm <- matrix(FileContent$X.7, nrow=19 )
LPrice<-tail(Xm,-5)
LowPrice<-head(LPrice,-6)
LowPrice

#convert the vector  from character to numeric and assign it to a variable called Low
Low<-as.numeric(LowPrice)
Low

##########################################################
#Date variable in the files

Xm <- matrix(FileContent$X.9, nrow=19 )
Xm[,4]
################################################
#High price variable

Xm <- matrix(FileContent$X.10, nrow=19 )
HPrice<-tail(Xm,-5)
HighPrice<-head(HPrice,-6)
HighPrice

#convert the vector  from character to numeric and assign it to a variable called High
High<-as.numeric(HighPrice)
High

#########################################################################
#P/E Ratio price variable
Xm <- matrix(FileContent$X.12, nrow=19 )
RPrice<-tail(Xm ,-5)
RatioPrice<-head(RPrice,-6)
RatioPrice

#convert the vector  from character to numeric and assign it to a variable called PERatio
PERati<-as.factor(RatioPrice)
PERatio<-structure(PERati, dim = c(8L,82L))


BATUPERATIO<- sapply(1:82,function(i) PERatio[,i][1])
BATUPERATIO

BOBUPERATIO <- sapply(1:82,function(i) PERatio[,i][2])
BOBUPERATIO

DFCUPERATIO <- sapply(1:82,function(i) PERatio[,i][3])
DFCUPERATIO

NICPERATIO <- sapply(1:82,function(i) PERatio[,i][4])
NICPERATIO

NVLPERATIO <- sapply(1:82,function(i) PERatio[,i][5])
NVLPERATIO

SBUPERATIO <- sapply(1:82,function(i) PERatio[,i][6])
SBUPERATIO

UCLPERATIO <- sapply(1:82,function(i) PERatio[,i][7])
UCLPERATIO

UMEMEPERATIO <- sapply(1:82,function(i) PERatio[,i][8])
UMEMEPERATIO

#############################################################################
#Market Cap(Ugx Bn) price variable
Xm <- matrix(FileContent$X.14, nrow=19 )
Market<-tail(Xm,-5)
MarketTotal<-head(Market,-6)
MarketTotal
#convert the vector  from character to numeric and assign it to a variable called PERatio
MarketCap<-as.factor(MarketTotal)
MarketCapital<-structure(MarketCap, dim = c(8L,82L))


BATUMarketCap<- sapply(1:82,function(i) MarketCapital[,i][1])

BOBUMarketCap <- sapply(1:82,function(i) MarketCapital[,i][2])

DFCUMarketCap <- sapply(1:82,function(i) MarketCapital[,i][3])

NICMarketCap <- sapply(1:82,function(i) MarketCapital[,i][4])

NVLMarketCap <- sapply(1:82,function(i) MarketCapital[,i][5])

SBUMarketCap <- sapply(1:82,function(i) MarketCapital[,i][6])

UCLMarketCap <- sapply(1:82,function(i) MarketCapital[,i][7])

UMEMEMarketCap <- sapply(1:82,function(i) MarketCapital[,i][8])



#######################################################
#plotting of the variable
#ploting a bar graph for the sales against the security
Security
is.factor(BATUMarketCap)
AverageSales<-c(mean(BATUMarketCap),mean(BOBUSales),mean(NICSales),mean(DFCUSales),mean(NVLSales),mean(SBUSales),mean(UCLSales),mean(UMEMESales))
barplot(AverageSales, main= "Average Sales of the companies",border = "blue", xlab= "Securities", ylab="Average Price", names.arg= Security[,1],density=c(10,20,30,40,50))

B<-as.vector.factor(BATUMarketCap)
BATUMarketCapMean<-mean(B)
BATUMarketCapMean
BO<-as.vector(BOBUMarketCap)
BATUMarketCapMean<-mean(BO)
D<-as.vector(DFCUMarketCap)
DFCUMarketCapMean<-mean(D) 
NI<-as.vector(NICMarketCap)
NICMarketCapMean<-mean(NI)
NV<-as.vector(NVLMarketCap)
NVLMarketCapMean<-mean(NV) 
S<-as.vector(SBUMarketCap)
SBUMarketCapMean<-mean(S) 
U<-as.vector(UCLMarketCap)
UCLMarketCapMean<-mean(U) 
UM<-as.vector(UMEMEMarketCap)
UMEMEMarketCapMean<-mean(UM)

shinyServer(
  function(input,output){
    
    #this is a bar graph
    output$myhist <- renderPlot({
      barplot(AveragePrice, main= "Average Prices of the stock",border = "blue", xlab= "Securities", ylab="Average Price", names.arg= Security[,1],density=c(10,20,30,40,50))
      
      
    })
    output$myhist1 <- renderPlot({
      barplot(AverageSales, main= "Average Sales of the companies",border = "blue", xlab= "Securities", ylab="Average Sales", names.arg= Security[,1],density=c(10,20,30,40,50))
      
    })
    output$myhist2 <- renderPlot({
      data <- switch(input$dist1,
                     BATU = BATUSales,
                     BOBU = BOBUSales,
                     DFCU = DFCUSales,
                     NIC = NICSales,
                     NVL = NVLSales,
                     SBU = SBUSales,
                     UCL = UCLSales,
                     UMEME = UMEMESales,
                     BATUSales)
      
      plot(data, type = "o" , col="blue", xlim=c(1,82),xlab="Days of Trading", ylab="Shares Traded")
      if (data == BATUSales) {
        title(main="Year Stock traded by BATU", col.main="Blue", font.main=4)
      } else if (data == BOBUSales) {
        title(main="Year Stock traded by BOBU", col.main="Blue", font.main=4)
      } else if (data == DFCUSales) {
        title(main="Year stock tradings by DFCU", col.main="Blue", font.main=4)
      } else if (data == NICSales) {
        title(main="Year stock tradings by NIC", col.main="Blue", font.main=4)
      } else if (data == NVLSales) {
        title(main="Year stock tradings by NVL", col.main="Blue", font.main=4)
      } else if (data == SBUSales) {
        title(main="Year stock tradings by SBU", col.main="Blue", font.main=4)
      } else if (data == UCLSales) {
        title(main="Year stock tradings by UCL", col.main="Blue", font.main=4)
      } else
        title(main="Year stock tradings by UMEME", col.main="Blue", font.main=4)
      
    })
    output$myhist3 <- renderPlot({
      data <- switch(input$dist2,
                     BATU = BATUSales,
                     BOBU = BOBUSales,
                     DFCU = DFCUSales,
                     NIC = NICSales,
                     NVL = NVLSales,
                     SBU = SBUSales,
                     UCL = UCLSales,
                     UMEME = UMEMESales,
                     BATUSales)
      
      plot(data, type = "o" , col="blue", xlim=c(1,82),xlab="Days of Trading", ylab="Shares Traded")
      if (data == BATUSales) {
        title(main="Year Stock traded by BATU", col.main="Blue", font.main=4)
      } else if (data == BOBUSales) {
        title(main="Year Stock traded by BOBU", col.main="Blue", font.main=4)
      } else if (data == DFCUSales) {
        title(main="Year stock tradings by DFCU", col.main="Blue", font.main=4)
      } else if (data == NICSales) {
        title(main="Year stock tradings by NIC", col.main="Blue", font.main=4)
      } else if (data == NVLSales) {
        title(main="Year stock tradings by NVL", col.main="Blue", font.main=4)
      } else if (data == SBUSales) {
        title(main="Year stock tradings by SBU", col.main="Blue", font.main=4)
      } else if (data == UCLSales) {
        title(main="Year stock tradings by UCL", col.main="Blue", font.main=4)
      } else
        title(main="Year stock tradings by UMEME", col.main="Blue", font.main=4)
      
    })
    
    
    output$myline <- renderPlot({
        data <- switch(input$dist,
                       BATU = BATUData,
                       BOBU = BOBUData,
                       DFCU = DFCUData,
                       NIC = NICData,
                       NVL = NVLData,
                       SBU = SBUData,
                       UCL = UCLData,
                       UMEME = UMEMEData,
                       BOBUData)
        
        
      
      plot(data,col="blue")
      
          })
    
    output$myline1 <- renderPlot({
      data <- switch(input$dust,
                     BATU = BATUData,
                     BOBU = BOBUData,
                     DFCU = DFCUData,
                     NIC = NICData,
                     NVL = NVLData,
                     SBU = SBUData,
                     UCL = UCLData,
                     UMEME = UMEMEData,
                     BATUData)
      
      choise <- input$choice
      
                     
    if (data == BATUData && choise == "First") {
        moving_average = forecast(ma(BATUData[1:77,1], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, BATUData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,65700),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of BATU sales", col.main="Blue", font.main=4)
        lines(BATUData[1:82,1])
      } else if (data == BATUData && choise == "Second") {
        exp <- ses(BATUData[1:77,1], 5, initial="simple")
        exp_accuracy = accuracy(exp, BATUData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,65700),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of BATU sales", col.main="Blue", font.main=4)
        lines(BATUData[1:82,1])
      } else if (data == BATUData && choise == "Third") {
        train = BATUData[1:77,1]
        test = BATUData[1:82,1]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,65700),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of BATU sales", col.main="Blue", font.main=4)
        lines(BATUData[1:82,1])
      } else if (data == BOBUData && choise == "First") {
        moving_average = forecast(ma(BOBUData[1:77,1], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, BOBUData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,1202000),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of BOBU sales", col.main="Blue", font.main=4)
        lines(BOBUData[1:82,1])
      } else if (data == BOBUData && choise == "Second") {
        exp <- ses(BOBUData[1:77,1], 5, initial="simple")
        exp_accuracy = accuracy(exp, BOBUData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,1202000),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of BOBU sales", col.main="Blue", font.main=4)
        lines(BOBUData[1:82,1])
      } else if (data == BOBUData && choise == "Third") {
        train = BOBUData[1:77,1]
        test = BOBUData[1:82,1]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,1202000),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of BOBU sales", col.main="Blue", font.main=4)
        lines(BOBUData[1:82,1])
      } else if (data == DFCUData && choise == "First") {
        moving_average = forecast(ma(DFCUData[1:77,1], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, DFCUData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,757580),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of DFCU sales", col.main="Blue", font.main=4)
        lines(DFCUData[1:82,1])
      } else if (data == DFCUData && choise == "Second") {
        exp <- ses(DFCUData[1:77,1], 5, initial="simple")
        exp_accuracy = accuracy(exp, DFCUData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,757580),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of DFCU sales", col.main="Blue", font.main=4)
        lines(DFCUData[1:82,1])
      } else if (data == DFCUData && choise == "Third") {
        train = DFCUData[1:77,1]
        test = DFCUData[1:82,1]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,757580),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of DFCU sales", col.main="Blue", font.main=4)
        lines(DFCUData[1:82,1])
      } else if (data == NICData && choise == "First") {
        moving_average = forecast(ma(NICData[1:77,1], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, NICData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,213800),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of NIC sales", col.main="Blue", font.main=4)
        lines(NICData[1:82,1])
      } else if (data == NICData && choise == "Second") {
        exp <- ses(NICData[1:77,1], 5, initial="simple")
        exp_accuracy = accuracy(exp, NICData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,213800),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of NIC sales", col.main="Blue", font.main=4)
        lines(NICData[1:82,1])
      } else if (data == NICData && choise == "Third") {
        train = NICData[1:77,1]
        test = NICData[1:82,1]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,213800),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of NIC sales", col.main="Blue", font.main=4)
        lines(NICData[1:82,1])
      } else if (data == NVLData && choise == "First") {
        moving_average = forecast(ma(NVLData[1:77,1], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, NVLData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,16000),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of NVL sales", col.main="Blue", font.main=4)
        lines(NVLData[1:82,1])
      } else if (data == NVLData && choise == "Second") {
        exp <- ses(NVLData[1:77,1], 5, initial="simple")
        exp_accuracy = accuracy(exp, NVLData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,16000),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of NVL sales", col.main="Blue", font.main=4)
        lines(NVLData[1:82,1])
      } else if (data == NVLData && choise == "Third") {
        train = NVLData[1:77,1]
        test = NVLData[1:82,1]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,16000),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of NVL sales", col.main="Blue", font.main=4)
        lines(NVLData[1:82,1])
      } else if (data == SBUData && choise == "First") {
        moving_average = forecast(ma(SBUData[1:77,1], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, SBUData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,105886910),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of SBU sales", col.main="Blue", font.main=4)
        lines(SBUData[1:82,1])
      } else if (data == SBUData && choise == "Second") {
        exp <- ses(SBUData[1:77,1], 5, initial="simple")
        exp_accuracy = accuracy(exp, SBUData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,105886910),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of SBU sales", col.main="Blue", font.main=4)
        lines(SBUData[1:82,1])
      } else if (data == SBUData && choise == "Third") {
        train = SBUData[1:77,1]
        test = SBUData[1:82,1]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,105886910),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of SBU sales", col.main="Blue", font.main=4)
        lines(SBUData[1:82,1])
      } else if (data == UCLData && choise == "First") {
        moving_average = forecast(ma(UCLData[1:77,1], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, UCLData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,1710000),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of UCL sales", col.main="Blue", font.main=4)
        lines(UCLData[1:82,1])
      } else if (data == UCLData && choise == "Second") {
        exp <- ses(UCLData[1:77,1], 5, initial="simple")
        exp_accuracy = accuracy(exp, UCLData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,1710000),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of UCL sales", col.main="Blue", font.main=4)
        lines(UCLData[1:82,1])
      } else if (data == UCLData && choise == "Third") {
        train = UCLData[1:77,1]
        test = UCLData[1:82,1]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,1710000),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of UCL sales", col.main="Blue", font.main=4)
        lines(UCLData[1:82,1])
      } else if (data == UMEMEData && choise == "First") {
        moving_average = forecast(ma(UMEMEData[1:77,1], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, UMEMEData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,23001600),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of UMEME sales", col.main="Blue", font.main=4)
        lines(UMEMEData[1:82,1])
      } else if (data == UMEMEData && choise == "Second") {
        exp <- ses(UMEMEData[1:77,1], 5, initial="simple")
        exp_accuracy = accuracy(exp, UMEMEData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,23001600),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of UCL sales", col.main="Blue", font.main=4)
        lines(UMEMEData[1:82,1])
      } else if (data == UMEMEData && choise == "Third") {
        train = UMEMEData[1:77,1]
        test = UMEMEData[1:82,1]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,23001600),xlab="Days of Trading", ylab="Shares Traded")
        title(main="Graph Showing forecasts of UCL sales", col.main="Blue", font.main=4)
        lines(UMEMEData[1:82,1])
      }
    })
    output$myline2 <- renderPlot({
      dropdown <- switch(input$select,
                     BATU = BATUData,
                     BOBU = BOBUData,
                     DFCU = DFCUData,
                     NIC = NICData,
                     NVL = NVLData,
                     SBU = SBUData,
                     UCL = UCLData,
                     UMEME = UMEMEData,
                     BATUData)
      
      choise_selection <- input$choice_selection
      
      
    if (dropdown == BATUData && choise_selection == "First") {
        moving_average = forecast(ma(BATUData[1:77,2], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, BATUData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,30000),xlab="Days of Trading", ylab="Gain in prices")
        lines(BATUData[1:82,2])
        title(main="Graph Showing forecasts of BATU sales", col.main="Blue", font.main=4)
      } else if (dropdown == BATUData && choise_selection == "Second") {
        exp <- ses(BATUData[1:77,2], 5, initial="simple")
        exp_accuracy = accuracy(exp, BATUData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,30000),xlab="Days of Trading", ylab="Gain in prices")
        lines(BATUData[1:82,2])
        title(main="Graph Showing forecasts of BATU Prices", col.main="Blue", font.main=4)
      } else if (dropdown == BATUData && choise_selection == "Third") {
        train = BATUData[1:77,2]
        test = BATUData[1:82,2]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,30000),xlab="Days of Trading", ylab="Gain in prices")
        lines(BATUData[1:82,2])
        title(main="Graph Showing forecasts of BATU Prices", col.main="Blue", font.main=4)
      } else if (dropdown == BOBUData && choise_selection == "First") {
        moving_average = forecast(ma(BOBUData[1:77,2], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, BOBUData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,170),xlab="Days of Trading", ylab="Gain in prices")
        lines(BOBUData[1:82,2])
        title(main="Graph Showing forecasts of BOBU Prices", col.main="Blue", font.main=4)
      } else if (dropdown == BOBUData && choise_selection == "Second") {
        exp <- ses(BOBUData[1:77,2], 5, initial="simple")
        exp_accuracy = accuracy(exp, BOBUData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,170),xlab="Days of Trading", ylab="Gain in prices")
        lines(BOBUData[1:82,2])
        title(main="Graph Showing forecasts of BOBU Prices", col.main="Blue", font.main=4)
      } else if (dropdown == BOBUData && choise_selection == "Third") {
        train = BOBUData[1:77,2]
        test = BOBUData[1:82,2]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,170),xlab="Days of Trading", ylab="Gain in prices")
        lines(BOBUData[1:82,2])
        title(main="Graph Showing forecasts of BOBU Prices", col.main="Blue", font.main=4)
      } else if (dropdown == DFCUData && choise_selection == "First") {
        moving_average = forecast(ma(DFCUData[1:77,2], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, DFCUData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,990),xlab="Days of Trading", ylab="Gain in prices")
        lines(DFCUData[1:82,2])
        title(main="Graph Showing forecasts of DFCU Prices", col.main="Blue", font.main=4)
      } else if (dropdown == DFCUData && choise_selection == "Second") {
        exp <- ses(DFCUData[1:77,2], 5, initial="simple")
        exp_accuracy = accuracy(exp, DFCUData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,990),xlab="Days of Trading", ylab="Gain in prices")
        lines(DFCUData[1:82,2])
        title(main="Graph Showing forecasts of DFCU Prices", col.main="Blue", font.main=4)
      } else if (dropdown == DFCUData && choise_selection == "Third") {
        train = DFCUData[1:77,2]
        test = DFCUData[1:82,2]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,990),xlab="Days of Trading", ylab="Gain in prices")
        lines(DFCUData[1:82,2])
        title(main="Graph Showing forecasts of DFCU Prices", col.main="Blue", font.main=4)
      } else if (dropdown == NICData && choise_selection == "First") {
        moving_average = forecast(ma(NICData[1:77,2], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, NICData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,15),xlab="Days of Trading", ylab="Gain in prices")
        lines(NICData[1:82,2])
        title(main="Graph Showing forecasts of NIC Prices", col.main="Blue", font.main=4)
      } else if (dropdown == NICData && choise_selection == "Second") {
        exp <- ses(NICData[1:77,2], 5, initial="simple")
        exp_accuracy = accuracy(exp, NICData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,15),xlab="Days of Trading", ylab="Gain in prices")
        lines(NICData[1:82,2])
        title(main="Graph Showing forecasts of NIC Prices", col.main="Blue", font.main=4)
      } else if (dropdown == NICData && choise_selection == "Third") {
        train = NICData[1:77,2]
        test = NICData[1:82,2]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,15),xlab="Days of Trading", ylab="Gain in prices")
        lines(NICData[1:82,2])
        title(main="Graph Showing forecasts of NIC Prices", col.main="Blue", font.main=4)
      } else if (dropdown == NVLData && choise_selection == "First") {
        moving_average = forecast(ma(NVLData[1:77,2], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, NVLData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,620),xlab="Days of Trading", ylab="Gain in prices")
        lines(NVLData[1:82,2])
        title(main="Graph Showing forecasts of NVL Prices", col.main="Blue", font.main=4)
      } else if (dropdown == NVLData && choise_selection == "Second") {
        exp <- ses(NVLData[1:77,2], 5, initial="simple")
        exp_accuracy = accuracy(exp, NVLData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,620),xlab="Days of Trading", ylab="Gain in prices")
        lines(NVLData[1:82,2])
        title(main="Graph Showing forecasts of NVL Prices", col.main="Blue", font.main=4)
      } else if (dropdown == NVLData && choise_selection == "Third") {
        train = NVLData[1:77,2]
        test = NVLData[1:82,2]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,620),xlab="Days of Trading", ylab="Gain in prices")
        lines(NVLData[1:82,2]) 
        title(main="Graph Showing forecasts of NVL Prices", col.main="Blue", font.main=4)
      } else if (dropdown == SBUData && choise_selection == "First") {
        moving_average = forecast(ma(SBUData[1:77,2], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, SBUData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,35),xlab="Days of Trading", ylab="Gain in prices")
        title(main="Graph Showing forecasts of SBU Prices", col.main="Blue", font.main=4)
        lines(SBUData[1:82,2])
      } else if (dropdown == SBUData && choise_selection == "Second") {
        exp <- ses(SBUData[1:77,2], 5, initial="simple")
        exp_accuracy = accuracy(exp, SBUData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,35),xlab="Days of Trading", ylab="Gain in prices")
        lines(SBUData[1:82,2])
        title(main="Graph Showing forecasts of SBU Prices", col.main="Blue", font.main=4)
      } else if (dropdown == SBUData && choise_selection == "Third") {
        train = SBUData[1:77,2]
        test = SBUData[1:82,2]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,35),xlab="Days of Trading", ylab="Gain in prices")
        lines(SBUData[1:82,2])
        title(main="Graph Showing forecasts of SBU Prices", col.main="Blue", font.main=4)
      } else if (dropdown == UCLData && choise_selection == "First") {
        moving_average = forecast(ma(UCLData[1:77,2], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, UCLData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,20),xlab="Days of Trading", ylab="Gain in prices")
        lines(UCLData[1:82,2])
        title(main="Graph Showing forecasts of UCL Prices", col.main="Blue", font.main=4)
      } else if (dropdown == UCLData && choise_selection == "Second") {
        exp <- ses(UCLData[1:77,2], 5, initial="simple")
        exp_accuracy = accuracy(exp, UCLData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,20),xlab="Days of Trading", ylab="Gain in prices")
        lines(UCLData[1:82,2])
        title(main="Graph Showing forecasts of UCL Prices", col.main="Blue", font.main=4)
      } else if (dropdown == UCLData && choise_selection == "Third") {
        train = UCLData[1:77,2]
        test = UCLData[1:82,2]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,20),xlab="Days of Trading", ylab="Gain in prices")
        lines(UCLData[1:82,2])
        title(main="Graph Showing forecasts of UCL Prices", col.main="Blue", font.main=4)
      } else if (dropdown == UMEMEData && choise_selection == "First") {
        moving_average = forecast(ma(UMEMEData[1:77,2], order=3), h=5)
        moving_average_accuracy = accuracy(moving_average, UMEMEData[78:82])
        moving_average; moving_average_accuracy
        plot(moving_average, ylim=c(0,700),xlab="Days of Trading", ylab="Gain in prices")
        lines(UMEMEData[1:82,2])
        title(main="Graph Showing forecasts of UMEME Prices", col.main="Blue", font.main=4)
      } else if (dropdown == UMEMEData && choise_selection == "Second") {
        exp <- ses(UMEMEData[1:77,2], 5, initial="simple")
        exp_accuracy = accuracy(exp, UMEMEData[78:82])
        exp; exp_accuracy
        plot(exp, ylim=c(0,700),xlab="Days of Trading", ylab="Gain in prices")
        lines(UMEMEData[1:82,2])
        title(main="Graph Showing forecasts of UMEME Prices", col.main="Blue", font.main=4)
      } else if (dropdown == UMEMEData && choise_selection == "Third") {
        train = UMEMEData[1:77,2]
        test = UMEMEData[1:82,2]
        arma_fit <- auto.arima(train)
        arma_forecast <- forecast(arma_fit, h = 5)
        arma_fit_accuracy <- accuracy(arma_forecast, test)
        arma_fit; arma_forecast; arma_fit_accuracy
        plot(arma_forecast, ylim=c(0,700),xlab="Days of Trading", ylab="Gain in prices")
        lines(UMEMEData[1:82,2])
        title(main="Graph Showing forecasts of UMEME Prices", col.main="Blue", font.main=4)
        
      } 
        
      
    })
      }
)