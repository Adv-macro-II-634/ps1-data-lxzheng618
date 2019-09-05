######Macro 670 PS1
install.packages("readxl")
library("readxl")
database<-read_excel("D:/2019 fall macro data/2007.xlsx")
data<-dataframe(database)
wgt<-data[,1]
earning<-data[,2]
income<-data[,3]
wealth<-data[,4]
q=c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1)

#########Table1 quantiles for three variables
earning_q<-quantile(earning/1000,q,weight=wgt)
income_q<-quantile(income/1000,q,weight=wgt)
wealth_q<-quantile(wealth/1000,q,weight=wgt)

########Table2
