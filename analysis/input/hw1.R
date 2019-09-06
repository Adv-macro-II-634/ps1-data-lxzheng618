############Macro 670 PS1
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

#########Replicate table1
table<-matrix(c(earning_q,income_q,wealth_q),ncol=12,nrow = 3,byrow = TRUE)
colnames(table)<-c("0","1","5","10","20","40","60","80","90","95","99","100")
rownames(table)<-c("earnings","income","wealth")
table1<-as.table(table)
print(table1)

###########Table2

######Coefficient of variation
cve=sd(earning)/mean(earning)
cvi=sd(income)/mean(income)
cvw=sd(wealth)/mean(wealth)

######Variance of the logs
vle=var(earning,base=exp(1))
vli=var(income,base=exp(1))
vlw=var(wealth,base=exp(1))

######GINI index
ineq(earning)
ineq(income)
ineq(wealth)

######Top 1%/ Lowest 40%
e1<-sort(earning)
e2<-earning[order(-earning)]
eh1<-function(e2,n){sum(head(e2,n))}
el40<-function(e1,n){sum(head(e1,n))}
et=eh1(e2,221)/el40(e1,8834)
print(et)

i1<-sort(income)
i2<-income[order(-income)]
ih1<-function(i2,n){sum(head(i2,n))}
il40<-function(i1,n){sum(head(i1,n))}
it=ih1(i2,221)/il40(i1,8834)
print(it)

w1<-sort(wealth)
w2<-wealth[order(-wealth)]
wh1<-function(w2,n){sum(head(w2,n))}
wl40<-function(w1,n){sum(head(w1,n))}
wt=wh1(w2,221)/wl40(w1,8834)
print(wt)

######Location of mean


######Mean/median
em<-weighted.mean(earning,weight=wgt)/quantile(earning,c(0.5),weight=wgt)
im<-weighted.mean(income,weight=wgt)/quantile(income,c(0.5),weight=wgt)
wm<-weighted.mean(wealth,weight=wgt)/quantile(wealth,c(0.5),weight=wgt)

############Lorenz curve
library(ineq)
plot(Lc(earning),main = "Earning Lorenz Curve")
plot(Lc(income),main = "Income Lorenz Curve")
plot(Lc(wealth),main = "Wealth Lorenz Curve")
