data<-read.csv('Advertising.csv')
#data
multiple_linear_reg<-function(x,y,data)
{
  Beta_all<-solve(t(x)%*%x)%*%t(x)%*%y
  Beta_all
  #y=bo+b1x1+b2x2+b3x3
  data$pred<-x%*%Beta_all
  sum_1<-sum((data$sales-data$pred)**2)
  sum_2=sum((data$sales-mean(data$sales))**2)
  rse<-1-sum_1/sum_2
  print(rse)
}
X<-as.matrix(cbind(data$TV,data$radio,data$newspaper))
Y<-as.matrix(data$sales)
multiple_linear_reg(X,Y,data)
  
