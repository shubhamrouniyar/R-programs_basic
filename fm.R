library(digest)
n<-16
count_distinct<-function(stream)
{
  bits<-rep(0,n)
  for(s in stream)
  {
    x<-strtoi(substr(digest(s,algo="md5"),1,8),16L)
    if(!is.na(x)){
    bits[index(tail((intToBits(x)),16))]<-1
  }
  }
  return(2^index(bits))
}
index<-function(z)
{
  return(min(which(z!=1)))
}
stream = sample(0:2^16,10000,replace=TRUE)
ans<-count_distinct(stream)
ans

