login<-c("shubham","parth","shivam","parth","rahul","shubham","shubham","shivam","ravi","pintu","ziya","ankita")
hash_function<-function(value)
{
  hv<-digest(object=value,algo='murmur32',TRUE)
  y<-paste('0x',hv,sep="")
  return(as.numeric(y)%%2**16)

}
max_count<-0
for(i in 1:length(login))
{
  hash_value<-hash_function((login[i]))
  print(hash_value)
  str<-intToBits(hash_value)
  str<-str[1:17]
  if(all(str=='0'))
  {
    tzc<-0
  }
  else
  {
    str<-rev(str)
    tzc<-min(which(str==1))-1
  }
  max_count<-max(tzc,max_count)
}
cat("No of distict element is ",2**max_count)
  