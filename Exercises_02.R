#1 Switch Function
#2
L<-function(v){
  n=length(v)
  l<-v[1]
  for(i in 2:n){
    if(v[i]>l){
      l=v[i]
    }
  }
  return(l)
}

#3
ScndL<-function(v){
  n=length(v)
  if(n<=1){retuurn(NA)}
  M<-L(v)
  w<-v[-(which(v==M))]
  m=L(w)
  return(m)
}
#4
f<-function(n){
  j=0
  v<-c(0)
  for(i in 1:((n^2)-1)){
    if(i%%n==0){j=j+1}
    if((i%%n)>=j){
      v<-c(v, ((i-j)%%n))
    }else{
      v<-c(v, n-((i-j)%%n))
    }
  }
  m<-matrix(v, nrow=n, ncol=n)
  return(m)
}
