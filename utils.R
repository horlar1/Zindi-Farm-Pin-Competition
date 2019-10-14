

#### conect to SOAR package
fstoreconnect = function(subdir){
  oldLC = Sys.getenv("R_LOCAL_CACHE", unset = ".R_Cache")
  Sys.setenv(R_LOCAL_CACHe = subdir)
}
fstoreconnect("rstore")
tmp = Objects()

###
freq.encode = function(x ,xnew = x){
  if(is.factor(x) || is.character(x)){
    return(as.numeric(factor(xnew, levels = names(sort(table(x))))))
  }else{
    return(approxfun(density(x[!is.na(x)],n=length(x)/100))(xnew))
  }
}


#ndvi 
NDVI <- function(k, i) {
  bk <- k
  bi <- i
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}


group_b = function(x){
  if(x < 1000){
    return("Bin1")
  }else if(x>1000 & x<=1500){
    return("Bin2")
  }else if(x > 1500 & x<=2000){
    return("Bin3")
  }else if(x>2000 & x<=2500){
    return("Bin4")
  }else if(x> 2500 & x <= 3000){
    return("bin5")
  }else if(x > 3000 & x<=3500){
    return("bin6")
  }else if(x> 3500 & x <= 4000){
    return("bin7")
  }else {
    return("bin8")
  }
}


map.func = function(x, y =2){
  map = x %>% 
    sapply(FUN = function(x){strsplit(x, '[,.:"]')[[1]][y]}) %>% 
    sub("[[:punct:]]", '',.) %>% 
    sub("'",'',.)
  return(map)
}



#### my f2 cnt
my.f2cnt = function(th2,vn1,vn2, filter = TRUE){
  data = data.frame(f1= th2[,vn1],f2=th2[,vn2], filter = filter)
  colnames(data) = c("f1","f2","filter")
  sum1 = sqldf::sqldf("select f1,f2, count(*) as cnt from data where filter=1 group by 1,2")
  tmp = sqldf::sqldf("select b.cnt from data a left join sum1 b on a.f1=b.f1 and a.f2=b.f2")
  tmp$cnt[is.na(tmp$cnt)] = 0
  return(tmp$cnt)
  
}


#### my f2 cnt
my.f3cnt = function(th2,vn1,vn2,vn3, filter = TRUE){
  data = data.frame(f1= th2[,vn1],f2=th2[,vn2],f3=th2[,vn3], filter = filter)
  colnames(data) = c("f1","f2","f3","filter")
  sum1 = sqldf::sqldf("select f1,f2,f3, count(*) as cnt from data where filter=1 group by 1,2,3")
  tmp = sqldf::sqldf("select b.cnt from data a left join sum1 b on a.f1=b.f1 and a.f2=b.f2 and a.f3=b.f3")
  tmp$cnt[is.na(tmp$cnt)] = 0
  return(tmp$cnt)
  
}
