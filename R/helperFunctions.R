
# Function to count groups for currency codes:
getCCgroups <- function(x = c()){
  df <- setNames(data.frame(cCode=str_split(x,','), stringsAsFactors = F),'ccode')
  df <- inner_join(df, CurrencyCodes, by=c('ccode'='CurrencyCode'))
  return(sum(df$GroupCount, na.rm = T))
}

# Function to predict with regsubsets:
predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

# sumary function++
mysummary<-function(x){
  result<-c(length(x),mean(x),sd(x),sd(x)/length(x))
  names(result)<-c("N","Mean","SD","SE")
  return(result)
}