lm2equation=function(mydata,xvar,yvar,parse=FALSE){
  fit=eval(parse(text=paste0("lm(",yvar,"~",xvar,",data=",mydata,")")))
  intercept=round(coef(fit)[1],1)
  slope=round(coef(fit)[2],1)
  if(parse) equation=paste0("y==",slope,"*x",ifelse(intercept>=0,'+','-'),abs(intercept))
  else equation=paste0("y = ",slope,"x",ifelse(intercept>=0,' + ',' - '),abs(intercept))
  p=round(summary(fit)$coeff[2,4],3)
  if(p==0) equation=paste(equation,"\n(p < 0.001)")
  else equation=paste(equation,"\n(p =",p,")")
}    

lm2table=function(mydata,xvar,yvar,parse=FALSE){
  fit=eval(parse(text=paste0("lm(",yvar,"~",xvar,",data=",mydata,")")))
  res1 = data.frame(summary(fit)$coeff)
  res2 = cbind(round(res1[,1:3],2),sprintf("%0.4f",res1[,4]))
  colnames(res2) = c("Estimate", "Std. Error", "t value", 
                     "Pr(>|t|)")
  res2
}