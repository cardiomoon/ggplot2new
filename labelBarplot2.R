labelBarplot=function(dataname,xvar,yvar=NULL,fillvar=NULL,stat="bin"){
   if(stat=="bin") dat=labelBarplotBin(dataname,xvar,fillvar)
   else dat=labelBarplotIdentity(dataname,xvar,yvar)
   dat
}    

labelBarplotBin=function(dataname,xvar,fillvar){
    
        temp=paste0("ddply(",dataname,", .(",xvar,"), function(.) {
           good <- prop.table(table(factor(.$",fillvar,")))
           res <- cumsum(prop.table(table(factor(.$",fillvar,"))))
           data.frame(",fillvar,"= names(res), y = c(res), good = good, pos = cumsum(good) - 0.5*good)
       })")
        dat <- eval(parse(text=temp))
        dat$pos=dat$pos.Freq
        dat$label=paste(round(dat$good.Freq*100,1),"%")
   
    dat
}
labelBarplotIdentity=function(dataname,xvar,yvar){

    
       temp=paste0("ddply(",dataname,",'",xvar,"', transform,
                  percent_weight = ",yvar,"/ sum(",yvar,") * 100)")
       dat <- eval(parse(text=temp))
       temp=paste0("ddply(dat,'",xvar,"', transform,
             pos=(cumsum(percent_weight)-0.5*percent_weight)/100)")
       dat <- eval(parse(text=temp))
       temp=paste0("ddply(dat,'",xvar,"', transform,    
             label=paste(round(percent_weight, digits=1), '%'))")
       dat <- eval(parse(text=temp))
    dat
}

