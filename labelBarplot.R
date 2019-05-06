labelBarplot=function(dataname,xvar,yvar=NULL,fillvar=NULL,position="stack",stat="bin"){
   if(stat=="bin") dat=labelBarplotBin(dataname,xvar,fillvar,position)
   else dat=labelBarplotIdentity(dataname,xvar,yvar,position)
   dat
}    

labelBarplotBin=function(dataname,xvar,fillvar,position="stack"){
    if(position=="dodge") {
        temp=paste0("ddply(",dataname,",.(",xvar,"),function(.) {
                res <-table(factor(.$",fillvar,"))
                data.frame(",fillvar,"=names(res),label=c(res),pos=c(res))
                })")
        dat <- eval(parse(text=temp))
        
    } else if(position=="stack"){
        temp=paste0("dat <- ddply(",dataname,", .(",xvar,"), function(.) {
           good <- table(factor(.$",fillvar,"))
           res <- cumsum(table(factor(.$",fillvar,")))
           data.frame(",fillvar,"= names(res), label = c(res), good = good, pos = cumsum(good)-0.5*good )
       })")
        dat <- eval(parse(text=temp))
        dat$pos=dat$pos.Freq
        dat=dat[,-6]
    } else {  #position="fill"
        temp=paste0("ddply(",dataname,", .(",xvar,"), function(.) {
           good <- prop.table(table(factor(.$",fillvar,")))
           res <- cumsum(prop.table(table(factor(.$",fillvar,"))))
           data.frame(",fillvar,"= names(res), y = c(res), good = good, pos = cumsum(good) - 0.5*good)
       })")
        dat <- eval(parse(text=temp))
        dat$pos=dat$pos.Freq
        dat$label=paste(round(dat$good.Freq*100,1),"%")
    }
    dat
}
labelBarplotIdentity=function(dataname,xvar,yvar,position="stack"){

    if(position=="stack") {       ### Stacked bar graph
         temp<- paste0("ddply(",dataname,",'",xvar,"', transform,
               pos=cumsum(",yvar,")-0.5*",yvar,")")
         dat <- eval(parse(text=temp))
         temp=paste0("ddply(dat,'",xvar,"', transform,
             label=",yvar,")")
         dat <- eval(parse(text=temp))
    } else if(position=="fill"){  ### Proportional Stacked bar graph

       temp=paste0("ddply(",dataname,",'",xvar,"', transform,
                  percent_weight = ",yvar,"/ sum(",yvar,") * 100)")
       dat <- eval(parse(text=temp))
       temp=paste0("ddply(dat,'",xvar,"', transform,
             pos=(cumsum(percent_weight)-0.5*percent_weight)/100)")
       dat <- eval(parse(text=temp))
       temp=paste0("ddply(dat,'",xvar,"', transform,    
             label=paste(round(percent_weight, digits=1), '%'))")
       dat <- eval(parse(text=temp))
     }
     dat 
}

