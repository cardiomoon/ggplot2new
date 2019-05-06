#require(deducorrect)

if(Sys.info()[["sysname"]]=="Darwin") {par(family="AppleGothic")}

allNA=function(x){
    result=ifelse((sum(is.na(x))==length(x)),1,0)
    result
}

#'delete columns containig all NA
#'
#'@param data A data.frame
delAllNA=function(data,show.del=0){
    select=sapply(data,function(x) allNA(x)==1)
    count=length(which(select==TRUE))
    if(show.del){
       #cat("== Delete column that contains no datum(all NA values). ==\n")    
       if(count>1) cat("Total ",count," columns were deleted.\n") 
       else if(count==1) cat(count," column was deleted.\n") 
       else "No column was deleted.\n\n"
       if(count>0) cat("Deleted column(s):",colnames(data[select]),"\n\n")
    }
    
    data1=data[,which(select==FALSE)]    
    data1 
}


countNaN=function(x){
    suppressWarnings(sum(is.na(as.integer(x))))
}


isFalseString=function(x,ratio=0.05){
    if(is.numeric(x)) return (FALSE)
    else {
        x=gsub(",","",x)
        x=gsub("$","",x)
        if((countNaN(x)/length(x)<ratio)) return(TRUE)
    }
    return(FALSE)
}

extractNumber=function(x){
    
    #str(x)
    #suppressWarnings(select<-sapply(x,function(y) is.na(as.numeric(y))))
    #cat(paste("Selected row(s):",which(select),"\n",sep=""))
    #cat("Converted From:",x[select])
    x=gsub("[^0-9.]","",x)
    #cat("  To:",as.numeric(x[select]),"\n")
    as.numeric(x)
}

cleanFalseString=function(df,show=1){
    #str(df)
    select=sapply(df,isFalseString)
    count=length(which(select==TRUE))
    #cat("\n== Check for numeric columns treated as non-numeric. ==\n")
    #if(count>0) cat("Converted column(s): ",colnames(df[select]),"\n\n")
    #else cat("No column was found.\n\n")
    if(count>1) df[,select]=lapply(df[,select],extractNumber)
    else if(count==1) df[select]=lapply(df[select],extractNumber)
    
    df
}

outlierCheck=function(df,coef=2){
    cat("\n== Outlier check ==\n")
    for(i in 1:ncol(df)){
        if(!is.numeric(df[[i]])) next
        result=boxplot.stats(df[[i]],coef=coef)$out
        if(length(result)>0){
            cat(i,". column: ", colnames(df)[i],
                ",mean:", round(mean(df[[i]],na.rm=TRUE),1),
                ",sd:",round(sd(df[[i]],na.rm=TRUE),1),"\n")
            cat("-outlier :",result,"\n")
        }
    }
}

NAcheck=function(df){
    cat("\n== Check NA count for each column ==\n\n")
    na.count=apply(df,2,function(x) sum(is.na(x)))
    result=na.count[na.count>0]
    print(result)
    barplot(na.count[na.count>0])
}
    
cleanData=function(df,show=1){
    df1=delAllNA(df,show.del=show)
    #NAcheck(df1)
    df2=cleanFalseString(df1,show=show)
    #df1=outlierCheck(df1)
    df2
}

GroupVar=function(df,max.ylev=20){
    result=c()
    for(i in 1:ncol(df)){
       if(length(unique(df[[i]]))<=max.ylev) result=c(result,colnames(df)[i])      
   }    
   result
}

ContinuousVar=function(df){
    result=c()
    for(i in 1:ncol(df)){
        if(is.numeric(df[[i]])) result=c(result,colnames(df)[i])      
    }    
    result
}

BiVar=function(df){
    result=c()
    for(i in 1:ncol(df)){
        if(length(unique(df[[i]]))==2) result=c(result,colnames(df)[i])      
    }    
    result
}

residplot <- function(fit, nbreaks=10) {
    fit2 <- rstudent(fit)
    hist(fit2, breaks=nbreaks, freq=FALSE,
         xlab="Studentized Residual",
         main="Distribution of Errors")
    rug(jitter(fit2), col="brown")
    curve(dnorm(x, mean=mean(fit2), sd=sd(fit2)),
          add=TRUE, col="blue", lwd=2)
    lines(density(fit2)$x, density(fit2)$y,col="red", lwd=2, lty=2)
    legend("topright",
           legend = c( "Normal Curve", "Kernel Density Curve"),
           lty=1:2, col=c("blue","red"), cex=.7)
}

hat.plot <- function(fit) {
    p <- length(coefficients(fit))
    n <- length(fitted(fit))
    plot(hatvalues(fit), main="Index Plot of Hat Values")
    abline(h=c(2,3)*p/n, col="red", lty=2)
    identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}    