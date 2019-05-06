predictvals<-function(model,xvar=NULL,yvar=NULL,xrange=NULL,samples=100,
                      interval="none",...){
    if(is.null(xvar)) xvar=as.character(model$terms[[3]])
    if(is.null(yvar)) yvar=as.character(model$terms[[2]])
    if(is.null(xrange)){
        if(any(class(model) %in% c("lm","glm")))
            xrange=range(model$model[[xvar]])
        else if (any(class(model) %in% "loess"))
            xrange <-range(model$x)
    }
    newdata <-data.frame(x=seq(xrange[1],xrange[2],length.out=samples))
    names(newdata)<-xvar
    if(any(class(model) %in% "glm")){ 
        newdata2<-predict(model,newdata=newdata,type="response",interval=interval,...)
    } else newdata2<-predict(model,newdata=newdata,interval=interval,...)
    if(interval=="none") {
        newdata[[yvar]]=newdata2
    } else {
        for(i in 1:ncol(newdata2)) newdata=cbind(newdata,newdata2[,i])
        names(newdata)<-c(xvar,"fit","lwr","upr")
    }        
    newdata
}

ggpredict=function(model,xvar=NULL,yvar=NULL,xrange=NULL,samples=100,
                   interval="none",...){
    
    if(is.null(xvar)) xvar=as.character(model$terms[[3]])
    if(is.null(yvar)) yvar=as.character(model$terms[[2]])
    
    observed=model$model
    data=predictvals(model,xvar=xvar,yvar=yvar,xrange=xrange,samples=samples,
                     interval=interval,...)
    if(interval=="none"){
        ggplot(observed,aes_string(x=xvar,y=yvar))+
            geom_point()+
            geom_line(data=data)
        
        #ggplot(data,aes(x=data[[1]],y=data[[2]]))+
        #    geom_line()+
        #    geom_point(data=observed)
        
    } else{
        ggplot(data,aes_string(x=xvar))+
            geom_line(aes(y=fit))+
            geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.2)+
            geom_point(data=observed,aes_string(y=yvar))
        
    }
}    