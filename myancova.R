require(ggplot2)
require(grid)

myancova=function(data,y,x,A,formula=NULL){
    df=data[c(y,x,A)]
    if(!("factor" %in% class(df[[A]]))) df[[A]]=factor(df[[A]])
    df$all=rep("all",nrow(data))
    df$colour=factor(df[[A]])
    if(is.null(formula)) formula=as.formula(paste(y,"~",x,"+",A))
    fit=lm(formula,data=df)
    coef=fit$coef
    slope=rep(coef[2],length(coef)-1)
    intercept=coef[1]
    for(i in 3:length(coef)) intercept=c(intercept,coef[1]+coef[i])
    name=levels(df[[A]])
    df1=data.frame(name,slope,intercept)
    colnames(df1)[1]=A
    df1$colour=df1[[A]]
    df1
    p<-ggplot(data=df,aes_string(x=x,y=y,colour="colour",fill=A))+
            geom_point()+
            facet_grid(as.formula(paste(".~",A)),margins=TRUE)+
            guides(colour=FALSE,fill=FALSE,linetype=FALSE)+
            geom_abline(data=df1,aes_string(slope="slope",intercept="intercept",
                                            colour="colour",linetype="colour"))
    
    p
    
}


transparent=function(){
    
    
    temp=theme(rect= element_rect(fill = 'transparent',size=0),
               panel.background=element_rect(fill = 'transparent'),
               panel.border=element_rect(size=0.5),
               panel.grid.major=element_blank(),
               panel.grid.minor=element_blank())
    temp
}

#width=c(3,1)
multiggplot=function(p,vp,title){
    
    fsize=20
    grid.newpage()
    for(i in 1:length(p))  print(p[[i]],vp=vp[[i]])
    grid.text(title,x=0.5,
              y=0.96,just=c("centre"),gp=gpar(fontsize=fsize))
}

 data=mtcars
 y="mpg"
 x="wt"
 A="cyl"
formula=NULL
 # 
# 
#  

# result=myancova(mtcars,"mpg","wt","cyl")
# str(result)
# result$p[[2]]
# result
myancova(radial,"NTAV","age","sex")
