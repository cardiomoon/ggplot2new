require(ggplot2)
require(reshape2)
require(plyr)
require(dplyr)
require(moonBook)
require(gcookbook)
require(car)

## position=dodge ##



labelBarplot=function(dataname,xvar,yvar,position="stack"){
   if(position=="stack") {
       temp=paste0("ddply(",dataname,",.(",xvar,"),function(.) {
                res <-table(factor(.$",yvar,"))
                data.frame(",yvar,"=names(res),label=c(res),pos=c(res))
                })")
       dat <- eval(parse(text=temp))
       
   } else if(position=="dodge"){
       temp=paste0("dat <- ddply(",dataname,", .(",xvar,"), function(.) {
           good <- table(factor(.$",yvar,"))
           res <- cumsum(table(factor(.$",yvar,")))
           data.frame(",yvar,"= names(res), label = c(res), good = good, pos = cumsum(good)-0.5*good )
       })")
       dat <- eval(parse(text=temp))
       dat$pos=dat$pos.Freq
   } else {  #position="fill"
       temp=paste0("ddply(",dataname,", .(",xvar,"), function(.) {
           good <- prop.table(table(factor(.$",yvar,")))
           res <- cumsum(prop.table(table(factor(.$",yvar,"))))
           data.frame(",yvar,"= names(res), y = c(res), good = good, pos = cumsum(good) - 0.5*good)
       })")
       dat <- eval(parse(text=temp))
       dat$pos=dat$pos.Freq
       dat$label=paste(round(dat$good.Freq*100,1),"%")
   }
   dat
}

dataname="acs"
xvar="Dx"
yvar="sex"
position="dodge"

labelBarplot(dataname,xvar,yvar,position)


dat <- ddply(acs, .(Dx), function(.) {
    res <- table(factor(.$sex))
    data.frame(sex = names(res), label = c(res),pos=c(res))
})
dat

ggplot(data=acs,aes(x=Dx,fill=sex))+
    geom_bar(position="dodge")+
    geom_text(data=dat,aes(y=pos,label=label),position=position_dodge(0.9),vjust=2)

## position=stack ##

dat <- ddply(acs, .(Dx), function(.) {
    good <- table(factor(.$sex))
    res <- cumsum(table(factor(.$sex)))
    data.frame(sex = names(res), label = c(res), good = good, pos = cumsum(good)-0.5*good )
})
dat$pos=dat$pos.Freq

ggplot(data=acs,aes(x=Dx,fill=sex))+
    geom_bar()+
    geom_text(data=dat,aes(y=pos,label=label))

## position=fill ##

dat <- ddply(acs, .(Dx), function(.) {
    good <- prop.table(table(factor(.$sex)))
    res <- cumsum(prop.table(table(factor(.$sex))))
    data.frame(sex = names(res), y = c(res), good = good, pos = cumsum(good) - 0.5*good)
})
dat$pos=dat$pos.Freq
dat$label=paste(round(dat$good.Freq*100,1),"%")
dat

ggplot(data=acs,aes(x=Dx,fill=sex))+
    geom_bar(position="fill")+    
    geom_text(data=dat,aes(y=pos,label=label))

#########  stat="identity" ##########



### dodge
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
    geom_bar(stat="identity",position="dodge")+
    geom_text(aes(label=Weight,y=Weight),position=position_dodge(0.9),vjust=1.5)


### Stacked bar graph
dat <- ddply(cabbage_exp, "Date", transform,
             pos=cumsum(Weight)-0.5*Weight)
dat
dat <- ddply(dat, "Date", transform,
             label=Weight)


ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
    geom_bar(stat="identity")+
    geom_text(data=dat,aes(label=Weight,y=pos),vjust=1.5)


### Proportional Stacked bar graph

dat <- ddply(cabbage_exp, "Date", transform,
             percent_weight = Weight / sum(Weight) * 100)

dat <- ddply(dat, "Date", transform,
             pos=(cumsum(percent_weight)-0.5*percent_weight)/100)

dat <- ddply(dat, "Date", transform,
             label=paste(round(percent_weight, digits=1), '%'))


ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
    geom_bar(stat="identity",position="fill")+
    geom_text(data=dat,aes(label=label,y=pos),vjust=1.5)


table(Salaries$rank)
dat <- ddply(Salaries, "rank", summarize,
             label=length(rank))
dat

ggplot(data=Salaries,aes(x=rank))+geom_bar()+
  geom_text(data=dat,aes(label=label,y=label))
