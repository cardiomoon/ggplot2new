require(plyr)
require(ggplot2)
require(moonBook)
require(gcookbook)


#### bin의 경우: y가 없다. #####
p<-ggplot(data=acs,aes(x=Dx,fill=sex))+geom_bar(position="dodge")
p
p+geom_text(stat="bin",aes(label=..count..),position=position_dodge(0.9))

p<-ggplot(data=acs,aes(x=Dx,fill=sex))+geom_bar()
p
p+geom_text(stat="bin",aes(label=..count..))




dat=ddply(acs,"Dx",summarize,label=length(Dx),pos=label)
dat
p+geom_text(data=dat,aes(label=label,y=pos),colour="white",vjust=2)


p1=ggplot(data=acs,aes(x=age,fill=..count..))+geom_bar()
p1
p1+geom_text(stat="bin",aes(label=..count..),vjust=-1)

#### identity의 경우: y가 있다./####
upc=subset(uspopchange,rank(Change)>40)
upc

p2=ggplot(upc,aes(x=Abb,y=Change,fill=Region))+geom_bar(stat="identity")
p2+geom_text(aes(label=Change),vjust=-1)
