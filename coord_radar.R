coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
        theta <- match.arg(theta, c("x", "y"))
        r <- if (theta == "x") 
                "y"
        else "x"
        ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
                direction = sign(direction),
                is_linear = function(coord) TRUE)
}


require(ggplot2)
irislong=reshape2::melt(data=iris,id="Species")
iris2=ddply(irislong,.(Species,variable),summarize,mean=mean(value))
iris2
ggplot(data=iris2,aes(x=variable,y=mean,colour=Species,group=Species,fill=Species))+
        geom_point()+geom_polygon(alpha=0.5)+coord_radar()+theme_bw()+
        facet_wrap(~Species,nrow=2)+theme(legend.position="none")+xlab("")+ylab("") 

ggplot(data=iris2,aes(x=variable,y=mean,colour=Species,group=Species,fill=Species))+
        geom_point()+geom_polygon(alpha=0.5)+coord_radar()+theme_bw()+
        theme(legend.position="bottom")+xlab("")+ylab("") 




scaled <- as.data.frame(lapply(mtcars, ggplot2:::rescale01))
scaled$model <- rownames(mtcars)    # add model names as a variable
# melt the dataframe
mtcarsm <- reshape2::melt(scaled)
mtcarsm
# plot it as using the polygon geometry in the polar coordinates
ggplot(mtcarsm, aes(x = variable, y = value)) +
        geom_polygon(aes(group = model,colour=model), fill = NA, size = 1) +
        coord_radar() +
        theme(strip.text.x = element_text(size = rel(0.8)),
              axis.text.x = element_text(size = rel(0.8)),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              legend.position="none") +
        xlab("") + ylab("")


require(psych)
op <- par(mfrow=c(3,2))
spider(y=1,x=2:9,data=Thurstone,connect=FALSE) #a radar plot
spider(y=1,x=2:9,data=Thurstone) #same plot as a spider plot
spider(y=1:3,x=4:9,data=Thurstone,overlay=TRUE)
#make a somewhat oversized plot
spider(y=26:28,x=1:25,data=cor(bfi,use="pairwise"),fill=TRUE,scale=2) 
par(op)
