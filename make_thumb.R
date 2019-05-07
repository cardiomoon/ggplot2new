require(ggplot2)
# require(moonBook2)
require(png)

makeThumb <- function(file, height, width) {

        img <- png::readPNG(file)

        png(file = paste("thumb", file, sep = "_"), height = height, width = width)
        par(mar=c(0,0,0,0), xaxs="i", yaxs="i", ann=FALSE)
        plot(1:2, type='n', xaxt = "n", yaxt = "n", xlab = "", ylab = "")
        lim <- par()
        rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
        dev.off()
}

plotEx=read.csv("ggplotEx.csv",stringsAsFactors = FALSE)
str(plotEx)
plotEx$Code[33]="ggplot(data=pg,aes(x=group,y=weight,fill=hl))+ \n geom_boxplot()+ \nscale_fill_manual(values=c('grey85','#FFDDCC'),guide=FALSE)+ \n ggtitle('항목 강조를 위한 전처리')"
plotEx$Code[13]
write.csv(plotEx,"ggplotEx.csv",row.names = FALSE)
data(gcookbook::wind)
require(gcookbook)
wind<-gcookbook::wind
i=14
for(i in 1:7)
{
        cat("\nmake file:",i,"\n")
if(plotEx$Preprocessing[i]!="") eval(parse(text=plotEx$Preprocessing[i]))
p<-eval(parse(text=plotEx$Code[i]))
p<-p+ theme_grey(base_family='NanumGothic') 
filename=paste0(i,".png")
ggsave(filename,p,width=7,height=5,units="in")
makeThumb(filename,100,140)
}


plotEx
df<-plotEx
(total=nrow(df))
ncol=7
df$data_id=1:total
df$xmax=df$data_id%%ncol
df$xmax[df$xmax==0]=ncol
df$xmin=df$xmax-1
df$ymin=(total- df$data_id)%/%ncol
df$ymax=df$ymin+1
df$data_id=as.character(df$data_id)
df$xmin=df$xmin*140
df$xmax=df$xmax*140
df$ymin=df$ymin*100
df$ymax=df$ymax*100
p<-ggplot(df,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax))
for(j in 1:total) {
        
        image=png::readPNG(paste0("thumb_",j,".png"))
        xmax=(j%%ncol)*140
        if(xmax==0) xmax=ncol*140
        xmin=xmax-140
        ymin=((total-j)%/%ncol)*100
        ymax=ymin+100
        p<-p+annotation_raster(image,xmin,xmax,ymin,ymax,interpolate =FALSE)
}        
tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:20px 20px 20px 20px;"

p<-p+geom_rect_interactive(aes(tooltip=tooltip,data_id=data_id),
                           colour="black",size=1,alpha=0.1)

p<-p+coord_fixed()+theme_clean()
p
ggiraph(code=print(p),selection_type = "single",
        tooltip_extra_css = tooltip_css,
        hover_css="cursor:pointer;stroke:gold;stroke-width:3px;",width="1024px",height="768px")
