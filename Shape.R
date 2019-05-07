require(sqldf)
require(maptools)
require(BH)
#require(rgdal)

#crsTMcenter<-CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m +no_defs")
#crsWGS84lonlat<-CRS("+proj=longlat +zone=52 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +lat_0=38N +lon_0=127E")

'%!in%' <- function(x,y)!('%in%'(x,y))

census2data=function(census.data,mode=1){
    data=NULL
    if(is.numeric(mode) & (mode>0) & (mode<4)) {  
        names(census.data)<-gsub("[[:punct:]]+","_",gsub("[[:punct:]]$","",names(census.data)))
        census.data$C행정구역별_읍면동<-sub("^\\'","",census.data$C행정구역별_읍면동)
        
        if(mode==1) data<-subset(census.data,nchar(C행정구역별_읍면동)==2 & C행정구역별_읍면동>10)
        else if(mode==2) {
            data<-subset(census.data,nchar(C행정구역별_읍면동)==5)
            data<-subset(data,행정구역별_읍면동 %!in% c(" 동부", " 읍부", " 면부"))
        }    
        else if(mode==3) data<-subset(census.data,nchar(C행정구역별_읍면동)==7)
    }
    data$code=data$C행정구역별_읍면동
    data
}

shape2map=function(filename){
    #shp=readShapePoly(filename,verbose=T, proj4string=crsTMcenter)
    shp1=readShapePoly(filename)
    #shp1=spTransform(shp,crsWGS84lonlat)
    map=fortify(shp1)
    shp1@data$id=rownames(shp1@data)
    df=merge(map,shp1@data,"id")
    df$region=df$code
    df
}

# Merge shape file and data file by code into a data.frame
mergeShapeData=function(shape_file,data_file,code=code){
    myshape=readShapePoly(shape_file)
    mydf=data_file
    
    myshape@data=data.frame(myshape@data,mydf[match(myshape@data$code,mydf$code),])
    myshape@data$id=rownames(myshape@data)
    df=fortify(myshape)
    df <- merge(df, myshape@data, "id")
    df
}

# #require(plyr)
# map2region=function(map) {
#   pos=ddply(map,"region",summarize,long=median(long),lat=median(lat))
#   areaCode=read.csv("data/areacode.csv")
#   pos=merge(pos,areaCode,by.x="region",by.y="code")
#   pos
# }
map2region=function(map) {
  pos=ddply(map,"region",summarize,long=mean(long),lat=mean(lat))
  areaCode=read.csv("data/areacode.csv")
  pos=merge(pos,areaCode,by.x="region",by.y="code")
  pos
}