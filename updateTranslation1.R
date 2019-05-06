# key=c("Title","eng","kor")
# en=c("Web-based Meta-analysis","English","Korean")
# kor=c("웹에서 하는 R 메타분석","영어","한국어")
# 
# 
# dictionary=data.frame(key,en,kor)
# write.csv(dictionary,"dictionary.csv",row.names=FALSE)
dictionary=read.csv("dictionary.csv",fileEncoding="utf-8") 
# update the processed translation file translation.bin
# run this every time dictionary.csv is updated 
# it reads the look-up table in dictionary.csv and turns it into a 2D list

library(plyr)
translationContent <- read.csv("dictionary.csv",fileEncoding="utf-8") 
translation <- dlply(translationContent ,.(key), function(s) key = as.list(s))

save(translation, file = "translation.bin")


