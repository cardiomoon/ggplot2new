require(shiny)
require(Cairo)
require(MatchIt)
require(ztable)
require(readxl)
require(ggplot2)
require(rmarkdown)
require(markdown)
require(extrafont)
require(moonBook)
require(RColorBrewer)
require(plyr)
require(dplyr)
require(reshape2)
require(grid)
require(lattice)
require(car)
require(maps)
require(maptools)
require(ggthemes)
require(gridExtra)
require(survival)
require(ggmap)
require(officer)
require(rrtable)
require(rvg)
require(ggiraph)
require(ggiraphExtra)
require(plotly)
require(gcookbook)
require(mycor)
require(htmlwidgets)
require(scales)
#require(kormaps2014)
#library(rhandsontable)
#require(cowplot)

source("cleaning.R")
#source("summarySE.R")
#source("labelBarplot2.R")
source("lm2equation.R")
source("theme_clean.R")
source("Shape.R")
source("ggpredict.R")


load("translation.bin") # contains the dictionary, parsed as a double list

options(ztable.type="html")
options(shiny.maxRequestSize=30*1024^2)

dic=read.csv("dictionary.csv",fileEncoding="utf-8",stringsAsFactors = FALSE)

countryCode=read.csv("countryCode.csv",stringsAsFactors = FALSE)
korpop1=readRDS("data/korpopdata1.RDS")
korpop2=readRDS("data/korpopdata2.RDS")
korpop3=readRDS("data/korpopdata3.RDS")
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
states_map <- map_data("state")

levelchoices=c("시도"=1,"시군구"=2,"읍면동"=3)
areachoices=c("전국"=0,"서울특별시"=11,"부산광역시"=21,"대구광역시"=22,"인천광역시"=23,
              "광주광역시"=24,"대전광역시"=25,"울산광역시"=26,"경기도"=31,"강원도"=32,"충청북도"=33,
              "충청남도"=34,"전라북도"=35,"전라남도"=36,"경상북도"=37,"경상남도"=38,"제주특별자치도"=39)

selections=c("ifill1","ix16","icolor16","ix15","iy15","ifill15","ix13","iy13","icolor13","ifacet13","ifacet7",
             "ixx","iyy","icolorcolor","ifillfill","igroupgroup","ifacetfacet","ifacet","ix12","iy12","icolor12","facetvar","ifill11",
             "map_id","tooltip","iy10","ix10","igroup10","ix9","ifill9","iy9","ix8","ifill8",
             "iy8","ifacet8","ix7","ifill7","iy7","icolour7","ix6","ifill6","iy6","ix5",
             "ifill5","iy5","ix4","iy4","igroup4","donuts2","count2","iy3","ix3","icolor3",
             "count1","iy2","ix2","igroup2","iy1","ix1","iA1","ilabel1",
             "ix","iy","iA","ilabel2","pies","donuts","icolour","igroup","ifill","ialpha","ifacet")
varnames=c("fill","x","color","x","y","fill","x","y","color","facet","facet",
           "x","y","color","fill","group","facet","facet","x","y","colorvar","facetvar","fill",
           "map_id","tooltip","y","x","group","x","fill","y","x","fill",
           "y","facet","x","fill","y","colour","x","fill","y","x",
           "fill","y","x","y","group","donuts","count","y","x","color",
           "count","y","x","group","y","x","A","label",
           "x","y","A","label","pies","donuts","colour","group","fill","alpha")

multiselections=c("ix12","ix4","ifill11","ixx","ifillfill")
multivarnames=c("x","x","fill","x","fill")

palette2colors=function(name){
        brewer.pal(brewer.pal.info[rownames(brewer.pal.info)==name,"maxcolors"],name)
}

#loadfonts(device="postscript")
shinyServer(function(input, output,session) {
  
        help_console <- function(topic, format=c("text", "html", "latex", "Rd"),
                                 lines=NULL, before=NULL, after=NULL) {  
                format=match.arg(format)
                if (!is.character(topic)) topic <- deparse(substitute(topic))
                helpfile<-NULL
                try(helpfile <- utils:::.getHelpFile(help(topic)))
                if(is.null(helpfile)){
                        cat("No help file about ",input$mydata," is found")       
                } else {
                        hs <- capture.output(switch(format, 
                                                    text=tools:::Rd2txt(helpfile),
                                                    html=tools:::Rd2HTML(helpfile),
                                                    latex=tools:::Rd2latex(helpfile),
                                                    Rd=tools:::prepare_Rd(helpfile)
                        )
                        )
                        if(!is.null(lines)) hs <- hs[lines]
                        hs <- c(before, hs, after)
                        cat(hs, sep="\n")
                        invisible(hs)
                        
                        
                }
        }      
        
  uploaded<-NULL
  
  tr <- function(text){ # translates text into current language
          sapply(text,function(s) translation[[s]][[input$language]], USE.NAMES=FALSE)
  }
  
  tr2out=function(string,size=3){
          output[[string]]=renderPrint(cath(tr(string),size))
  }
  
  # md2html=function(string){
  #         HTML(markdownToHTML(fragment.only=TRUE,text=string))
  # }    
  md2html=function(string){
          result=length(unlist(gregexpr("`",string)))/2
          res=string
          for(i in 1:result){
                  res=sub("`","<code>",res)
                  res=sub("`","</code>",res)
          }
          res=paste0("<p>",res,"</p>")
          res
  }
  
    
  putmsg=function(msg="test message"){
       session$sendCustomMessage(type = 'testmessage',message = list( msg)) 
  }
  
  file2ext=function(filename){
          namelist=unlist(strsplit(filename,".",fixed=TRUE))
          result=namelist[length(namelist)]
          if(tolower(result) %in% c('csv','xlsx','dbf','sav','dta','sas7bdat')) return(tolower(result))
          else return(NULL)
  }
  
  file2newname=function(file){
          mypath=unlist(strsplit(file$datapath,"/"))
          length(mypath)
          temp=mypath[1]
          for(i in 2:(length(mypath)-1)) temp=paste(temp,mypath[i],sep="/")
          result=paste(temp,"/","test.",file2ext(file$name),sep="")
          result
  }
  
  
  
  my_readfile=function(file){
          ext=file2ext(file$name)
          result=NULL
          if(is.null(ext)) {
                  session$sendCustomMessage(type = 'testmessage',
                                            message = list( 'Only file with xlsx or csv format is supported.'))
          } else if(ext=="csv") {
                  try(result<-read.csv(file$datapath,header=TRUE),silent=TRUE)
                  if(is.null(result)) {
                          try(result<-read.csv(file$datapath,header=TRUE,fileEncoding="euc-kr"),silent=TRUE)
                          if(is.null(result)) session$sendCustomMessage(type = 'testmessage',
                                                                        message = list( 'File read error : please check file encoding')) 
                  }
          } else if(ext=="dbf"){
                  try(result1<-foreign::read.dbf(file$datapath),silent=TRUE)
                  if(is.null(result1)) {
                          session$sendCustomMessage(type = 'testmessage',
                                                    message = list( 'File read error : please check file encoding')) 
                          result<-NULL
                  } else result=mydata2csv(result1)
                  
          } else if(ext=="sav"){
                  result1<-NULL
                  result1<-haven::read_sav(file$datapath)
                  if(is.null(result1)) {
                          if(is.null(result1)) session$sendCustomMessage(type = 'testmessage',
                                                                         message = list( 'File read error1 : please check file encoding')) 
                          result<-NULL
                  } else result=mydata2csv(result1)
                  
          } else if(ext=="dta"){
                  try(result1<-haven::read_stata(file$datapath),silent=TRUE)
                  if(is.null(result1)) {
                          session$sendCustomMessage(type = 'testmessage',
                                                    message = list( 'File read error : please check file encoding')) 
                          result<-NULL
                  }else result<-mydata2csv(result1)
                  
          } else if(ext=="sas7bdat"){
                  try(result1<-haven::read_sas(file$datapath),silent=TRUE)
                  if(is.null(result1)) {
                          session$sendCustomMessage(type = 'testmessage',
                                                    message = list( 'File read error : please check file encoding')) 
                          result<-NULL
                  } else result<-mydata2csv(result1)
                  
          } else {
                  newname=file2newname(file)
                  file.copy(file$datapath,newname)
                  result=read_excel(newname)
                  if(is.null(result)) session$sendCustomMessage(type = 'testmessage',
                                                                message = list( 'File read error : please check file encoding')) 
          } 
          result
  }
  
  mypaste=function(tempA,...){
     result=""
     if(tempA=="")  result=paste0(...)
     else result=paste0(tempA,',',...)
     result
  }
  
  myplotPNG=function(listf){
    filename=c()
    count=length(listf)
    for(i in 1:count){
      path <- paste("plot_", i, ".png", sep="")
      filename <- c(filename, path)
      plotPNG(listf[[i]],path,width=6,height=6,units="in",res=300)
    }
    filename
  }
  
 
test=function(e){
  if(input$language=="kor") putmsg("잘못된 명령입니다. 수정해주세요.") 
  else putmsg("Invalid R code. Please fix.") 
  
  updateCheckboxInput(session,'doPreprocessing',value=FALSE)

}

test1=function(e){
  if(input$language=="kor") putmsg("찾을 수 없는 위치입니다.다시 입력해주세요.") 
  else putmsg("Can not find this location. Please re-enter.") 
        
  updateCheckboxInput(session,'qmap',value=FALSE)
  updateCheckboxInput(session,'location',value="")
  
}


choice2vp=function(choice){
  
    mychoice<-as.numeric(choice)
    
    remain<-mychoice%%10
    
 
    x<-y<-0.5
    
    width<-height<-ifelse(mychoice>9,0.4,0.5)  
    
    if(remain %in% c(0,5,6)) x=0.5
    else if(remain %in% c(1,3,7)) x=ifelse(mychoice>9,0.3,0.25)
    else x=0.75
    if(remain %in% c(7,0,8)) y=0.5
    else if(remain %in% c(1,5,2)) y=ifelse(mychoice>9,0.3,0.25)
    else y=0.75
    if(remain %in% c(7,0,8)) height=ifelse(mychoice>9,0.9,1)
    if(remain %in% c(6,0,5)) width=ifelse(mychoice>9,0.9,1)
    if(mychoice>9) height<-width<-0.4
    result=paste("x=",x,",y=",y,",width=",width,",height=",height,sep="")
    
  result
  
}

output$helpData=renderPrint({
         if(input$mydata=="uploaded") {
                h4("Uploaded File")
                p("No information about the data.")
        }
        else help_console(input$mydata,"html") 
})

output$helpFunction=renderPrint({
        help_console(input$iplotmain,"html") 
})

output$helpData2=renderPrint({
        if(input$mydata=="uploaded") {
                h4("Uploaded File")
                p("No information about the data.")
        }
        else help_console(input$mydata,"html") 
})

observe({
        
        if(input$language=="kor") {
                
                updateSelectInput(session,"PreProcessingExample","데이타 전처리 선택",
                                  choices=c("None"=0,"열 이름 바꾸기"=4,
                                            "범주형 변수로 바꾸기"=7,
                                            "열 추가하기/ 삭제하기"=5,
                                            "자료 정렬/필터"=6,
                                            "연속형변수를 k개의 구간으로 나누기"=8,
                                            "평균과 신뢰구간으로 데이타 요약하기"=1,
                                            "넓은 데이타 프레임을 길게 변환하기"=2,
                                            "긴 데이타 프레임을 넓게 변환하기"=3))
                updateSelectInput(session,"example","예제갤러리",
                            c("None"=0,"Diamonds Are Forever"=21,
                              "성악가의 키(1)"=14,"성악가의 키(2)"=15,"성악가의 키(3)"=16,
                              "대학 교수의 연봉(1)"=11,"대학 교수의 연봉(2)"=12,"대학 교수의 연봉(3)"=13,
                              "대학 교수의 연봉(4)"=17,
                              "전국 시도 인구분포도"=18,
                              "동별 서울 인구분포도"=19,
                              "US Murder Rate by State"=33,
                              "US Crime Rates"=34,
                              "area plot with palette"=20,
                              "polar plot"=22,
                              "horizontal bar plot"=28,
                              "heat map"=23,
                              "boxplot"=24,
                              "taco-HSD"=27,
                              "Logistic Regression*"=40,
                              "Cleveland Dot Plot"=29,
                              "Cleveland Dot Plot2"=30,
                              "Cleveland Dot Plot3"=31,
                              "화우 화산의 등고선"=32,
                              "barplot with errorbar"=25,
                              "linechart with errorbar"=26,
                              "overlapped densities"=10,"Bubble plot"=1,"histogram with density*"=2,
                              "violin with boxplot"=3,"dotplot with boxplot"=4,
                              "stat_density2d (1)"=5,"stat_density2d (2)"=6,
                              "stat_density2d (3)"=7,"항목강조를 위한 전처리"=8,
                              "전처리로 분할면에 각각 주석 넣기"=9,
                              "산점도에 테이블 넣기"=46,
                              "산점도에 회귀결과 테이블 넣기"=47,
                              "인구분포도-행정구역지도"=35,
                              "시도별 결핵 신환 발생 추이"=36,
                              "시도별 결핵 신환 발생 분포도"=37,
                              "시도별 결핵 발병률 추이"=38,
                              "시도별 결핵 발병률 분포도"=39,
                              "서울시 교통 돌발상황 위치"=41,
                              "서울시 교통 돌발상황 위치별 버블챠트"=42,
                              "서울시 교통 돌발상황 레벨플롯"=43,
                              "서울시 강동구 공영주차장 위치"=44,
                              "제주도 여행코스(1일차)"=45,
                              "geom_label 데모*"=48,
                              "면분할 예제*"=49,
                              "labeller예제*"=50,
                              "산점도에 라벨붙이기*"=51,
                              "산점도에 라벨붙이기(중복체크)*"=52,
                              "geom_count예제*"=53))
                
                
                
        } else {
                updateSelectInput(session,"PreProcessingExample","Select Data Preprocessing",
                                  choices=c("None"=0,"Change Colummn name"=4,
                                            "Encode as a factor"=7,
                                            "Add/compute/delete column"=5,
                                            "Select/filter data"=6,
                                            "Grouping a continuous var into k-groups"=8,
                                            "Summmarize data with mean and se"=1,
                                            "Transform wide form data to long form"=2,
                                            "Transform long form data to wide form"=3))
                updateSelectInput(session,"example","example gallery",
                                  c("None"=0,"Diamonds Are Forever"=21,
                                    "singer's height(1)"=14,"singer's height(2)"=15,"singer's height(3)"=16,
                                    "Salaries of Professors(1)"=11,"Salaries of Professors(2)"=12,"Salaries of Professors(3)"=13,
                                    "Salaries of Professors(4)"=17,
                                    "Korean population map"=18,
                                    "Seoul population map"=19,
                                    "US Murder Rate by State"=33,
                                    "US Crime Rates"=34,
                                    "area plot wiith palette"=20,
                                    "polar plot"=22,
                                    "horizontal bar plot"=28,
                                    "heat map"=23,
                                    "boxplot"=24,
                                    "taco-HSD"=27,
                                    "Logistic Regression*"=40,
                                    "Cleveland Dot Plot"=29,
                                    "Cleveland Dot Plot2"=30,
                                    "Cleveland Dot Plot3"=31,
                                    "Maunga Whau Volcano"=32,
                                    "barplot with errorbar"=25,
                                    "linechart with errorbar"=26,
                                    "overlapped densities"=10,"Bubble plot"=1,"histogram with density*"=2,
                                    "violin with boxplot"=3,"dotplot with boxplot"=4,
                                    "stat_density2d (1)"=5,"stat_density2d (2)"=6,
                                    "stat_density2d (3)"=7,"Highlighing an item"=8,
                                    "adding annotations to individual facets"=9,
                                    "adding table to scatterplot"=46,
                                    "adding regression result table to scatterplot"=47,
                                    # "인구분포도-행정구역지도"=35,
                                    # "시도별 결핵 신환 발생 추이"=36,
                                    # "시도별 결핵 신환 발생 분포도"=37,
                                    # "시도별 결핵 발병률 추이"=38,
                                    # "시도별 결핵 발병률 분포도"=39,
                                    # "서울시 교통 돌발상황 위치"=41,
                                    # "서울시 교통 돌발상황 위치별 버블챠트"=42,
                                    # "서울시 교통 돌발상황 레벨플롯"=43,
                                    # "서울시 강동구 공영주차장 위치"=44,
                                    # "제주도 여행코스(1일차)"=45,
                                    "geom_label demo*"=48,
                                    "labeller_example(1)*"=49,
                                    "labeller example(2)*"=50,
                                    "geom_label()*"=51,
                                    "geom_label(check_overlap=TRUE)*"=52,
                                    "geom_count()*"=53))
                
                
                
        }              
        
        headings=dic$key[dic$class=="output"]
        for(i in 1:length(headings)) tr2out(headings[i])
        
        checks=dic$key[dic$class=="Checkbox"]
        for(i in 1:length(checks)) updateCheckboxInput(session,checks[i],label=tr(checks[i]))
        
        selects=dic$key[dic$class=="Select"]
        for(i in 1:length(selects)) updateSelectInput(session,selects[i],label=tr(selects[i]))
        
        texts=dic$key[dic$class=="Text"]
        for(i in 1:length(texts)) updateSelectInput(session,texts[i],label=tr(texts[i]))
        
        radios=dic$key[dic$class=="Radio"]
        for(i in 1:length(radios)) updateRadioButtons(session,radios[i],label=tr(radios[i]))
        
        numerics=dic$key[dic$class=="Numeric"]
        for(i in 1:length(numerics)) updateNumericInput(session,numerics[i],label=tr(numerics[i]))
        
        htmls=dic$key[dic$class=="html"]
        for(i in 1:length(htmls)) shinyjs::html(htmls[i],tr(htmls[i]))

        htmls2=dic$key[dic$class=="html2"]
        for(i in 1:length(htmls2)) {
                shinyjs::html(htmls2[i],md2html(tr(htmls2[i])))
        }
        
        updateRadioButtons(session,'language',label=tr('language'))
        #updateSelectInput(session,'metadigits',label=tr('metadigits'))
})

observe({
  if(input$doPreprocessing){
    tryCatch(eval(parse(text=input$preprocessing)),error=function(e){test(e)})
  }
  
  
})

output$table<-renderTable({
  if(input$showtable|(varchoice()==0)){

     head(df(),10)
  }
})

output$table1<-renderTable({
        
        head(df())
        
})


observe({
    if(!is.null(input$file)) { 
        updateRadioButtons(session,"radio", choices = list("Salaries","mtcars","acs","radial","iris","heightweight",
                                                       "uploaded file"="uploaded"),
                       selected = input$radio)
        uploaded<<-cleanData(my_readfile(input$file))  
    }    
    updateTextInput(session,"mydata",value=input$radio)
    updateSelectInput(session,"example",selected=0)
    resetPlot()
})


observe({
        mydf=df()
        
        ggchoice=c("None",colnames(mydf))
        for(i in 1:length(selections))
                updateSelectInput(session,selections[i],choices=c(ggchoice,1))
        for(i in 1:length(multiselections))
                updateSelectInput(session,multiselections[i],choices=colnames(mydf))
        
        updateSelectInput(session,"country",choices=countryCode$Name)
        
     
})


       

###################
#rHandsontableOutput("hot",height=400),
###############
# 
# values = reactiveValues()
# current<-""
# 
# origin=reactive({
#     if(input$doPreprocessing) try(eval(parse(text=input$preprocessing)))
#     validate(
#         need(class(try(eval(parse(text=input$mydata))))=="data.frame","이용하실 R 데이타 이름을 입력하세요")
#     )
#     #resetPlot()
#     df<-eval(parse(text=input$mydata)) 
#     df
#     
# })
# 
# data=reactive({
#     temp<-input$mydata
#     if(is.null(input$hot)) DF=origin()
#     else {
#         if((current=="")|(current==input$mydata)){
#             current<<-temp
#             DF=hot_to_r(input$hot)
#             
#         } else {
#             DF=origin()
#             current<<-temp
#         }       
#         
#     }       
#     values[["DF"]]=DF
#     DF
# })
# 
# 
# 
# output$hot <- renderRHandsontable({
#     DF = data()
#     if (!is.null(DF))
#         #    rhandsontable(DF, useTypes = TRUE, stretchH = "all")
#         rhandsontable(DF, useTypes = TRUE,search=TRUE) %>%
#               hot_cols(columnSorting = TRUE) %>%
#                  hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
#       hot_context_menu(
#         customOpts = list(
#           search = list(name = "Search",
#                         callback = htmlwidgets::JS(
#                           "function (key, options) {
#                           var srch = prompt('Search criteria');
#                           
#                           this.search.query(srch);
#                           this.render();
# }"))))
#               
#   
# })
# 
# df=reactive({
#     df=values$DF
#     df
# })

df=reactive({
  if(input$doPreprocessing) try(eval(parse(text=input$preprocessing)))
  validate(
    need(class(try(eval(parse(text=input$mydata))))=="data.frame","Enter the data name")
  )
  #resetPlot()
  df<-eval(parse(text=input$mydata))

})


vector2str=function(vars){
    if(length(vars)<1) return(NULL)
    temp=paste0("c('",vars[1],"'")
    if(length(vars)>1) {
       for (i in 2:length(vars)) temp=paste0(temp,",'",vars[i],"'")
     }
     temp=paste0(temp,")")
     temp
}

vector2form=function(vars){
  if(length(vars)<1) return(NULL)
  temp=vars[1]
  if(length(vars)>1) {
    for (i in 2:length(vars)) temp=paste(temp,"+",vars[i])
  }
  temp
}

# For download preprocessed data
output$downloadMydf = downloadHandler(
  filename="Mydata.csv",
  content=function(file){
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    write.csv(df(),file=file,row.names=FALSE)
  },
  contentType="text/csv"
)

observeEvent(input$clearPreprocessing,{
        updateTextInput(session,"preprocessing",value="")
        updateCheckboxInput(session,"doPreprocessing",value=FALSE)
})

# observeEvent(input$doSummary,{
#   if((input$measurevar1!="None")&(length(input$groupvar1)>0)) {
#     groupvar=vector2str(input$groupvar1)
#     measurevars=vector2str(input$measurevar1)
#   updateTextInput(session,'preprocessing',value=paste(
#   "dat<-summarySE(",input$mydata,",measurevar=",measurevars,",groupvars=",groupvar,")",sep=""))
#   updateCheckboxInput(session,'doPreprocessing',value=TRUE)
#   updateTextInput(session,'mydata',value="dat")
#   updateSelectInput(session,'PreProcessingExample',selected=0)
#   
#   } else putmsg("변수를 선택하시고 버튼을 누르세요")
#   
# })
# 
# observeEvent(input$wide2long,{
#   if(input$id.var!="None") {
#     idvar=vector2str(input$id.var)
#     temp=paste0(input$newdata,"<- melt(",input$mydata,",id.vars=",idvar)
#     
#     if(length(input$measure.vars)>=1) {
#        groupvar=vector2str(input$measure.vars)
#        temp=mypaste(temp,"measure.vars=",groupvar)
#     }
#     if(input$variable.name!="variable") temp=mypaste(temp,"variable.name='",input$variable.name,"'")
#     if(input$value.name!="value") temp=mypaste(temp,"value.name='",input$value.name,"'")
#     temp=paste0(temp,")")
#     updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
#     updateCheckboxInput(session,'doPreprocessing',value=TRUE)
#     updateTextInput(session,'mydata',value=input$newdata)
#     updateSelectInput(session,'PreProcessingExample',selected=0)
#     updateSelectInput(session,'id.var',selected=NA)
#     updateSelectInput(session,'measure.vars',selected=NA)
#     updateTextInput(session,'variable.name',value='variable')
#     updateTextInput(session,'value',value='value')
#     updateTextInput(session,'newdata',value='dat')
#   } else putmsg("아이디 변수(id.var)를 선택하시고 버튼을 누르세요")
#   
# })

olddata=""

observeEvent(input$doSummary,{
        if((input$measurevar1!="None")&(length(input$groupvar1)>0)) {
                groupvar=vector2str(input$groupvar1)
                measurevars=vector2str(input$measurevar1)
                temp=paste("summarised<-summarySE(",input$mydata,",measurevar=",measurevars,",groupvars=",groupvar,")",sep="")
                updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
                
                updateCheckboxInput(session,'doPreprocessing',value=TRUE)
                olddata<<-c(olddata,input$mydata)
                updateTextInput(session,'mydata',value="summarised")
                updateSelectInput(session,"PreProcessingExample",selected=0)
                
                
        } else putmsg("Please select variable and press button.")
        
})

observeEvent(input$wide2long,{
        # if(input$id.var!="None") {
        
        temp=paste0("melted <- melt(",input$mydata)
        if(!is.null(input$id.var)){
                idvar=vector2str(input$id.var)
                temp=paste0(temp,",id.vars=",idvar)
        }
        if(length(input$measure.vars)>=1) {
                groupvar=vector2str(input$measure.vars)
                temp=mypaste(temp,"measure.vars=",groupvar)
        }
        if(input$variable.name!="variable") temp=mypaste(temp,",variable.name='",input$variable.name,"'")
        if(input$value.name!="value") temp=mypaste(temp,",value.name='",input$value.name,"'")
        temp=paste0(temp,")")
        updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
        updateCheckboxInput(session,'doPreprocessing',value=TRUE)
        olddata<<-c(olddata,input$mydata)    
        updateTextInput(session,'mydata',value="melted")
        
        updateSelectInput(session,'id.var',selected=NA)
        updateSelectInput(session,'measure.vars',selected=NA)
       
        updateTextInput(session,'variable.name',value='variable')
        updateTextInput(session,'value',value='value')
        updateSelectInput(session,"PreProcessingExample",selected=0)
        
        
        # } else putmsg("아이디 변수(id.var)를 선택하시고 버튼을 누르세요")
        
})

observeEvent(input$goback,{
        if(length(olddata)>1){
                mycommand=unlist(strsplit(input$preprocessing,"\n",fixed=TRUE))
                rccount=length(mycommand)
                temp=""
                if(rccount==1) temp=""
                else for(i in 1:(rccount-1)){
                        # cat("\n> ",mycommand[i],"\n")
                        #print(eval(parse(text=mycommand[i])))
                        temp=mypaste(temp,mycommand[i],sep="\n")
                }
                updateTextInput(session,"preprocessing",value=temp)
                updateTextInput(session,"mydata",value=olddata[length(olddata)])
                olddata<<-olddata[-length(olddata)]
                
        } else putmsg("No preprocessing left.")
        
})

observeEvent(input$gohome,{
        updateTextInput(session,"preprocessing",value="")
        if(length(olddata)>2) updateTextInput(session,"mydata",value=olddata[2])
        olddata<<-""
})

addpaste=function(temp,...,sep="\n"){
   if(temp=="") result=paste0(...)
   else result=paste0(temp,sep,...)
}

# 
# observeEvent(input$long2wide,{
#   if((input$value.var2!="None") & (length(input$id.var2)*length(input$measure.vars2)!=0)){
#     idvar=vector2form(input$id.var2)
#     measurevar=vector2form(input$measure.vars2)
#     temp=paste0("dat=dcast(",input$mydata)
#     temp=mypaste(temp,idvar,"~",measurevar)
#     temp=mypaste(temp,"value.var='",input$value.var2,"')")
#     updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
#     updateCheckboxInput(session,'doPreprocessing',value=TRUE)
#     updateTextInput(session,'mydata',value="dat")
#     updateSelectInput(session,'PreProcessingExample',selected=0)
#     
#   } else putmsg("값을 채울 변수를 선택하시고 버튼을 누르세요")
#   
# })
# 
# observeEvent(input$changecolname,{
#   if((input$oldcolname!="None")&(input$newcolname!="")) {
#     mydf=input$mydata
#     temp=paste0("names(",mydf,")[names(",mydf,")=='",input$oldcolname,"']<-'",
#                 input$newcolname,"'")
#     temp=addpaste(input$preprocessing,temp)
#     updateTextInput(session,'preprocessing',value=temp)
#     updateCheckboxInput(session,'doPreprocessing',value=TRUE)
#     updateSelectInput(session,'PreProcessingExample',selected=0)
#     
#   } else putmsg("이름을 바꿀 열과 새 이름을 입력하시고 버튼을 누르세요")
#   
# })
# 
# observeEvent(input$delcol,{
#   vars=input$delcolname
#   if(length(vars)>0) {
#     temp=input$preprocessing
#     for(i in 1:length(vars)){
#        temp=addpaste(temp,input$mydata,"$",vars[i],"<-NULL")
#     }
#     updateTextInput(session,'preprocessing',value=temp)
#     updateCheckboxInput(session,'doPreprocessing',value=TRUE)
#     updateSelectInput(session,'PreProcessingExample',selected=0)
#     
#   } else putmsg("삭제할 열(들)을 선택하시고 버튼을 누르세요")
#   
# })
# 
# observeEvent(input$addcol,{
#   
#   if(input$colnameadd != "") {
# 
#     temp=paste0(input$mydata,"$",input$colnameadd,"<-1:nrow(",input$mydata,")")
#     updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
#     updateCheckboxInput(session,'doPreprocessing',value=TRUE)
#     updateSelectInput(session,'PreProcessingExample',selected=0)
#     
#   } else putmsg("행번호열로 사용할 열 이름을 입력한 후 버튼을 누르세요")
#   
# })
# 
# observeEvent(input$addcol2,{
#   
#   if(input$addcolorder!="") {
#     
#     temp=paste0("mutated=mutate(",input$mydata,",",input$addcolorder,")")
#     updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
#     updateCheckboxInput(session,'doPreprocessing',value=TRUE)
#     updateTextInput(session,'mydata',value="mutated")
#     updateSelectInput(session,'PreProcessingExample',selected=0)
#     
#   } else putmsg("계산하여 추가할 내용을 입력한 후 버튼을 누르세요")
#   
# })
# 
# observeEvent(input$filter,{
#   
#   if(input$filtercondition!="") {
#     
#     temp=paste0("filtered=filter(",input$mydata,",",input$filtercondition,")")
#     updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
#     updateCheckboxInput(session,'doPreprocessing',value=TRUE)
#     updateTextInput(session,'mydata',value="filtered")
#     updateSelectInput(session,'PreProcessingExample',selected=0)
#     
#   } else putmsg("검색할 조건을 입력한 후 버튼을 누르세요")
#   
# })
# 
# observeEvent(input$sort,{
#   
#   if(!is.null(input$sortvar1) |!is.null(input$sortvar2)){
#     temp=paste0("sorted=arrange(",input$mydata,",")
#     if(!is.null(input$sortvar1)) {
#        
#       temp=paste0(temp,makesortorder(input$sortvar1,input$desc1))
#     }
#     if(!is.null(input$sortvar2)) {
#       
#       temp=paste0(temp,ifelse(is.null(input$sortvar1),"",","),
#                   makesortorder(input$sortvar2,input$desc2))
#     }
#     temp=paste0(temp,")")   
#     updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
#     updateCheckboxInput(session,'doPreprocessing',value=TRUE)
#     updateTextInput(session,'mydata',value="sorted")
#     updateSelectInput(session,'PreProcessingExample',selected=0)
#     
#   } else putmsg("검색할 조건을 입력한 후 버튼을 누르세요")
#   
# })
# 
# observeEvent(input$select,{
#   
#   if(!is.null(input$selectcol)) {
#     temp=paste0("selected=dplyr::select(",input$mydata,",c(",makesortorder(input$selectcol,FALSE),"))")
#     
#     updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
#     updateCheckboxInput(session,'doPreprocessing',value=TRUE)
#     updateTextInput(session,'mydata',value="selected")
#     updateSelectInput(session,'PreProcessingExample',selected=0)
#     
#   } else putmsg("열을 선택한 후 버튼을 누르세요")
#   
# })

observeEvent(input$long2wide,{
        if((input$value.var2!="None") & (length(input$id.var2)*length(input$measure.vars2)!=0)){
                idvar=vector2form(input$id.var2)
                measurevar=vector2form(input$measure.vars2)
                temp=paste0("casted=reshape2::dcast(",input$mydata)
                temp=mypaste(temp,idvar,"~",measurevar)
                temp=mypaste(temp,"value.var='",input$value.var2,"')")
                updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
                updateCheckboxInput(session,'doPreprocessing',value=TRUE)
                olddata<<-c(olddata,input$mydata)    
                updateTextInput(session,'mydata',value="casted")
                updateSelectInput(session,"PreProcessingExample",selected=0)
                
                
        } else putmsg("Please select the variable and press button")
        
})

observeEvent(input$changecolname,{
        if((input$oldcolname!="None")&(input$newcolname!="")) {
                if(input$mydata=="mutated") newdata<-"changed"
                else newdata<-"mutated"
                temp=paste0(newdata,"<-",input$mydata,";")
                mydf=input$mydata
                temp=paste0(temp,"names(",newdata,")[names(",newdata,")=='",input$oldcolname,"']<-'",
                            input$newcolname,"'")
                temp=addpaste(input$preprocessing,temp)
                updateTextInput(session,'preprocessing',value=temp)
                updateCheckboxInput(session,'doPreprocessing',value=TRUE)
                olddata<<-c(olddata,input$mydata)  
                updateTextInput(session,'mydata',value=newdata)
                updateSelectInput(session,"PreProcessingExample",selected=0)
                
                
        } else putmsg("이름을 바꿀 열과 새 이름을 입력하시고 버튼을 누르세요")
        
})

observeEvent(input$rankcol,{
        if((input$base!="None")&(input$rankname!="")) {
                if(input$mydata=="ranked") newdata<-"ranked2"
                else newdata<-"ranked"
                temp=paste0(newdata,"<-",input$mydata,";")
                mydf=input$mydata
                temp=paste0(temp,newdata,"$",input$rankname,"<-rank2group(",newdata,"$",input$base,",",
                            input$rankk,")")
                temp=addpaste(input$preprocessing,temp)
                updateTextInput(session,'preprocessing',value=temp)
                updateCheckboxInput(session,'doPreprocessing',value=TRUE)
                olddata<<-c(olddata,input$mydata)  
                updateTextInput(session,'mydata',value=newdata)
                updateSelectInput(session,"PreProcessingExample",selected=0)
                
                
        } else putmsg("Please select criteria variable, enter new name and press button.")
        
})

observeEvent(input$makefactor,{
        if(input$oldfname!="None") {
                if(c("factor") %in% class(df()[[input$oldfname]])){
                        putmsg("You select a categorical variable.")
                } else {
                        if((!is.null(input$relevel)) & (length(input$relevel)!=length(unique(df()[[input$oldfname]])))){
                                putmsg("Please specify orders all or none")
                        } else{
                                if(input$mydata=="mutated") newdata<-"changed"
                                else newdata<-"mutated"
                                temp=paste0(newdata,"<-dplyr::mutate(",input$mydata,",")
                                if(input$newfname!="") temp=paste0(temp,input$newfname)
                                else temp=paste0(temp,input$oldfname)
                                temp=paste0(temp,"=factor(",input$oldfname)
                                if(is.null(input$relevel)) temp=paste0(temp,"))")
                                else temp=paste0(temp,",levels=",vector2str(input$relevel),"))")
                                temp=addpaste(input$preprocessing,temp)
                                updateTextInput(session,'preprocessing',value=temp)
                                updateCheckboxInput(session,'doPreprocessing',value=TRUE)
                                olddata<<-c(olddata,input$mydata)  
                                updateTextInput(session,'mydata',value=newdata)
                                updateSelectInput(session,"PreProcessingExample",selected=0)
                        }
                        
                }
                
        } else putmsg("Please select a column be factorized, enter new name and press button.")
        
})

observeEvent(input$delcol,{
        
        if(!is.null(input$delcolname)) {
                
                temp=paste0("deleted<-subset(",input$mydata,",select=",vector2str(input$delcolname,del=TRUE),")")
                updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
                updateCheckboxInput(session,'doPreprocessing',value=TRUE)
                olddata<<-c(olddata,input$mydata)
                updateTextInput(session,'mydata',value="deleted")
                updateSelectInput(session,"PreProcessingExample",selected=0)
                
                
        } else putmsg("Please select column(s) and press the button.")
        
})

observeEvent(input$addcol,{
        
        if(input$colnameadd != "") {
                temp=paste0("added=",input$mydata,";")
                temp=paste0(temp,"added$",input$colnameadd,"<-1:nrow(added)")
                updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
                updateCheckboxInput(session,'doPreprocessing',value=TRUE)
                olddata<<-c(olddata,input$mydata)
                updateTextInput(session,'mydata',value="added")
                updateSelectInput(session,"PreProcessingExample",selected=0)
                
        } else putmsg("please enter new name and press button.")
        
})

observeEvent(input$addcol2,{
        
        if(input$addcolorder!="") {
                if(input$mydata=="mutated") newdata<-"changed"
                else newdata<-"mutated"
                temp=paste0(newdata,"=dplyr::mutate(",input$mydata,",",input$addcolorder,")")
                updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
                updateCheckboxInput(session,'doPreprocessing',value=TRUE)
                olddata<<-c(olddata,input$mydata)
                updateTextInput(session,'mydata',value=newdata)
                updateSelectInput(session,"PreProcessingExample",selected=0)
                
                
        } else putmsg("Please enter how to compute and press the button")
        
})

observeEvent(input$filter,{
        
        if(input$filtercondition!="") {
                
                temp=paste0("filtered=dplyr::filter(",input$mydata,",",input$filtercondition,")")
                updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
                updateCheckboxInput(session,'doPreprocessing',value=TRUE)
                olddata<<-c(olddata,input$mydata)
                updateTextInput(session,'mydata',value="filtered")
                updateSelectInput(session,"PreProcessingExample",selected=0)
                
                
        } else putmsg("Please enter the filter condition and press the button.")
        
})

observeEvent(input$sort,{
        
        if(!is.null(input$sortvar1) |!is.null(input$sortvar2)){
                temp=paste0("sorted=arrange(",input$mydata,",")
                if(!is.null(input$sortvar1)) {
                        
                        temp=paste0(temp,makesortorder(input$sortvar1,input$desc1))
                }
                if(!is.null(input$sortvar2)) {
                        
                        temp=paste0(temp,ifelse(is.null(input$sortvar1),"",","),
                                    makesortorder(input$sortvar2,input$desc2))
                }
                temp=paste0(temp,")")   
                updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
                updateCheckboxInput(session,'doPreprocessing',value=TRUE)
                olddata<<-c(olddata,input$mydata)
                updateTextInput(session,'mydata',value="sorted")
                updateSelectInput(session,"PreProcessingExample",selected=0)
                
        } else putmsg("Please select the column and press the button.")
        
})

observeEvent(input$select,{
        
        if(!is.null(input$selectcol)) {
                temp=paste0("selected=dplyr::select(",input$mydata,",c(",makesortorder(input$selectcol,FALSE),"))")
                
                updateTextInput(session,'preprocessing',value=addpaste(input$preprocessing,temp))
                updateCheckboxInput(session,'doPreprocessing',value=TRUE)
                olddata<<-c(olddata,input$mydata)
                updateTextInput(session,'mydata',value="selected")
                updateSelectInput(session,"PreProcessingExample",selected=0)
                
        } else putmsg("Please select the column and press the button.")
        
})


makesortorder=function(var,condition){
   temp=""
   for(i in 1:length(var)){
       temp1=""
       if(condition) temp1="desc("
       temp1=paste0(temp1,var[i])
       if(condition) temp1=paste0(temp1,")")
       temp=mypaste(temp,temp1)
   }
   temp
}
  


observeEvent(input$reglabel,{
  
  if((input$xvar!="None") & (input$yvar!="None")) {
    
    temp=lm2equation(input$mydata,input$xvar,input$yvar,input$annparse)
    updateTextInput(session,'annlabel',value=temp)
    
  } else putmsg("Please select x-axis and y-axis variables and press the button.")
  
})

observeEvent(input$regtable,{
  
  if((input$xvar!="None") & (input$yvar!="None")) {
    
    
    temp=paste0("
res=lm2table('",input$mydata,"','",input$xvar,"','",input$yvar,"')
table_grob=tableGrob(res)")
    temp=paste0(input$preprocessing,temp)
    updateTextInput(session,'preprocessing',value=temp)
    updateCheckboxInput(session,'doPreprocessing',value=TRUE)
    updateTextInput(session,'grob',value="table_grob")
    
  } else putmsg("Please select x-axis and y-axis variables and press the button.")
  
})
  
var2none=function(){
  #putmsg("var2none")
  myselectlist=c("xvar","yvar","colorvar","fillvar","sizevar","shapevar","alphavar",
                 "linetypevar","palette","facetrow","facetcol","facetwrap")
  for (i in 1:length(myselectlist)) updateSelectInput(session,myselectlist[i],selected="None")
  updateSelectInput(session,"fonts",choices=c("NA","Helvetica","Times","Courier","NanumGothic",fonts()),selected="Helvetica")
}



observe({
  mydf=df()
  
  contvar=ContinuousVar(mydf)
  groupvar=GroupVar(mydf)
  
  ggchoice=c("None",colnames(mydf))
  ggchoice2=colnames(mydf)
  
  # shinyjs::html("dbinfo",paste("자료개수:",nrow(mydf),", 열개수:",ncol(mydf),
  #                              ", 누락된 자료:",sum(!complete.cases(mydf))))
  # shinyjs::onclick("update",{
  #   df=df()
  #   shinyjs::html("dbinfo",paste("자료개수:",nrow(df),", 열개수:",ncol(df),
  #                                                        ", 누락된 자료:",sum(!complete.cases(df))))})
  updateSelectInput(session,'xvar',choices=c(ggchoice,1),selected=input$xvar)
       updateSelectInput(session,'yvar',choices=c(ggchoice,"..density.."),selected=input$yvar)
    updateSelectInput(session,'groupvar',choices=c(ggchoice,1),selected=input$groupvar)
    updateSelectInput(session,'colorvar',choices=ggchoice,selected=input$colorvar)
    updateSelectInput(session,'fillvar',choices=c(ggchoice,"..count.."),selected=input$fillvar)
    updateSelectInput(session,'sizevar',choices=c(ggchoice,"..prop.."),selected=input$sizevar)
    updateSelectInput(session,'shapevar',choices=ggchoice,selected=input$shapevar)
    updateSelectInput(session,'alphavar',choices=ggchoice,selected=input$alphavar)
    updateSelectInput(session,'linetypevar',choices=ggchoice,selected=input$linetypevar)
    #updateSelectInput(session,'textlabel',choices=c("None",paste0("rownames(",input$mydata,")"),colnames(mydf)),selected=input$textlabel)
    updateSelectInput(session,'facetrow',choices=ggchoice,selected=input$facetrow)
    updateSelectInput(session,'facetcol',choices=ggchoice,selected=input$facetcol)
    updateSelectInput(session,'facetwrap',choices=ggchoice,selected=input$facetwrap)
    updateSelectInput(session,"fonts",choices=c("NA","Helvetica","Times","Courier","NanumGothic",fonts()),selected=input$fonts)
    updateSelectInput(session,'measurevar1',choices=colnames(mydf))
    
    updateSelectInput(session,'id.var',choices=colnames(mydf))
    updateSelectInput(session,'sortvar1',choices=ggchoice2)
    updateSelectInput(session,'selectcol',choices=ggchoice2)
    updateSelectWithout('value.var2',ggchoice2,c(input$id.var2,input$measure.vars2))
})

observe({
  mydf=df()
  
  ggchoice2=colnames(mydf)
    
  updateSelectWithout('sortvar2',ggchoice2,input$sortvar1)
  updateSelectWithout('groupvar1',ggchoice2,input$measurevar1)
  updateSelectWithout('measure.vars',ggchoice2,input$id.var)
  if(input$oldfname!="None") updateSelectInput(session,'relevel',choices=unique(mydf[[input$oldfname]]))
  updateSelectWithout('measure.vars2',ggchoice2,input$id.var2)
  updateSelectInput(session,'base',choices=c("None",ContinuousVar(mydf)))
  
})

updateSelectWithout=function(id,choices,exclude){
  if(!is.null(exclude)) updateSelectInput(session,id,
             choices=excludechoice(exclude,choices))
  else updateSelectInput(session,id,choices=choices) 
}

excludechoice=function(A,full){
    result=full
    if(!is.null(A)) {
         for(i in 1:length(A)){
            result=result[-grep(A[i],result)]
        } 
    }
    result
}


observe({
  mydf=df()
  
  contvar=ContinuousVar(mydf)
  groupvar=GroupVar(mydf)
  
  ggchoice=c("None",colnames(mydf))
  ggchoice2=colnames(mydf)
  
    updateSelectInput(session,'id.var2',choices=colnames(mydf))
    
    updateSelectInput(session,'oldcolname',choices=ggchoice)
    updateSelectInput(session,'oldfname',choices=ggchoice)
    updateSelectInput(session,'delcolname',choices=colnames(mydf))
    updateSelectInput(session,'contourzvar',choices=ggchoice,selected=input$contourzvar)
    if(input$anngeom=="rect") {
       updateTextInput(session,"annx",label="xmin")
       updateTextInput(session,"annxend",label="xmax")
       updateTextInput(session,"anny",label="ymin")
       updateTextInput(session,"annyend",label="ymax")
    } else if(input$anngeom=="pointrange") {
      updateTextInput(session,"annx",label="x")
      updateTextInput(session,"annxend",label="ymin")
      updateTextInput(session,"anny",label="y")
      updateTextInput(session,"annyend",label="ymax")
    } else {
      updateTextInput(session,"annx",label="x")
      updateTextInput(session,"annxend",label="xend")
      updateTextInput(session,"anny",label="y")
      updateTextInput(session,"annyend",label="yend")
    }
    updateSelectInput(session,'mapfill',choices=c("NA",colnames(mydf),colors()),selected=input$mapfill)
})

observe({
    
    updateTextInput(session,'inset1vp',value=choice2vp(input$inset1choice))
    updateTextInput(session,'inset2vp',value=choice2vp(input$inset2choice))
    updateTextInput(session,'inset3vp',value=choice2vp(input$inset3choice))
    updateTextInput(session,'inset4vp',value=choice2vp(input$inset4choice))
})

observe({
    if(input$location!=""){
      tryCatch(geocode(input$location),error=function(e){test1(e)})
      loc<-geocode(input$location)
      locn=as.numeric(loc)
      #putmsg(locn[1])
      if(!is.na(locn[1])) {
      updateTextInput(session,"lon",value=locn[1])
      updateTextInput(session,"lat",value=locn[2])
      }
      
    }
})



fheight=function(){ 
  
## plotWidth 및 plotHeight의 비율에 맞게 plot size 조절, fwidth를 900으로
if(input$plotUnit=="in"){
  mywidth=input$plotWidth*input$plotRes
  myheight=input$plotHeight*input$plotRes
} else if(input$plotUnit=="cm"){
  mywidth=(input$plotWidth*input$plotRes)/2.54
  myheight=(input$plotHeight*input$plotRes)/2.54
} else{
  mywidth=(input$plotWidth*input$plotRes)/25.4
  myheight=(input$plotHeight*input$plotRes)/25.4
}
fwidth=900
fheight=(myheight*fwidth)/mywidth
fheight
}

fheight2=function(){ 
  
  ## plotWidth 및 plotHeight의 비율에 맞게 plot size 조절, fwidth를 900으로
  if(input$plotUnit=="in"){
    mywidth=input$plotWidth*input$plotRes
    myheight=input$plotHeight*input$plotRes
  } else if(input$plotUnit=="cm"){
    mywidth=(input$plotWidth*input$plotRes)/2.54
    myheight=(input$plotHeight*input$plotRes)/2.54
  } else{
    mywidth=(input$plotWidth*input$plotRes)/25.4
    myheight=(input$plotHeight*input$plotRes)/25.4
  }
  fwidth=640
  fheight=(myheight*fwidth)/mywidth
  fheight
}

fheight3=function(){   #Multiplot
  
  ## plotWidth 및 plotHeight의 비율에 맞게 plot size 조절, fwidth를 900으로
  if(input$plotUnit2=="in"){
    mywidth=input$plotWidth2*input$plotRes2
    myheight=input$plotHeight2*input$plotRes2
  } else if(input$plotUnit2=="cm"){
    mywidth=(input$plotWidth2*input$plotRes2)/2.54
    myheight=(input$plotHeight2*input$plotRes2)/2.54
  } else{
    mywidth=(input$plotWidth2*input$plotRes2)/25.4
    myheight=(input$plotHeight2*input$plotRes2)/25.4
  }
  fwidth=900
  fheight=(myheight*fwidth)/mywidth
  fheight
}



doExample=function(example){
  if(example!=0) resetPlot()
  if(example==1){
    updateTextInput(session,"mydata",value="subset(countries,Year==2009 & healthexp>2000)")  
    
    mydf=df()
    ggchoice=c("None",colnames(df()))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="healthexp")
    updateSelectInput(session,"yvar",choices=c(ggchoice,"..density.."),selected="infmortality")
    updateSelectInput(session,"sizevar",choices=ggchoice,selected="GDP")
    
    updateCheckboxInput(session,"pshape",value=TRUE)
    updateNumericInput(session,"pointshape",value=21)
    updateSelectInput(session,"pointfill",selected="cornsilk")
    updateCheckboxInput(session,"sizearea",value=TRUE)
    updateNumericInput(session,"maxsize",value=15)
    updateCheckboxInput(session,"point",value=TRUE)
  }
  else if(example==2){
    updateTextInput(session,"mydata",value="faithful")
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="waiting")
    updateSelectInput(session,"yvar",choices=c(ggchoice,"..density.."),selected="..density..")
    #updateSelectInput(session,"sizevar",selected="GDP")
    
    updateSelectInput(session,"histfill",selected="cornsilk")
    updateSelectInput(session,"histcolor",selected="grey60")
    updateCheckboxInput(session,"density",value=TRUE)
    updateSelectInput(session,"linestat",selected="density")
    updateSelectInput(session,"densitycolor",selected="red")
    updateTextInput(session,"rordertext",value="xlim(35,105)")  
    updateCheckboxInput(session,"histogram",value=TRUE)
    updateCheckboxInput(session,"addrorder",value=TRUE)
  }
  else if(example==3){
    updateTextInput(session,"mydata",value="acs")
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="Dx")
    updateSelectInput(session,"yvar",choices=c(ggchoice,"..density.."),selected="age")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="Dx")
    updateSelectInput(session,"palette",selected="Set2")
    updateSelectInput(session,"boxfill",selected="black")
    updateNumericInput(session,"boxwidth",value=0.1)
    updateNumericInput(session,"boxalpha",value=0.6)
    updateSelectInput(session,"violinscale",selected="count")
    updateCheckboxInput(session,"violintrim",value=FALSE)
    updateCheckboxInput(session,"statsummary",value=TRUE)
    updateSelectInput(session,"sumfill",selected="white")
    updateSelectInput(session,"sumfuny",selected="median")
    updateNumericInput(session,"sumshape",value=21)
    updateNumericInput(session,"sumsize",value=3.5)
    updateCheckboxInput(session,"boxplot",value=TRUE)
    updateCheckboxInput(session,"violin",value=TRUE)
    
  } else if(example==4){
    updateTextInput(session,"mydata",value="heightweight")
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="sex")
    updateSelectInput(session,"yvar",choices=c(ggchoice,"..density.."),selected="heightIn")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count"),selected="sex")
    updateSelectInput(session,"boxfill",selected="white")
    updateNumericInput(session,"boxwidth",value=.4)
    updateSelectInput(session,"dotbinaxis",selected="y")
    updateSelectInput(session,"dotstackdir",selected="center")
    updateNumericInput(session,"dotbinwidth",value=.5)
    updateNumericInput(session,"dotalpha",value=.9)
    updateCheckboxInput(session,"boxplot",value=TRUE)
    updateCheckboxInput(session,"dotplot",value=TRUE)
    
  }else if(example==5){  
    updateTextInput(session,"mydata",value="faithful")
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="eruptions")
    updateSelectInput(session,"yvar",choices=c(ggchoice,"..density.."),selected="waiting")
    updateSelectInput(session,'tdcolor',selected="..level..")
    updateCheckboxInput(session, 'point',value=TRUE)
    updateCheckboxInput(session, 'statdensity2d',value=TRUE)

  } else if(example==6){  
    updateTextInput(session,"mydata",value="faithful")
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="eruptions")
    updateSelectInput(session,"yvar",choices=c(ggchoice,"..density.."),selected="waiting")
    
 
    updateTextInput(session,'tdalpha',value="..density..")
    updateSelectInput(session,'tdgeom',selected="tile")
    updateCheckboxInput(session,'tdcontour',value=FALSE)
    updateCheckboxInput(session, 'statdensity2d',value=TRUE)
    updateCheckboxInput(session, 'point',value=TRUE)
    
} else if(example==7){  
    updateTextInput(session,"mydata",value="faithful")
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="eruptions")
    updateSelectInput(session,"yvar",choices=c(ggchoice,"..density.."),selected="waiting")
    
    updateTextInput(session,'tdfill',value="..density..")
    updateSelectInput(session,'tdgeom',selected="raster")
    updateCheckboxInput(session,'tdcontour',value=FALSE)
    updateTextInput(session,'tdh',value="c(.5,5)")
    updateCheckboxInput(session, 'statdensity2d',value=TRUE)
    
 } else if(example==8){
    pre1=
      "pg <- PlantGrowth
    pg$hl<-'no'
    pg$hl[pg$group=='trt2']<-'yes'"
    updateTextInput(session,"preprocessing",value=pre1)
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="pg")  
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="group")
    updateSelectInput(session,"yvar",choices=c(ggchoice,"..density.."),selected="weight")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="hl")
    updateTextInput(session,"rordertext",value="scale_fill_manual(values=c('grey85','#FFDDCC'),guide=FALSE)")
    updateTextInput(session,"title",value="항목 강조를 위한 전처리")
    updateCheckboxInput(session, 'boxplot',value=TRUE)
    updateCheckboxInput(session, 'addrorder',value=TRUE)
    
    
  } else if(example==9){
    
    updateTextInput(session,"preprocessing",value=
                      "mpg2<-mpg
                    levels(mpg2$drv)<-c('4WD','Front','Rear')
                    lm_labels <- function(dat) {
                    mod <- lm(hwy ~ displ, data=dat)
                    formula <- sprintf('italic(y) == %.2f %+.2f * italic(x)',
                    round(coef(mod)[1], 2), round(coef(mod)[2], 2))
                    r <- cor(dat$displ, dat$hwy)
                    r2 <- sprintf('italic(R^2) == %.2f', r^2)
                    data.frame(formula=formula, r2=r2, stringsAsFactors=FALSE)
                    }
                    labels <- ddply(mpg2, 'drv', lm_labels)")                           
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="mpg2")  
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="displ")
    updateSelectInput(session,"yvar",choices=c(ggchoice,"..density.."),selected="hwy")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="drv")
    updateSelectInput(session,"facetcol",choices=ggchoice,selected="drv")
    
    updateCheckboxInput(session, 'pshape',value=TRUE)
    updateNumericInput(session, 'pointshape',value=21)
    updateCheckboxInput(session, 'psize',value=TRUE)
    updateNumericInput(session, 'pointsize',value=3)
    
    updateSelectInput(session,"smoothmethod",selected="lm")
    updateSelectInput(session,"legendposition",selected="top")
    
    updateTextInput(session,"rordertext",value=
                      "geom_text(x=3, y=40, family='Times',fontface='italic',aes(label=formula), data=labels, parse=TRUE, hjust=0) +
                    geom_text(x=3.5, y=35, family='Times',fontface='italic',aes(label=r2), data=labels, parse=TRUE, hjust=0)")
    updateTextInput(session,"title",value="Adding annotation to each facets")
    updateTextInput(session,"xlab",value="displacement")
    updateTextInput(session,"ylab",value="highway mpg")
    updateNumericInput(session,"fontsize",value=14)
    updateCheckboxInput(session, 'point',value=TRUE)
    updateCheckboxInput(session, 'smooth',value=TRUE)
    updateCheckboxInput(session, 'addrorder',value=TRUE)
  }   
  else if(example==10){
    updateTextInput(session,"mydata",value="acs")  
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="age")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="sex")
    updateNumericInput(session,"densityalpha",value=0.4)
    updateCheckboxInput(session,"density",value=TRUE)
    
  }
  else if(example==11){
    updateTextInput(session,"mydata",value="Salaries")
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="yrs.since.phd")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="salary")
    updateCheckboxInput(session,'point',value=TRUE)
    updateTextInput(session,"title",value="Staff Career and Salary") 
    temp1=c("colorvar","shapevar")
    for(i in 1:length(temp1)) updateSelectInput(session,temp1[i],choices=ggchoice,selected="rank")
    
  }
  else if(example==12){
    updateTextInput(session,"mydata",value="Salaries")
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="rank")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="salary")
    updateSelectInput(session,"boxfill",selected="cornflowerblue")
    updateSelectInput(session,"pointposition",selected="jitter")
    updateSelectInput(session,"pointcolor",selected="blue")
    updateCheckboxInput(session,"boxnotch",value=TRUE)
    updateNumericInput(session,"boxalpha",value=0.5)
    updateTextInput(session,"title",value="교수의 직급과 연봉") 
    temp1=c("boxplot","point","rug")
    for(i in 1:length(temp1)) updateCheckboxInput(session,temp1[i],value=TRUE)
    
  }
  else if(example==13){
    updateTextInput(session,"mydata",value="Salaries")
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="salary")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="rank")
    updateNumericInput(session,"densityalpha",value=0.3)
    updateTextInput(session,"title",value="교수의 직급과 연봉") 
    updateCheckboxInput(session,"density",value=TRUE)
  }
  else if(example==14){
    updateTextInput(session,"mydata",value="singer")  
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="voice.part")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="height")
    updateTextInput(session,"title",value="Singer's Height") 
    updateCheckboxInput(session,"boxplot",value=TRUE)
    
  }
  else if(example==15){
    updateTextInput(session,"mydata",value="singer")  
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="voice.part")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="height")
    updateSelectInput(session,"boxfill",selected="lightgreen")
    updateNumericInput(session,"boxwidth",value=0.2)
    updateSelectInput(session,"violinfill",selected="lightblue")
    updateTextInput(session,"title",value="Singer's Height(2)") 
    updateCheckboxInput(session,"boxplot",value=TRUE)
    updateCheckboxInput(session,"violin",value=TRUE)
    
  
    } else if(example==16){
    updateTextInput(session,"mydata",value="singer")  
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="height")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="voice.part")
    updateSelectInput(session,"facetrow",choices=ggchoice,selected="voice.part")
    updateTextInput(session,"title",value="Singer's Height(3)") 
    updateCheckboxInput(session,"density",value=TRUE)
    
  }else if(example==17){
    updateTextInput(session,"mydata",value="Salaries")
    
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    
    updateSelectInput(session,"xvar",choices=ggchoice,selected="yrs.since.phd")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="salary")
    updateSelectInput(session,"colorvar",choices=ggchoice,selected="rank")
    updateSelectInput(session,"shapevar",choices=ggchoice,selected="sex")
    updateTextInput(session,"title",value="Career and salary ") 
    updateCheckboxInput(session,"point",value=TRUE)
    
  } else if(example==18){
    updateTextInput(session,"preprocessing",value="kpmap=readRDS('kpopmap.rds')")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="kpmap")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="long")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="lat")
    updateSelectInput(session,"groupvar",choices=c(ggchoice,1),selected="group")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="외국인_계_명")
    
    updateCheckboxInput(session,"coordmap",value=TRUE)
    updateSelectInput(session,"polygoncolor",selected="black")
    updateNumericInput(session,"polygonsize",value=0.2)
    updateTextInput(session,"title",value=paste0("Population distribution in South Korea"))
    updateTextInput(session,"cpalette",value="'white','orange','red'") 
    updateCheckboxInput(session,"polygon",value=TRUE)
  }  
  else if(example==19){
    updateTextInput(session,"preprocessing",value="seoulpmap=readRDS('seoulpopmap.rds')")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="seoulpmap")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="long")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="lat")
    updateSelectInput(session,"groupvar",choices=c(ggchoice,1),selected="group")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="총인구_명")
    updateCheckboxInput(session,"coordfixed",value=TRUE)
    updateSelectInput(session,"polygoncolor",selected="white")
    updateNumericInput(session,"polygonsize",value=0.2)
    updateTextInput(session,"title",value=paste0("Population distribution in Seoul"))
    updateTextInput(session,"cpalette",value="'white','orange','red'")
    updateCheckboxInput(session,"polygon",value=TRUE)
    
    }  else if(example==20){
    
    updateTextInput(session,"mydata",value="uspopage")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="Year")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="Thousands")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="AgeGroup")
    updateSelectInput(session,"palette",choices=c("None",rownames(brewer.pal.info)),selected="Blues")
    updateNumericInput(session,"areaalpha",value=0.4)
    updateCheckboxInput(session,"line",value=TRUE)
    updateNumericInput(session,"linesize",value=0.2)
    updateSelectInput(session,"lineposition",selected="stack")
    updateCheckboxInput(session,"area",value=TRUE)
    
    
  } else if(example==21){
    
    updateTextInput(session,"mydata",value="diamonds[sample(nrow(diamonds),1000),]")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="carat")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="price")
    updateSelectInput(session,"colorvar",choices=ggchoice,selected="cut")
    updateTextInput(session,"title",value="Diamonds Are Forever")
    updateCheckboxInput(session,"point",value=TRUE)
    
  } else if(example==22){
    updateTextInput(session,"mydata",value="wind")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="DirCat")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="SpeedCat")
  
    updateSelectInput(session,"histcolor",selected="black")
    updateCheckboxInput(session,"coord_polar",value=TRUE)
    updateSelectInput(session,"palette",selected="Blues")
    updateTextInput(session,"rordertext",
                    value="scale_x_continuous(limits=c(0,360),breaks=seq(0,360,by=45))")
    updateCheckboxInput(session,"legendreverse",value=TRUE)
    updateCheckboxInput(session,"histogram",value=TRUE)
    updateCheckboxInput(session, 'addrorder',value=TRUE)

  } else if(example==23){  
   
    updateTextInput(session,"mydata",value="taco")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="AgeGroup")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="Filling")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="Rating")
    updateSelectInput(session,"tilecolor",selected="white")
    updateTextInput(session,"cpalette",value="'white','steelblue'")
    updateSelectInput(session,"facetcol",choices=ggchoice,selected="ShellType")
    updateSelectInput(session,"theme",selected="bw")
    updateCheckboxInput(session,"tile",value=TRUE)
    
  } else if(example==24){  
   
    updateTextInput(session,"mydata",value="taco")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="Filling")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="Rating")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="Filling")
    updateNumericInput(session,'boxalpha',value=0.5)
    updateSelectInput(session,"facetcol",choices=ggchoice,selected="ShellType")
    updateCheckboxInput(session,"coordflip",value=TRUE)
    updateSelectInput(session,"theme",selected="bw")
    updateSelectInput(session,"legendposition",selected="none")
    updateCheckboxInput(session,"boxplot",value=TRUE)
  } else if(example==27){  
    updateTextInput(session,"preprocessing",value=
"
taco.anova <- aov(Rating~ShellType*AgeGroup,data = taco)
taco.hsd <- data.frame(TukeyHSD(taco.anova,'AgeGroup', conf.level=.95)$AgeGroup)
taco.hsd$Comparison <- row.names(taco.hsd)")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="taco.hsd")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="Comparison")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="diff")
    updateSelectInput(session,"colorvar",choices=ggchoice,selected="Comparison")
    updateNumericInput(session,"pointsize",value=4)
    updateNumericInput(session,"ebsize",value=1)
    
    updateTextInput(session,"ymin",value="lwr")
    updateTextInput(session,"ymax",value="upr")
    updateTextInput(session,"ylab",value="Difference in Mean Rating by Age Group")
    updateCheckboxInput(session,"coordflip",value=TRUE)
    updateSelectInput(session,"legendposition",selected="none")
    updateCheckboxInput(session,"point",value=TRUE)
    updateCheckboxInput(session,"errorbar",value=TRUE)
    
  } else if(example==28){  
    updateTextInput(session,"preprocessing",value=
                      "filling.results <- taco %>%
group_by(Filling) %>%
summarise(Rating = mean(Rating))")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="filling.results")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="Filling")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="Rating")
    updateSelectInput(session,"fillvar",choices=ggchoice,selected="Filling")
    updateSelectInput(session,"barstat",selected="identity")
    updateCheckboxInput(session,"coordflip",value=TRUE)
    updateTextInput(session,"ylim",value="0.8,0.875")
    updateSelectInput(session,"theme",selected="bw")
    updateSelectInput(session,"legendposition",selected="none")
    updateCheckboxInput(session,"bar",value=TRUE)
    
  } else if(example==25){  #barchaart with errorbar  
    updateTextInput(session,"preprocessing",value="df=summarySE(Salaries,'salary',c('sex','rank','rank'),conf.interval=0.95)")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="df")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="rank")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="salary")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="sex")
    updateSelectInput(session,"groupvar",choices=c(ggchoice,1),selected="sex")
    updateSelectInput(session,"colorvar",choices=ggchoice,selected="sex")
    updateSelectInput(session,"barstat",selected="identity")
    updateSelectInput(session,"barposition",selected="dodge")
    updateTextInput(session,"ymin",value="salary-se")
    updateTextInput(session,"ymax",value="salary+se")
    updateSelectInput(session,"ebcolor",selected="black")
    updateNumericInput(session,"ebpos",value=0.9)
    updateCheckboxInput(session,"bar",value=TRUE)
    updateCheckboxInput(session,"errorbar",value=TRUE)
    
  } else if(example==26){  #barchaart with errorbar  
    updateTextInput(session,"preprocessing",value="df=summarySE(Salaries,'salary',c('sex','rank','rank'),conf.interval=0.95)")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="df")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="sex")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="salary")
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="rank")
    updateSelectInput(session,"groupvar",choices=c(ggchoice,1),selected="rank")
    updateSelectInput(session,"colorvar",choices=ggchoice,selected="rank")

    updateSelectInput(session,"pointposition",selected="dodge(0.3)")
    updateCheckboxInput(session,"line",value=TRUE)
    updateSelectInput(session,"lineposition",selected="dodge(0.3)")

    updateTextInput(session,"ymin",value="salary-se")
    updateTextInput(session,"ymax",value="salary+se")
    updateNumericInput(session,"ebpos",value=0.3)
    updateCheckboxInput(session,"errorbar",value=TRUE)
    updateCheckboxInput(session,"point",value=TRUE)
    
  } else if(example==29){  # Cleveland dot plot  
    updateTextInput(session,"preprocessing",value="tophit=tophitters2001[1:25,]")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="tophit")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"yvar",choices=ggchoice,selected="name")
    updateSelectInput(session,"xvar",choices=ggchoice,selected="avg")

    updateNumericInput(session,"pointsize",value=3)
    updateNumericInput(session,"pointalpha",value=1)
    updateTextInput(session,"segmentyend",value="name")
    updateTextInput(session,"segmentxend",value="0")
    updateCheckboxInput(session,"segment",value=TRUE)
    updateCheckboxInput(session,"point",value=TRUE)

  } else if(example==30){  # Cleveland dot plot  
    updateTextInput(session,"preprocessing",value="tophit=tophitters2001[1:25,]")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="tophit")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"yvar",choices=ggchoice,selected="name")
    updateSelectInput(session,"xvar",choices=ggchoice,selected="avg")
    updateSelectInput(session,"colorvar",choices=ggchoice,selected="name")
    updateCheckboxInput(session,"reordery",value=TRUE)

    updateNumericInput(session,"pointsize",value=3)
    updateNumericInput(session,"pointalpha",value=1)
    updateTextInput(session,"segmentyend",value="name")
    updateTextInput(session,"segmentxend",value="0")

    updateSelectInput(session,"theme",selected="bw")
    updateSelectInput(session,"legendposition",selected="none")
    updateCheckboxInput(session,"segment",value=TRUE)
    updateCheckboxInput(session,"point",value=TRUE)
    
  } else if(example==31){  # Cleveland dot plot3  
    updateTextInput(session,"preprocessing",value=
"tophit=tophitters2001[1:25,]
nameorder<-tophit$name[order(tophit$lg,tophit$avg)]
tophit$name<-factor(tophit$name,levels=nameorder)")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="tophit")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="avg")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="name")
    updateSelectInput(session,"colorvar",choices=ggchoice,selected="lg") 

    updateNumericInput(session,"pointsize",value=3)
    updateNumericInput(session,"pointalpha",value=1)
    updateTextInput(session,"segmentxend",value="0.31")
    updateTextInput(session,"segmentyend",value="name")

    updateSelectInput(session,"facetrow",selected="lg")
    updateSelectInput(session,"facetscales",selected="free_y")
    updateSelectInput(session,"facetspace",selected="free_y")
    updateSelectInput(session,"legendposition",selected="None") 
    updateSelectInput(session,"theme",selected="bw")
    updateCheckboxInput(session,"point",value=TRUE)
    updateCheckboxInput(session,"segment",value=TRUE)

  } else if(example==32){  # 화우화산의 등고선
    updateTextInput(session,"preprocessing",value=
                      "volcano3d=melt(volcano)
names(volcano3d)<-c('x','y','z')")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="volcano3d")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="x")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="y")

    updateSelectInput(session,"contourzvar",choices=ggchoice,selected="z")
    updateSelectInput(session,"contourcolor",selected="grey50")
    updateNumericInput(session,"contourbinwidth",value=2)
    updateTextInput(session,"rordertext",
                    value="stat_contour(aes(z=z),binwidth = 10, size = 1)")
    updateCheckboxInput(session, 'addrorder',value=TRUE)
    updateCheckboxInput(session,"statcontour",value=TRUE)
    
  } else if(example==33){  # US murdur rate
    updateTextInput(session,"preprocessing",value=
                      "crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
states_map <- map_data('state')")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="crimes")  
    mydf=df()
    updateTextInput(session,"mapid",value="state")
    updateTextInput(session,"mapmap",value="states_map")
    updateSelectInput(session,"mapfill",choices=c("NA",colnames(mydf),colors()),selected="Murder")
    updateCheckboxInput(session, 'coordmap',value=TRUE)
    updateTextInput(session,"projection",value="polyconic")
    updateCheckboxInput(session,"map",value=TRUE)
  } else if(example==34){  # US crime rate
    updateTextInput(session,"preprocessing",value=
                      "crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimesm <- melt(crimes, id = 1)
states_map <- map_data('state')")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="crimesm")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateTextInput(session,"mapid",value="state")
    updateTextInput(session,"mapmap",value="states_map")
    updateSelectInput(session,"mapfill",choices=c("NA",colnames(mydf),colors()),selected="value")
    updateSelectInput(session,"facetwrap",choices=c("NA",colnames(mydf)),selected="variable")
    updateCheckboxInput(session, 'coordmap',value=TRUE)
    updateTextInput(session,"cpalette",value="'white','green','blue','red'")
    updateNumericInput(session,"mapsize",value=0.1)
    updateSelectInput(session,"mapcolor",selected="grey50")
    updateCheckboxInput(session,"map",value=TRUE)
  } else if(example==35){

    updateCheckboxInput(session,"coordfixed",value=TRUE)
    updateNumericInput(session,"polygonsize",value=0.1)
    updateTextInput(session,"title",value=paste0(
      names(which(areachoices==input$maparea))," ",
      names(which(levelchoices==input$maplevel))," 인구 분포도"))
    updateTextInput(session,"cpalette",value="'white','orange','red'") 
    if((input$maparea!=0)& (input$maplevel==1)) {
      putmsg("시도경계지도는 전국에서만 가능합니다.")
      updateSelectInput(session,"maparea",selected=0)
    }  
    filename=paste0("dat<-readRDS('data/level",input$maplevel,".RDS')")
    if(input$maparea==0) filename=paste0(filename,"\nkpmap=dat")
    else filename=paste0(filename,"\nkpmap<-dat[grep('^",input$maparea,"',dat$code),]")
    if((input$maparea==0)& (input$maplevel==3)) updateSelectInput(session,"polygoncolor",selected="NA")
    else updateSelectInput(session,"polygoncolor",selected="black")
    
    updateTextInput(session,"preprocessing",value=filename)
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="kpmap")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected=input$mapselect)
    updateSelectInput(session,"xvar",choices=ggchoice,selected="long")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="lat")
    updateSelectInput(session,"groupvar",choices=ggchoice,selected="group")
    updateCheckboxInput(session,"polygon",value=TRUE)
    
  } else if(example==36){
    updateTextInput(session,"title",value="시도별 결핵 신환 발생 추이")
    filename=paste0("tbc<-readRDS('data/tbc_long.RDS')")
    updateTextInput(session,"preprocessing",value=filename)
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="tbc")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"fillvar",choices=c(ggchoice,"..count.."),selected="시도별")
    updateSelectInput(session,"xvar",choices=ggchoice,selected="year")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="신환수")
    updateSelectInput(session,"groupvar",choices=ggchoice,selected="시도별")

    updateCheckboxInput(session, 'legendreverse',value=TRUE)
    updateSelectInput(session,"lineposition",selected="stack")
    updateNumericInput(session,"areaalpha",value=0.4)
    updateCheckboxInput(session, 'annotate',value=TRUE)
    updateTextInput(session,"annx",value=3)
    updateTextInput(session,"annxend",value=11)
    updateTextInput(session,"anny",value=31000)
    updateTextInput(session,"annyend",value=40000)
    updateSelectInput(session,"anngeom",selected="segment")
    updateSelectInput(session,"anncolor",selected="red")
    updateTextInput(session,"annarrow",value="arrow()")
    updateNumericInput(session,"annsize",value=2)
    updateNumericInput(session,"annalpha",value=0.6)
    updateCheckboxInput(session, 'area',value=TRUE)
    updateCheckboxInput(session, 'line',value=TRUE)
    
  }  else if(example==37){
    updateTextInput(session,"title",value="시도별 결핵 신환 발생 분포도")
    
    updateTextInput(session,"preprocessing",value=
"tbc<-readRDS('data/tbc_long4.RDS')
map=kormap1")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="tbc")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"facetwrap",choices=ggchoice,selected="year")
    updateSelectInput(session,"fillvar",choices=ggchoice,selected="신환수")

    updateTextInput(session,"mapid",value="code")
    updateTextInput(session,"mapmap",value="map")
    updateSelectInput(session,"mapcolor",choices=c("NA",colors()),selected="black")
    updateNumericInput(session,"mapsize",value=0.1)
    updateTextInput(session,"cpalette",value="'white','red'")
    updateCheckboxInput(session, 'coordmap',value=TRUE)
    updateNumericInput(session,"plotHeight",value=7)
    updateCheckboxInput(session, 'axis.blank',value=TRUE)
    updateTextInput(session,"xlim",value="map$long")
    updateTextInput(session,"ylim",value="map$lat")
    updateCheckboxInput(session, 'map',value=TRUE)
    
  } else if(example==38){
    updateTextInput(session,"title",value="시도별 결핵 발병률 추이(2005-2010)")
    filename=paste0("tbc<-readRDS('data/tbcinci.RDS')")
    updateTextInput(session,"preprocessing",value=filename)
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="tbc")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="년도")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="발병률")
    updateSelectInput(session,"groupvar",choices=ggchoice,selected="시도별")
    updateSelectInput(session,"colorvar",choices=ggchoice,selected="시도별")

    updateSelectInput(session,"legendposition",selected="none")
    updateTextInput(session,"textdata",value="tbc[tbc$년도==2005,]")
    updateTextInput(session,"textlabel",value="시도별")
    updateTextInput(session,"texthjust",value="1.2")
    
    updateTextInput(session,"rordertext",
                    value=paste0("geom_text(data=tbc[tbc$년도==2010,],aes(label=시도별),family='",
                                 ifelse(input$fonts=='NA','Helvetica',input$fonts),"',hjust=-0.2)"))
    updateCheckboxInput(session, 'point',value=TRUE)
    updateCheckboxInput(session, 'line',value=TRUE)
    updateCheckboxInput(session, 'text',value=TRUE)
    updateCheckboxInput(session, 'addrorder',value=TRUE)
    
    
  }else if(example==39){
    updateTextInput(session,"title",value="시도별 결핵 발병률 분포도")
    
    updateTextInput(session,"preprocessing",value=
                      "tbc<-readRDS('data/tbcinci.RDS')
map=kormap1")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="tbc")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))

    updateSelectInput(session,"fillvar",choices=ggchoice,selected="발병률")
    updateTextInput(session,"mapid",value="code")
    updateTextInput(session,"mapmap",value="map")
    updateSelectInput(session,"mapcolor",choices=c("NA",colors()),selected="black")
    updateNumericInput(session,"mapsize",value=0.1)
    updateTextInput(session,"cpalette",value="'white','red'")
    updateCheckboxInput(session, 'coordmap',value=TRUE)
    updateNumericInput(session,"plotHeight",value=7)
    updateCheckboxInput(session, 'axis.blank',value=TRUE)
    updateTextInput(session,"xlim",value="map$long")
    updateTextInput(session,"ylim",value="map$lat")
    updateSelectInput(session,"legendposition",selected="top")
    updateSelectInput(session,"facetwrap",choices=ggchoice,selected="년도")
    updateCheckboxInput(session, 'map',value=TRUE)
    
  } else if(example==40){
    updateTextInput(session,"title",value="로지스틱 회귀분석")
    
    updateTextInput(session,"preprocessing",value=
                      "b<-biopsy
b$malig=ifelse(b$class=='malignant',1,0)")
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="b")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="V1")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="malig")
    updateNumericInput(session,"jitterwidth",value="0.3")
    updateNumericInput(session,"jitterheight",value="0.06")
    updateNumericInput(session,"jittersize",value="1.5")
    updateNumericInput(session,"jitteralpha",value="0.5")
    updateNumericInput(session,"jittershape",value="21")
    updateSelectInput(session,"smoothmethod",selected="glm")
    updateTextInput(session,"smoothfamily",value="binomial")
    updateCheckboxInput(session, 'smooth',value=TRUE)
    updateCheckboxInput(session, 'jitter',value=TRUE)
  } else if(example==41) {
    updateTextInput(session,"title",value="서울시 교통 돌발상황위치")
    
    updateTextInput(session,"preprocessing",value="traffic=read.csv('traffic.csv')")

    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="traffic")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="start.pos.x")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="start.pos.y")
    updateSelectInput(session,"colorvar",choices=ggchoice,selected="info.tit")
    updateNumericInput(session,"pointsize",value=4)
    updateNumericInput(session,"pointalpha",value=0.7)
    updateNumericInput(session,"zoom",value=11)
    #updateTextInput(session,"location",value="서울")
    updateTextInput(session,"lon",value="126.977969")
    updateTextInput(session,"lat",value="37.566535")
    updateSelectInput(session,"maptype",selected="roadmap")
    updateSelectInput(session,"legend",selected="topleft")
    updateCheckboxInput(session, 'qmap',value=TRUE)
    updateCheckboxInput(session, 'point',value=TRUE)
  } else if(example==42) {
    updateTextInput(session,"title",value="서울시 교통 돌발상황위치별 버블챠트")
    
    updateTextInput(session,"preprocessing",value=
"traffic=read.csv('traffic.csv')
traffic1=aggregate(rpt.id~start.pos.x+start.pos.y+info.tit,traffic,length)")
    
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="traffic1")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="start.pos.x")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="start.pos.y")
    updateSelectInput(session,"fillvar",choices=ggchoice,selected="info.tit")
    updateSelectInput(session,"sizevar",choices=ggchoice,selected="rpt.id")
    updateCheckboxInput(session, 'sizearea',value=TRUE)
    updateNumericInput(session,"pointshape",value=21)
    updateNumericInput(session,"pointalpha",value=0.6)
    updateNumericInput(session,"zoom",value=11)
    #updateTextInput(session,"location",value="서울")
    updateTextInput(session,"lon",value="126.977969")
    updateTextInput(session,"lat",value="37.566535")
    updateSelectInput(session,"maptype",selected="roadmap")
    updateSelectInput(session,"legend",selected="right")
    updateCheckboxInput(session, 'qmap',value=TRUE)
    updateCheckboxInput(session, 'point',value=TRUE)
  }else if(example==43) {
    updateTextInput(session,"title",value="서울시 교통 돌발상황 레벨플롯")
    
    updateTextInput(session,"preprocessing",value=
                      "traffic=read.csv('traffic.csv')")
    
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="traffic")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="start.pos.x")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="start.pos.y")
    updateSelectInput(session,"tdfill",selected="..level..")
    updateSelectInput(session,"tdgeom",selected="polygon")
    updateTextInput(session,"tdalpha",value=0.5)
    updateTextInput(session,"tdsize",value=0.5)
    updateTextInput(session,"tdbins",value=4)
    updateNumericInput(session,"zoom",value=11)
    #updateTextInput(session,"location",value="서울")
    updateTextInput(session,"lon",value="126.977969")
    updateTextInput(session,"lat",value="37.566535")
    updateSelectInput(session,"maptype",selected="terrain")
    updateSelectInput(session,"legend",selected="topleft")
    updateCheckboxInput(session, 'qmap',value=TRUE)
    updateCheckboxInput(session, 'statdensity2d',value=TRUE)
  }else if(example==44) {
    updateTextInput(session,"title",value="서울시 강동구 공영주차장 위치")
    
    updateTextInput(session,"preprocessing",value=
                      "dat=read.csv('data/서울_강동구_공영주차장_위경도.csv',fileEncoding='euc-kr')")
    
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="dat")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="LON")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="LAT")
    updateNumericInput(session,"pointsize",value=3)
    updateNumericInput(session,"pointalpha",value=0.6)
    updateSelectInput(session,"pointcolor",selected="red")
    updateTextInput(session,"textlabel",value="주차장명")
    updateNumericInput(session,"textsize",value=3)
    updateTextInput(session,"texty",value="LAT+0.001")
    updateTextInput(session,"textvjust",value="0")
    updateNumericInput(session,"zoom",value=14)
    #updateTextInput(session,"location",value="명일동")
    updateTextInput(session,"lon",value="127.150040")
    updateTextInput(session,"lat",value="37.5502209")
    updateSelectInput(session,"maptype",selected="roadmap")
    updateCheckboxInput(session, 'qmap',value=TRUE)
    updateCheckboxInput(session, 'point',value=TRUE)
    updateCheckboxInput(session, 'text',value=TRUE)
  }else if(example==45) {
    updateTextInput(session,"title",value="제주도 여행코스(1일차)")
    
    updateTextInput(session,"preprocessing",value=
                      "dat=read.csv('data/제주도여행코스_1일차.csv',fileEncoding='euc-kr')")
    
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="dat")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="LON")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="LAT")
    updateNumericInput(session,"pointsize",value=3)
    updateNumericInput(session,"pointalpha",value=0.6)
    updateSelectInput(session,"pointcolor",selected="red")
    updateTextInput(session,"textlabel",value="장소")
    updateNumericInput(session,"textsize",value=3)
    updateTextInput(session,"texty",value="LAT+0.001")
    updateTextInput(session,"textvjust",value="0")
    updateSelectInput(session,"pathcolor",selected="blue")
    updateSelectInput(session,"pathlinetype",selected=2)
    updateNumericInput(session,"zoom",value=11)
    updateTextInput(session,"location",value="")
    updateTextInput(session,"lon",value="126.4")
    updateTextInput(session,"lat",value="33.3616666")
    updateSelectInput(session,"maptype",selected="roadmap")
    updateCheckboxInput(session, 'qmap',value=TRUE)
    updateCheckboxInput(session, 'point',value=TRUE)
    updateCheckboxInput(session, 'text',value=TRUE)
    updateCheckboxInput(session, 'path',value=TRUE)
  }else if(example==46){
    
    updateTextInput(session,"preprocessing",value=
                      "require(gridExtra)
top10=head(mtcars[order(mtcars$mpg,decreasing=TRUE),c('mpg','wt')],10) 
table_grob=tableGrob(top10)")
    
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="mtcars")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="wt")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="mpg")
    updateTextInput(session,"grob",value="table_grob")
    updateTextInput(session,"acxmin",value="6")
    updateTextInput(session,"annlabel",value="Top 10 mpg cars")
    updateTextInput(session,"annx",value="8.2")
    updateTextInput(session,"anny",value="32.5")
    updateTextInput(session,"xlim",value=",10")
    updateCheckboxInput(session,"point",value=TRUE)
    updateCheckboxInput(session,"anncustom",value=TRUE)
    updateCheckboxInput(session,"annotate",value=TRUE)
    
  }else if(example==47){
    
    updateTextInput(session,"preprocessing",value=
                      "res=lm2table('mtcars','wt','mpg')
table_grob=tableGrob(res)")
    
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="mtcars")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="wt")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="mpg")
    updateTextInput(session,"grob",value="table_grob")
    updateTextInput(session,"acymax",value="10")
    updateTextInput(session,"acxmax",value="5")
    updateTextInput(session,"ylim",value="0")
    updateSelectInput(session,"smoothmethod",selected="lm")
    updateTextInput(session,"annx",value="4")
    updateTextInput(session,"anny",value="30")
    updateNumericInput(session,"annsize",value=7)
    updateTextInput(session,"annlabel",value="y=-5.3x + 37.3(p<0.001)")
    updateSelectInput(session,"annfamily",selected="Times")
    updateSelectInput(session,"annfontface",selected="italic")
    updateCheckboxInput(session,"point",value=TRUE)
    updateCheckboxInput(session,"anncustom",value=TRUE)
    updateCheckboxInput(session,"smooth",value=TRUE)
    updateCheckboxInput(session,"annotate",value=TRUE)
    
    
  } else if(example==48){
    
    updateTextInput(session,"preprocessing",
                    value="grid <- expand.grid(
x = seq(-pi, pi, length = 50),
y = seq(-pi, pi, length = 50)
) %>% mutate(r = x ^ 2 + y ^ 2, z = cos(r ^ 2) * exp(-r / 6))
Center='Center'")
    
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="grid")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="x")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="y")
    updateSelectInput(session,"fillvar",choices=ggchoice,selected="z")
    updateCheckboxInput(session,"text",value = TRUE)
    updateCheckboxInput(session,"tile",value = TRUE)
    updateCheckboxInput(session,"raster",value = TRUE)
    updateTextInput(session,"textdata",value="data.frame(x=0,y=0)")
    updateTextInput(session,"textx",value="x")
    updateTextInput(session,"texty",value="y")
    updateTextInput(session,"textlabel",value="Center")
    updateCheckboxInput(session,"legendposition",value = "none")
    updateCheckboxInput(session,"geom_label",value = TRUE)
    updateCheckboxInput(session,"inherit.aes",value = FALSE)
  } else if(example==49){  # 면분할 예제*
    
    updateTextInput(session,"preprocessing",
                    value="data <- mtcars %>% 
  mutate(
                    Logarithmic = log(mpg),
                    Inverse = 1 / mpg,
                    Cubic = mpg ^ 3,
                    Original = mpg
    ) %>% tidyr::gather(transformation, mpg2, Logarithmic:Original)")
    
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="data")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="mpg2")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="disp")
    updateCheckboxInput(session,"point",value = TRUE)
    updateSelectInput(session,"facetwrap",choices=ggchoice,selected="transformation")
    updateSelectInput(session,"facetswitch",selected="x")
    updateSelectInput(session,"facetscales",selected="free")
    updateCheckboxInput(session,"strip.bg.blank",value = TRUE)
  } else if(example==50){  # labeller 예제*
    
    updateTextInput(session,"preprocessing",
                    value="my_labeller <- label_bquote(
rows = .(am) / alpha,
cols = .(vs) ^ .(cyl)
)")
    
    updateCheckboxInput(session, 'doPreprocessing',value=TRUE)
    updateTextInput(session,"mydata",value="mtcars")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="wt")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="mpg")
    updateCheckboxInput(session,"point",value = TRUE)
    updateTextInput(session,"rordertext",value="facet_grid(am ~ vs + cyl, labeller = my_labeller)")  
    updateCheckboxInput(session,"addrorder",value = TRUE)

  } else if(example==51){  # 산점도에 라벨 붙이기*
    
    updateTextInput(session,"mydata",value="mtcars")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="wt")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="mpg")
    updateCheckboxInput(session,"point",value = TRUE)
    updateCheckboxInput(session,"text",value = TRUE)
    updateCheckboxInput(session,"geom_label",value = TRUE)
    
    updateTextInput(session,"textlabel",value ="rownames(mtcars)")
    updateTextInput(session,"textnudge_y",value ="0.7")
    
  } else if(example==52){  # 산점도에 라벨 붙이기(중복체크)*
    
    updateTextInput(session,"mydata",value="mtcars")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="wt")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="mpg")
    updateCheckboxInput(session,"point",value = TRUE)
    updateCheckboxInput(session,"text",value = TRUE)
    updateCheckboxInput(session,"geom_label",value = FALSE)
    updateCheckboxInput(session,"check_overlap",value = TRUE)
    updateTextInput(session,"textlabel",value ="rownames(mtcars)")
    updateTextInput(session,"textnudge_y",value ="0.7")
    
  }else if(example==53){  # geom_count()*
    
    updateTextInput(session,"mydata",value="mpg")  
    mydf=df()
    ggchoice=c("None",colnames(mydf))
    updateSelectInput(session,"xvar",choices=ggchoice,selected="cty")
    updateSelectInput(session,"yvar",choices=ggchoice,selected="hwy")
    updateCheckboxInput(session,"count",value = TRUE)
    
  }
  
  
  


  
  
  

  #updateSelectInput(session,"example",selected=0)
}

## example and multiplotsample 
observe({

  doExample(input$example)
  
})
  

 
  resetPlot=function(){
    
    #putmsg("resetPlot")
    mylist=c("point","count","text","label","segment","curve","jitter","line","histogram","boxplot","bar","polygon",
            "tile","raster","path","area","density","smooth","violin","dotplot","rug","errorbar","statsummary",
             "statdensity2d","addrorder","doPreprocessing","useRdata",
            "coord_polar","coordmap","coordfixed","coordflip",
            "palettecolor","cpalettec","legendreverse","remove.x.text","remove.y.text",
            "remove.tick","axis.blank",
            "log.x","log.y","strip.bg.blank",
            "yreverse","xreverse","annparse","map","qmap","annotate","anncustom")
    for(i in 1:length(mylist)) updateCheckboxInput(session,mylist[i],value=FALSE)
    updateCheckboxInput(session,"addrorder",value=FALSE)
    updateCheckboxInput(session,"reorderx",value=FALSE)
    updateCheckboxInput(session,"reordery",value=FALSE)
    updateTextInput(session,"rordertext",value="")
    updateTextInput(session,"cpalette",value="")
    updateTextInput(session,"ymin",value="")
    updateTextInput(session,"ymax",value="")
    updateTextInput(session,"mapmap",value="")
    updateTextInput(session,"mapid",value="")
    updateTextInput(session,"ylim",value="")
    updateTextInput(session,"xlim",value="")
    updateTextInput(session,"location",value="")
    updateTextInput(session,"lon",value="")
    updateTextInput(session,"lat",value="")
    updateNumericInput(session,"zoom",value=10)
    updateSelectInput(session,"maptype",selected="terrain")
    updateSelectInput(session,"source",selected="google")
    updateSelectInput(session,"extent",selected="device")
    updateSelectInput(session,"legend",selected="right")
    myselectlist=c("xvar","yvar","contourzvar","groupvar","colorvar","fillvar","sizevar","shapevar","alphavar",
                   "linetypevar","palette","facetrow","facetcol","facetwrap","facetswitch","ggthemes")
    for (i in 1:length(myselectlist)) updateSelectInput(session,myselectlist[i],selected="None")

    myNAlist=c("pointcolor","pointfill","jittercolor","jitterfill",
               "segmentcolor","linecolor","contourcolor",
               "barcolor","barfill","areacolor","areafill","hiscolor","hisfill",
               "densityfill","boxfill","boxcolor","polygonfill","polygoncolor",
               "violinfill","violincolor","pathcolor","tilecolor","tilefill",
               "violinscale","dotcolor","dotfill","sumfill","sumcolor","ebcolor",
               "anncolor","annfill","tdcolor","tdfill","smoothcolor","mapfill","mapcolor")
    for (i in 1:length(myNAlist)) updateSelectInput(session,myNAlist[i],selected="NA")
    
    updateSelectInput(session,"linetype",selected=-1)
    updateSelectInput(session,"smoothlinetype",selected=-1)
    
    myidentitylist=c("pointposition","linestat","lineposition","densityposition",
                     "areastat")
    for (i in 1:length(myidentitylist)) 
      updateSelectInput(session,myidentitylist[i],selected="identity")
    
    updateSelectInput(session,'labeller',selected="label_value")
    updateSelectInput(session,'densitycolor',selected="None")
    updateSelectInput(session,'barstat',selected="bin")
    updateSelectInput(session,'barposition',selected="stack")
    updateSelectInput(session,'hispos',selected="stack")
    updateSelectInput(session,'smoothmethod',selected="loess")
    updateSelectInput(session,'smoothformula',selected="y~x")
    updateTextInput(session,'smoothformula2',value="")
    updateSelectInput(session,'boxoutcolor',selected="None")
    updateSelectInput(session,'dotmethod',selected="dotdensity")
    updateSelectInput(session,'dotstackdir',selected="up")
    updateSelectInput(session,'dotbinpositions',selected="bygroup")
    updateSelectInput(session,'dotbinaxis',selected="x")
    updateSelectInput(session,'sumgeom',selected="point")
    updateSelectInput(session,'sumfuny',selected="mean")
    updateSelectInput(session,'legendposition',selected="right")
    updateSelectInput(session,"fonts",selected="NA")
    updateSelectInput(session,"facetwrapscale",selected="fixed")
    updateSelectInput(session,"theme",selected="grey")
    updateTextInput(session,"segmentxend",value="0")
    updateTextInput(session,"segmentyend",value="0")
    updateSelectInput(session,"segmentlineend",selected="butt")
    updateSelectInput(session,"facetscales",selected="fixed")
    updateSelectInput(session,"facetspace",selected="fixed")
    
    myFALSElist=c("factorxvar","sizearea","psize","pshape","boxnotch",
                  "legendreverse","coordflip","coord_polar","coordfixed","coordmap","palettec","cpalettec","xreverse","yreverse",
                  "remove.x.text","remove.y.text","remove.tick")
    for (i in 1:length(myFALSElist)) updateCheckboxInput(session,myFALSElist[i],value=FALSE)
    
    myTRUElist=c("se","violintrim","tdcontour","annlabeltext","inherit.aes")
    for (i in 1:length(myTRUElist)) updateCheckboxInput(session,myTRUElist[i],value=TRUE)
    
    myOnelist=c("pointalpha","segmentsize","baralpha","areaalpha","hisalpha",
                "densityalpha","boxwidth","boxalpha","violinadjust","violinalpha","tilealpha","dotalpha",
                "dotstackratio","lineadjust","densityadjust","jitteralpha",
                "polygonalpha","pathsize","pathalpha","annalpha","tdalpha","direction","mapalpha")
    for (i in 1:length(myOnelist)) updateNumericInput(session,myOnelist[i],value=1)
    
    myHalflist=c("polygonsize","areasize","densitysize","histsize","linesize","ebsize",
                 "smoothsize","contoursize","mapsize")
    for (i in 1:length(myHalflist)) updateNumericInput(session,myHalflist[i],value=0.5)  
      
    updateNumericInput(session,"tilesize",value=0.1)
    updateNumericInput(session,"maxsize",value=15)
    updateNumericInput(session,"pointsize",value=2)
    updateNumericInput(session,"pointshape",value=19)
    updateNumericInput(session,"jittersize",value=2)
    updateNumericInput(session,"jittershape",value=16)
    updateNumericInput(session,"jitterwidth",value=0.4)
    updateNumericInput(session,"jitterheight",value=0)
    updateNumericInput(session,"textsize",value=5)
    updateNumericInput(session,"barwidth",value=0.9)
 
    updateNumericInput(session,"hisbinwidth",value=0)
    updateNumericInput(session,"level",value=0.95)
    updateNumericInput(session,"ebpos",value=0)
    updateNumericInput(session,"ebwidth",value=0.2)
    updateNumericInput(session,"boxoutsize",value=2)
    updateNumericInput(session,"boxoutshape",value=16)
    updateNumericInput(session,"dotbinwidth",value=0)
    updateNumericInput(session,"sumsize",value=2)
    updateNumericInput(session,"sumshape",value=21)
    updateNumericInput(session,"facetwrapncol",value=0)
    updateNumericInput(session,"contourbinwidth",value=0)
    updateNumericInput(session,"annsize",value=5)
    
    mytextlist=c("title","xlab","ylab","textdata","textlabel","textx","texty",
                 "textvjust","texthjust","annx","annxend","anny","annyend",
                 "annlabel","annarrow","annhjust","annvjust","grob","tdh","smoothfamily")
    for(i in 1:length(mytextlist)) updateTextInput(session,mytextlist[i],value="")
    
    updateSelectInput(session,"palette",choices=c("None",rownames(brewer.pal.info)),selected="None")
    #updateSelectInput(session,"fonts",selected="NA")
    updateSelectInput(session,"annfamily",selected="NA")
    updateSelectInput(session,"annfontface",selected="plain")
    updateSelectInput(session,"anngeom",selected="text")
    updateSelectInput(session,"textstat",selected="identity")
    updateSelectInput(session,"contourgeom",selected="path")
    updateSelectInput(session,"tdgeom",selected="density2d")
    updateTextInput(session,"acxmin",value="-Inf")
    updateTextInput(session,"acxmax",value="Inf")
    updateTextInput(session,"acymin",value="-Inf")
    updateTextInput(session,"acymax",value="Inf")
    updateTextInput(session,"start",value="0")
    updateTextInput(session,"coordratio",value="1")
    updateTextInput(session,"projection",value="mercator")
    updateTextInput(session,"textnudge_x",value="0")
    updateTextInput(session,"textnudge_y",value="0")
    updateSelectInput(session,"theta",selected="x")
    updateNumericInput(session,"plotWidth",value=7)
    updateNumericInput(session,"plotHeight",value=5)
    updateNumericInput(session,"plotRes",value=300)
    updateSelectInput(session,"plotUnit",selected="in")
    
    updateTextInput(session,"rorder",value="")
    
    
  }
 
  resetMultiplot=function(){
     mylist=paste("plotinset",1:4,sep="")
     for(i in 1:length(mylist)) updateTextInput(session,mylist[i],value="")
     updateCheckboxInput(session,'doMultiplot',value=FALSE)
     updateTextInput(session,'label1',value="A")
     updateTextInput(session,'label2',value="B")
     updateTextInput(session,'label3',value="C")
     updateTextInput(session,'label4',value="D")
     updateSelectInput(session,'inset1choice',selected=3)
     updateSelectInput(session,'inset2choice',selected=1)
     updateSelectInput(session,'inset3choice',selected=4)
     updateSelectInput(session,'inset4choice',selected=2)
     updateCheckboxInput(session,"transparent1",value=FALSE)
  }
  
  varchoice=reactive({
    result=0
    if(!(input$xvar %in% c("","None"))) result=result+1
    if(!(input$yvar %in% c("","None"))) result=result+2
    if(input$map & (input$mapid!="") & (input$mapmap!="")) result=-1
    if(input$qmap &(result==0)) result=-1
    result
  })

  ggplotchoice=reactive({
      mylist=c("point","count","segment","text","jitter","line","histogram","boxplot","bar","polygon","path","tile",
               "area","density","smooth","violin","dotplot","rug","statsummary","statdensity2d",
               "statcontour","addrorder","map","qmap")
      sumcheck=0
      for(i in 1:length(mylist)) {
          temp=eval(parse(text=paste("input$",mylist[i])))
          if(temp==TRUE) sumcheck=sumcheck+1
      }
      sumcheck
  })
  

  output$plot.ui <- renderUI({
    #if(varchoice()*ggplotchoice()!=0){
    if(varchoice()!=0){
      #mysize=ifelse(ggplotchoice()%%2==1,370,0)
      #plotOutput("plot",width=600,428)
      #make_ggplot()
      plotOutput("myImage2",width=600,fheight2())
    }
  })
  
  
  #output$plot2 <- renderUI({
  #  mywidth=input$width
  #  myheight=input$height
  #  plotOutput("plot3", width = paste0(mywidth,"px"), height = paste0(myheight,"px"))
  #  
  #})
  

output$plot <- renderPlot({
  

      #if(varchoice()*ggplotchoice()!=0) {
      if(varchoice()!=0) {
           # Generate the PNG
        try(p<-make_ggplot())
        
        if(!is.null(p)) print(p)
        
      }  
  })

   
  observe ({
    
    choice=input$MultiplotExample
    
    if(choice==0) resetMultiplot()
    else{
    p1="ggplot(data.frame(x=c(0,2*pi)),aes(x=x))+
      stat_function(fun=sin,colour='darkgreen',size=2)+theme_grey()"
    
    p2="ggplot(data.frame(x=c(0,2*pi)),aes(x=x))+
      stat_function(fun=function(x){sin(x)^2},geom='area',colour='orange',fill='yellow',size=2)+
      theme_bw()+ylab(expression(sin(x)^2))"
    
    p3<-"ggplot(data.frame(x=c(0,2*pi)),aes(x=x))+
      stat_function(fun=function(x){sin(x)^3},colour='blue',size=2)+
      theme_bw()+ylab(expression(sin(x)^3))"
    
    updateTextInput(session,'plotinset1',value=p1)
    updateTextInput(session,'plotinset2',value=p2)
    updateTextInput(session,'plotinset3',value=p3)
    updateTextInput(session,'plotinset4',value="")
    updateCheckboxInput(session,"transparent1",value=FALSE)
    if(choice==1){
      updateSelectInput(session,'inset1choice',selected=6)
      updateSelectInput(session,'inset2choice',selected=1)
      updateSelectInput(session,'inset3choice',selected=2)
      updateTextInput(session,'label1',value="A")
      updateTextInput(session,'label2',value="B")
      updateTextInput(session,'label3',value="C")
      
    } else if (choice==2){
      updateSelectInput(session,'inset1choice',selected=7)
      updateSelectInput(session,'inset2choice',selected=4)
      updateSelectInput(session,'inset3choice',selected=2)
      updateTextInput(session,'label1',value="A")
      updateTextInput(session,'label2',value="B")
      updateTextInput(session,'label3',value="C")
    }
    else if(choice==3){
      updateSelectInput(session,'inset1choice',selected=0)
      updateSelectInput(session,'inset2choice',selected=11)
      updateSelectInput(session,'inset3choice',selected=14)
      updateTextInput(session,'label1',value="")
      updateTextInput(session,'label2',value="A")
      updateTextInput(session,'label3',value="B")
      updateCheckboxInput(session,"transparent1",value=FALSE)
      updateCheckboxInput(session,"transparent2",value=TRUE)
      updateCheckboxInput(session,"transparent3",value=TRUE)
      
    }else if(choice==4){
      
      p4="ggplot(data.frame(x=c(0,2*pi)),aes(x=x))+
      stat_function(fun=function(x){cos(x)^2},geom='area',colour='orange',fill='yellow',size=2)+
      theme_bw()+ylab(expression(cos(x)^2))"
      
      
      updateTextInput(session,'plotinset4',value=p4) 
      updateSelectInput(session,'inset1choice',selected=3)
      updateSelectInput(session,'inset2choice',selected=1)
      updateSelectInput(session,'inset3choice',selected=4)
      updateSelectInput(session,'inset4choice',selected=2)
      updateTextInput(session,'label1',value="A")
      updateTextInput(session,'label2',value="B")
      updateTextInput(session,'label3',value="C")
      updateTextInput(session,'label4',value="D")
      
      updateCheckboxInput(session,"transparent1",value=TRUE)
      
    }
    updateCheckboxInput(session,"doMultiplot",value=TRUE)
    }
  })  
  
  observeEvent(input$barlabel,{
          ## histogram
     if((input$yvar=="None") & (input$barstat=="bin") &(input$fillvar %in% c("None","..count..",input$xvar))){
         updateSelectInput(session,'textposition',selected="identity")
         updateSelectInput(session,'textstat',selected="count")
         updateTextInput(session,'textdodgeposition',value="")
         updateTextInput(session,'textlabel',value="..count..")
         updateTextInput(session,'textvjust',value=-0.2)
         updateCheckboxInput(session,'text',value=TRUE)
         
     } else if((input$yvar!="None") & (input$barstat=="identity") &(input$fillvar %in% c("None","..count..",input$xvar))){
             updateSelectInput(session,'textposition',selected="identity")
             updateSelectInput(session,'textstat',selected="identity")
             updateTextInput(session,'textdodgeposition',value="")
             updateTextInput(session,'textlabel',value=input$yvar)
             updateTextInput(session,'textvjust',value=-0.2)
             updateCheckboxInput(session,'text',value=TRUE)
             
     } else if(input$barposition=="fill"){
             if(input$barstat=="bin"){
                     updateTextInput(session,'preprocessing',
                             value=paste0("res=melt(apply(table(",input$mydata,"$",input$fillvar,",",input$mydata,"$",input$xvar,"),2,function(x) x/sum(x)))\n",
                                          "colnames(res)=c('",input$fillvar,"','",input$xvar,"','ratio')"))
             } else{
                     updateTextInput(session,'preprocessing',
                                     value=paste0("res=ddply(",input$mydata,",.(",input$xvar,"),transform,ratio=",input$yvar,"/sum(",input$yvar,"))"))
                                          
             }
             updateCheckboxInput(session,'doPreprocessing',value=TRUE)
             updateSelectInput(session,"textstat",selected="identity")
             updateTextInput(session,'textdata',value="res")
             updateTextInput(session,'textlabel',value="percent(ratio)")
             updateTextInput(session,'texty',value="ratio")
             updateSelectInput(session,'textposition',selected="fill")
             updateTextInput(session,'textdodgeposition',value="")
             updateTextInput(session,'textvjust',value="")
             updateCheckboxInput(session,'text',value=TRUE)
             
     } else if((input$barstat=="bin")){
             updateSelectInput(session,'textstat',selected="count")
             updateTextInput(session,'textlabel',value="..count..")
             updateTextInput(session,'textdata',value="")
             updateTextInput(session,'texty',value="")
             updateSelectInput(session,'textposition',selected=input$barposition)
             if(input$barposition=="stack"){
                     updateTextInput(session,'textdodgeposition',value="")
                     updateTextInput(session,'textvjust',value="")
                    
             } else if(input$barposition=="dodge"){
                     updateTextInput(session,'textdodgeposition',value="0.9")
                     updateTextInput(session,'textvjust',value="-0.2")
                    
             } 
             #else if(input$barposition=="fill"){
             #         updateTextInput(session,'textdodgeposition',value="")
             #         updateTextInput(session,'textlabel',value="scales::percent(..count../sum(..count..))")
             #         updateTextInput(session,'textvjust',value="")
             # }
             updateCheckboxInput(session,'text',value=TRUE)
     } else if(input$barstat=="identity"){
             updateSelectInput(session,'textstat',selected="identity")
             updateTextInput(session,'textlabel',value=input$yvar)
             updateSelectInput(session,'textposition',selected=input$barposition)
             updateTextInput(session,'textdata',value="")
             updateTextInput(session,'texty',value="")
             if(input$barposition=="stack"){
                     updateTextInput(session,'textdodgeposition',value="")
                     updateTextInput(session,'textvjust',value="")
             } else if(input$barposition=="dodge"){
                     updateTextInput(session,'textdodgeposition',value="0.9")
                     updateTextInput(session,'textvjust',value="-0.2")
             } 
             # else if(input$barposition=="fill"){
             #         updateTextInput(session,'textdodgeposition',value="")
             #         updateTextInput(session,'textlabel',value=paste0("scales::percent(",input$yvar,"/sum(",input$yvar,"))"))
             #         updateTextInput(session,'textvjust',value="")
             # }
             updateCheckboxInput(session,'text',value=TRUE)
     }
         
     updateTextInput(session,'textother',value=input$barother)
  })  
  
  observeEvent(input$ebauto,{
    
      if(input$xvar=="None") {
        putmsg("먼저 x축변수를 선택해주세요")  
        updateCheckboxInput(session,"ebauto",value=FALSE)
      } else if (input$yvar=="None") {
        putmsg("먼저 y축변수를 선택해주세요")  
        updateCheckboxInput(session,"ebauto",value=FALSE)
      } else if((input$groupvar=="None") &(input$fillvar=="None")) {
        putmsg("먼저 group 변수 또는 fill 변수를 선택해주세요")  
        updateCheckboxInput(session,"ebauto",value=FALSE)
      } else {
        mygroupvar=paste0("'",input$xvar,"'")
        if(!(input$groupvar %in% c("None","1"))) mygroupvar=mypaste(mygroupvar,"'",input$groupvar,"'")
        if(input$fillvar!="None") mygroupvar=mypaste(mygroupvar,"'",input$fillvar,"'")
        myorder=paste("df=summarySE(",input$mydata,",'",input$yvar,
                      "',c(",mygroupvar,"),conf.interval=",input$ebCI,")",sep="")
        updateTextInput(session,"preprocessing",value=myorder)
        updateCheckboxInput(session,"doPreprocessing",value=TRUE)
        updateTextInput(session,"mydata",value="df")
        updateTextInput(session,"ymin",value=paste0(input$yvar,"-se"))
        updateTextInput(session,"ymax",value=paste0(input$yvar,"+se"))
        updateCheckboxInput(session,"ebauto",value=FALSE)
    
        if(input$bar & (input$barposition=="dodge")) {
          updateNumericInput(session,"ebpos",value = 0.9)
        } else if(input$point) {
          updateNumericInput(session,"ebpos",value=0.3)
        }
      }  
    
  })
  
  observeEvent(input$saveToMultiplot,  {
    
    if(input$plotinset1=="") updateTextInput(session,'plotinset1',value=input$rorder)
    else if(input$plotinset2=="") updateTextInput(session,'plotinset2',value=input$rorder)
    else if(input$plotinset3=="") updateTextInput(session,'plotinset3',value=input$rorder)
    else if(input$plotinset4=="") updateTextInput(session,'plotinset4',value=input$rorder)
    updateCheckboxInput(session,"doMultiplot",value=TRUE)
    
  })
  
  observeEvent(input$saveToPPTList,  {
    
    mylist=paste0("ppt",1:10)
    
    for(i in 1:10) {
        if(input[[mylist[i]]]==""){
            updateTextInput(session,mylist[i],value=input$rorder)
            break
        }
    }
    
  })
  
  # observeEvent(input$savePPTList,  {
  #   
  #   if(input$plotinset1=="") updateTextInput(session,'plotinset1',value=input$rorder)
  #   else if(input$plotinset2=="") updateTextInput(session,'plotinset2',value=input$rorder)
  #   else if(input$plotinset3=="") updateTextInput(session,'plotinset3',value=input$rorder)
  #   else if(input$plotinset4=="") updateTextInput(session,'plotinset4',value=input$rorder)
  #   updateCheckboxInput(session,"doMultiplot",value=TRUE)
  #   
  # })  

  observeEvent(input$resetVar,{
      resetPlot()
      updateSelectInput(session,'example',selected=0)
      #updateTextInput(session,'preprocessing',value="")
      updateCheckboxInput(session,"doPreprocessing",value=FALSE)
      
  })

  
  output$ggplotText=renderPrint({
    #mysize=ifelse(ggplotchoice()%%2==1,'100%','0px')
    #str(mysize)
    #mysize
    if(input$showsummary){
        try(p<-eval(parse(text=paste(input$rorder))))
        try(summary(p))
    }
  })
  
  output$plot.ui2 <- renderUI({
    mywidth=input$mpwidth
    myheight=input$mpheight
    plotOutput("plotMultiplot", width = paste0(mywidth,"px"), height = paste0(myheight,"px"))
    
  })

  output$plotMultiplot=renderPlot({
      if(input$doMultiplot){
         p<-makeMultiplot()
      }
  })


output$myImage <- renderImage({
  
  if(varchoice()!=0) {
  # A temp file to save the output.
  # This file will be removed later by renderImage
  outfile <- tempfile(fileext='.png')
  
  # Generate the PNG
  #eval(parse(text=input$preprocessing))
  #try(p<-eval(parse(text=paste(input$rorder))))
  p<-make_ggplot()
  if(!is.null(p)) ggsave(outfile,p,width=input$plotWidth,height=input$plotHeight,
         units=input$plotUnit,dpi=input$plotRes)
  
  # Return a list containing the filename
  list(src = outfile,
       contentType = 'image/png',
       width = 900,
       height = fheight(),
       alt = "This is alternate text")
  } else{
    outfile="book.jpg"
    list(src = outfile,
         #width = 400,
         #height = 300,
         contentType = 'image/jpg',
         alt = "This is alternate text") 
  }  
}, deleteFile = ifelse(varchoice()!=0,TRUE,FALSE))


output$myImage2<- renderPlot({
  
  if(varchoice()!=0) {
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.png')
    
    p<-make_ggplot()
    p
  }  
})


  
output$myInsetImage <- renderImage({
    
    if(input$doMultiplot){
      
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.png')
      
      # Generate the PNG
      #p<-makeInsetPlot()
      
      #if(!is.null(p)) ggsave(outfile,p,width=input$plotWidth2,height=input$plotHeight2,
            #                units=input$plotUnit2,dpi=input$plotRes2)
      
      plotPNG(makeInsetPlot,outfile,width=input$plotWidth2,height=input$plotHeight2,units=input$plotUnit2,
              res=input$plotRes2)
      
      # Return a list containing the filename
      
      list(src = outfile,
           contentType = 'image/png',
           width = 900,
           height = fheight3(),
           alt = "This is alternate text")
      
    } else{
      outfile="book.jpg"
      list(src = outfile,
           #width = 400,
           #height = 300,
           contentType = 'image/jpg',
           alt = "This is alternate text") 
    }  
  }, deleteFile = ifelse(input$doMultiplot,TRUE,FALSE))

  
  transparent=function(){
     
         
         temp=theme(rect= element_rect(fill = 'transparent',size=0),
                    panel.background=element_rect(fill = 'transparent'),
                    panel.border=element_rect(size=0.5),
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank())
         temp
  }
  
  
  makeInsetPlot=function(){
    
    tjust=0.02
    fsize=20
    if(input$plotinset1!=""){
         vp1<-eval(parse(text=paste('viewport(',input$inset1vp,')')))
         p1=eval(parse(text=paste(input$plotinset1)))
         if(input$transparent1) p1<-p1+transparent()
         print(p1,vp=vp1)
         if(input$label1!="") 
           grid.text(input$label1,x=as.numeric(vp1$x)-as.numeric(vp1$width)/2+tjust,
                     y=(as.numeric(vp1$y)+as.numeric(vp1$height)/2)-tjust,
                     just=c("left","top"),gp=gpar(fontsize=fsize))
    }   
   
    if(input$plotinset2!="") {
      vp2=eval(parse(text=paste('viewport(',input$inset2vp,")")))
      p2=eval(parse(text=paste(input$plotinset2)))
      if(input$transparent2) p2<-p2+transparent()
      print(p2,vp=vp2)
      if(input$label2!="") 
        grid.text(input$label2,x=as.numeric(vp2$x)-as.numeric(vp2$width)/2+tjust,
                  y=as.numeric(vp2$y)+as.numeric(vp2$height)/2-tjust,
                  just=c("left","top"),gp=gpar(fontsize=fsize))
      
    }     
      
    if(input$plotinset3!="") {
      vp3=eval(parse(text=paste('viewport(',input$inset3vp,")")))
      p3=eval(parse(text=paste(input$plotinset3)))
      if(input$transparent3) p3<-p3+transparent()
      print(p3,vp=vp3)
      if(input$label3!="") 
        grid.text(input$label3,x=as.numeric(vp3$x)-as.numeric(vp3$width)/2+tjust,
                  y=as.numeric(vp3$y)+as.numeric(vp3$height)/2-tjust,
                  just=c("left","top"),gp=gpar(fontsize=fsize))
    }   
      
    if(input$plotinset4!="") {
      vp4=eval(parse(text=paste('viewport(',input$inset4vp,")")))
      p4=eval(parse(text=paste(input$plotinset4)))
      if(input$transparent4) p4<-p4+transparent()
      print(p4,vp=vp4)
      if(input$label4!="") 
        grid.text(input$label4,x=as.numeric(vp4$x)-as.numeric(vp4$width)/2+tjust,
                  y=as.numeric(vp4$y)+as.numeric(vp4$height)/2-tjust,
                  just=c("left","top"),gp=gpar(fontsize=fsize))
    }   

  }
  
  
  output$downloadPlot = downloadHandler(
    filename="Plot.png",
    content=function(file){
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      p<-make_ggplot()
      ggsave(file,p,width=input$plotWidth,height=input$plotHeight,units=input$plotUnit,dpi=input$plotRes)
      #plotPNG(plotlogPlot,filename=file,width=input$plotWidth,height=input$plotHeight,units=input$plotUnit,
      #        res=input$plotRes)
      #try(eval(parse(text=input$preprocessing)))
      #try(p<-eval(parse(text=paste(input$rorder))))
      #if(!is.null(p)) ggsave(file,p,width=input$plotWidth,height=input$plotHeight,
                             #units=input$plotUnit,dpi=input$plotRes)
      
      
    },
    contentType="image/png"
  )
output$downloadPDF = downloadHandler(
  filename="myplot.pdf",
  content=function(file){
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    p<-make_ggplot()
    ggsave(file,p,width=input$plotWidth,device=cairo_pdf,height=input$plotHeight,units=input$plotUnit,dpi=input$plotRes)
    #plotPNG(plotlogPlot,filename=file,width=input$plotWidth,height=input$plotHeight,units=input$plotUnit,
    #        res=input$plotRes)
    #embed_fonts(file)
    #eval(parse(text=input$preprocessing))
    #try(p<-eval(parse(text=paste(input$rorder))))
    #if(!is.null(p)) ggsave(file,p,width=input$plotWidth,height=input$plotHeight,
    #                       units=input$plotUnit,dpi=input$plotRes)
    
  },
  contentType="application/pdf"
)

addplot=function(mydoc,plotfunction,title=""){
  mydoc=addSlide(mydoc,"Title and Content")
  mydoc=addTitle(mydoc,title)
  mydoc=addPlot(mydoc,plotfunction,vector.graphic=input$vector)
  mydoc
}

printggplot=function(){
  p<-make_ggplot()
  print(p)
}

printmultiplot=function(){
  p<-makeInsetPlot()
  print(p)
}  


add_ggplot2=function(mydoc,code){
   ph_with_vg_at(mydoc,code=print(eval(parse(text=code))),left=1,top=2,width=8,height=5)
}

output$downloadPPT = downloadHandler(
  filename="ggplot.pptx",
  content=function(file){
    
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))

    
    p<-eval(parse(text=input$rorder))
    plot2pptx(p,target=file)
    
  },
  contentType="application/vnd-ms-powerpoint"
)

output$downloadPPT3 = downloadHandler(
  filename="ggplot.pptx",
  content=function(file){
    
    
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    #mydoc=addSlide(mydoc,"Title Slide")
    #mydoc=addTitle(mydoc,"Figure using ggplot2")
    #mydoc=addSubtitle(mydoc,"prepared by web-r.org")
    
    mylist=paste0("ppt",1:10)
    
    mydoc=read_pptx()
    
    no=1
    for(i in 1:10) {
      temp=input[[mylist[i]]]
      if(temp!="") {
          
          p<-eval(parse(text=temp))
          if(no==1) {
            plot2pptx(p,file)
          } else{
            plot2pptx(p,file,append=TRUE)
            no=no+1
          }
          
      }
    }  
    print(mydoc,target=file)
    #zip(zipfile=file,files=c("ggplot.pptx"))
  },
  contentType="application/vnd-ms-powerpoint"
)

output$downloadPPT2 = downloadHandler(
  filename="multiplot.pptx",
  content=function(file){
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    
    
    plotPNG(makeInsetPlot,"multiplot.PNG",width=input$plotWidth2,height=input$plotHeight2,units=input$plotUnit2,res=input$plotRes2)
        
    read_pptx() %>% 
      add_slide(layout = "Title and Content", master = "Office Theme") %>% 
      ph_with_img_at(src ="multiplot.PNG" , left=1,top=2,width=8,height=5) %>% 
      print(target = file)
    
    #zip(zipfile=file,files=c("multiplot.pptx"))
  },
  contentType="application/vnd-ms-powerpoint"
)


output$downloadMultiplot = downloadHandler(
  filename="Multiplot.png",
  content=function(file){
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    #plotPNG(makeMultiplot,file,width=6,height=4,units="in",res=300)
    
    plotPNG(makeInsetPlot,file,width=input$plotWidth2,height=input$plotHeight2,units=input$plotUnit2,
            res=input$plotRes2)
    
  },
  contentType="image/png"
)

output$downloadMultiplotpdf = downloadHandler(
  filename="Multiplot.pdf",
  content=function(file){
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))

    cairo_pdf(file=file,width=input$plotWidth2,height=input$plotHeight2)
    p<-makeInsetPlot()
    print(p)
    dev.off()
    #ggsave(file,p,width=input$plotWidth2,device=cairo_pdf,height=input$plotHeight2,units=input$plotUnit2,dpi=input$plotRes2)
    #plotPNG(plotlogPlot,filename=file,width=input$plotWidth,height=input$plotHeight,units=input$plotUnit,
    #        res=input$plotRes)
    #embed_fonts(file)
    #eval(parse(text=input$preprocessing))
    #try(p<-eval(parse(text=paste(input$rorder))))
    #if(!is.null(p)) ggsave(file,p,width=input$plotWidth,height=input$plotHeight,
    #                       units=input$plotUnit,dpi=input$plotRes)
    
  },
  contentType="application/pdf"
)

isValidColor=function(x) {
  if(x %in% colors()) result=TRUE
  else result=FALSE
  result
}

  make_ggplot=reactive({

      if(varchoice()!=0){
         
        
          myorder=""    
          temp=""
          if(input$doPreprocessing) {
            eval(parse(text=input$preprocessing))
            myorder=paste(input$preprocessing,"\n")
          }
          
          if((input$palette!="None")&(input$palettecont) ){
                  if(input$palettereverse){
                          eval(parse(text=paste0("palcolors=rev(palette2colors('",input$palette,"'))")))
                          myorder=paste0(myorder,"palcolors=rev(palette2colors('",input$palette,"'))\n")
                          
                  } else {
                          eval(parse(text=paste0("palcolors=palette2colors('",input$palette,"')")))
                          myorder=paste0(myorder,"palcolors=palette2colors('",input$palette,"')\n")
                  }
                  
          }
          if(input$xvar!="None") {
              if(input$reorderx & input$yvar!="None") temp=paste("x=reorder(",input$xvar,",",input$yvar,")",sep="")
              else if(input$factorxvar) temp=paste("x=factor(",input$xvar,")",sep="")
              else temp=paste("x=",input$xvar,sep="")
            }
            if(input$yvar!="None") {
              if(input$reordery) temp=mypaste(temp,"y=reorder(",input$yvar,",",input$xvar,")",sep="")
              else temp=mypaste(temp,"y=",input$yvar)
            }  
            if(input$groupvar!="None") temp=mypaste(temp,"group=",input$groupvar)
            if(input$colorvar!="None") temp=mypaste(temp,"colour=",input$colorvar)
            if(input$fillvar!="None") temp=mypaste(temp,"fill=",input$fillvar)
            if(input$sizevar!="None") temp=mypaste(temp,"size=",input$sizevar)
            if(input$shapevar!="None") temp=mypaste(temp,"shape=",input$shapevar)
            if(input$linetypevar!="None") temp=mypaste(temp,"linetype=",input$linetypevar)
            if(input$alphavar!="None") temp=mypaste(temp,"alpha=",input$alphavar)
            
            #eval(parse(text=paste("mydata=",input$mydata,sep="")))
            #p<-eval(parse(text=paste("ggplot(mydata,",temp,"))",sep="")))
            
            if(temp!="") temp=paste0("aes(",temp,")")
            temp1=paste0("data=",input$mydata)
            temp=mypaste(temp1,temp)
            
        myassign=""    
        if((input$qmap) &(input$lon!="")&(input$lat!="")){
          myassign=temp
          temp=""
          
          temp=paste0("location=c(",input$lon,",",input$lat,")")
          if(input$zoom!=10) temp=mypaste(temp,"zoom=",input$zoom)
          if(input$maptype!="terrain") temp=mypaste(temp,"maptype='",input$maptype,"'")
          if(input$source!="google") temp=mypaste(temp,"source='",input$source,"'")
          if(input$extent!="device") temp=mypaste(temp,"extent='",input$extent,"'")
          if(input$legend!="right") temp=mypaste(temp,"legend='",input$legend,"'")
          
           myorder=paste(myorder,"qmap(",temp,")")
           p<-eval(parse(text=myorder))  
           
        } else{
            myorder=paste(myorder,"ggplot(",temp,")",sep="")  
            p<-eval(parse(text=myorder))  
        }
        if(input$sizearea) {
          p<-p+scale_size_area(max_size=input$maxsize)
          myorder=paste(myorder,"+ \nscale_size_area(max_size=",input$maxsize,")",sep="")
        }  
        if(input$histogram) {
            temp=""
            if(input$histpos!='stack') temp=mypaste(temp,"position='",input$histpos,"'")
            if(input$histbinwidth!=0) temp=mypaste(temp,"binwidth=",input$histbinwidth)
            if(input$histcolor!="NA") temp=mypaste(temp,"colour='",input$histcolor,"'")
            if(input$histfill=="..count..") temp=mypaste(temp,"aes(fill=",input$histfill,")")
            else if(input$histfill!="NA") temp=mypaste(temp,"fill='",input$histfill,"'")
            if(input$histsize!=0.5) temp=mypaste(temp,"size=",input$histsize)
            if(input$histalpha!=1) temp=mypaste(temp,"alpha=",input$histalpha)
       
            p<-p+eval(parse(text=paste("geom_histogram(",temp,")",sep="")))    
            myorder=paste(myorder,"+ \n geom_histogram(",temp,")",sep="")
        }
        
        if(input$bar) {
          temp=""
          
          if(input$barstat!="bin") temp=mypaste(temp,"stat='",input$barstat,"'")
          if(input$barposition!="stack") temp=mypaste(temp,"position='",input$barposition,"'")
          if(input$barcolor!="NA") temp=mypaste(temp,"colour='",input$barcolor,"'")
          if(input$barfill!="NA") temp=mypaste(temp,"fill='",input$barfill,"'")
          if(input$barwidth!=0.9) temp=mypaste(temp,"width=",input$barwidth)
          if(input$barsize!=0.5) temp=mypaste(temp,"size=",input$barsize)
          if(input$baralpha!=1) temp=mypaste(temp,"alpha=",input$baralpha)
          if(input$barother!="") temp=mypaste(temp,input$barother)
          p<-p+eval(parse(text=paste("geom_bar(",temp,")"))) 
          myorder=paste(myorder,"+ \n geom_bar(",temp,")",sep="")
 
        }  
        
        if(input$density) {
          temp=""
          if(input$densitycolor!="None") {
             if(input$densitycolor!="NA") temp=mypaste(temp,"colour='",input$densitycolor,"'")
             else temp=mypaste(temp,"colour=NA")
          }  
          if(input$densityfill!="NA") temp=mypaste(temp,"fill='",input$densityfill,"'")
          if(input$densityposition!="identity") temp=mypaste(temp,"position='",input$densityposition,"'")
          if(input$densitysize!=0.5) temp=mypaste(temp,"size=",input$densitysize)
          if(input$densityadjust!=1) temp=mypaste(temp,"adjust=",input$densityadjust)
          if(input$densityalpha!=1) temp=mypaste(temp,"alpha=",input$densityalpha)
          p<-p+eval(parse(text=paste("geom_density(",temp,")"))) 
          myorder=paste(myorder,"+ \n geom_density(",temp,")",sep="")
        }  
        
        if(input$rug) {
            p<-p+geom_rug()
            myorder=paste(myorder,"+ \n geom_rug()",sep="")
        }    
          if(varchoice()==3) {
            if(input$smooth) {
              temp=""
              
              if(input$smoothmethod!="loess") temp=mypaste(temp,"method=",input$smoothmethod)
              if(input$smoothformula2!="") temp=mypaste(temp,"formula=",input$smoothformula2)
              else if(input$smoothformula!="y~x") temp=mypaste(temp,"formula=",input$smoothformula)
              if(input$se==FALSE) temp=mypaste(temp,"se=FALSE")  
              if(input$level!=0.95) temp=mypaste(temp,"level=",input$level)
              if(input$smoothsize!=0.5) temp=mypaste(temp,"size=",input$smoothsize)
              if(input$smoothcolor!="NA") temp=mypaste(temp,"colour='",input$smoothcolor,"'")
              if(input$smoothfamily!="") temp=mypaste(temp,"method.args=list(family='",input$smoothfamily,"')")
              if(input$smoothlinetype!=-1) temp=mypaste(temp,"linetype=",input$smoothlinetype)
              if(input$fullrange) temp=mypaste(temp,"fullrange=TRUE")  
              p<-p+eval(parse(text=paste("stat_smooth(",temp,")")))
              myorder=paste(myorder,"+ \n stat_smooth(",temp,")",sep="")
            }  
            if(input$area) {
              temp=""
              if(input$areastat!="identity") temp=mypaste(temp,"stat='",input$areastat,"'")
              if(input$areacolor!="NA") temp=mypaste(temp,"colour='",input$areacolor,"'")
              if(input$areafill!="NA") temp=mypaste(temp,"fill='",input$areafill,"'")
              if(input$areasize!=0.5) temp=mypaste(temp,"size=",input$areasize)
              if(input$areaalpha!=1) temp=mypaste(temp,"alpha=",input$areaalpha)
              p<-p+eval(parse(text=paste("geom_area(",temp,")")))
              myorder=paste(myorder,"+ \n geom_area(",temp,")",sep="")
              
            }
            
            if(input$segment) {
              temp=""
#               allnumeric=0
#               
#               ## 모두 숫자로 되어있는지 검사
#               if(grepl("^[[:digit:]]",input$segmentxend)) allnumeric=allnumeric+1
#               if(grepl("^[[:digit:]]",input$segmentyend)) allnumeric=allnumeric+2
#               if(allnumeric==3) {
#                 temp=mypaste(temp,"xend=",input$segmentxend)
#                 temp=mypaste(temp,"yend=",input$segmentyend)
#               } else {
#                 
#                 if(allnumeric==1) {
#                   temp=mypaste(temp,"aes(yend=",input$segmentyend,")")
#                   temp=mypaste(temp,"xend=",input$segmentxend)
#                 } else if(allnumeric==2){
#                   temp=mypaste(temp,"aes(xend=",input$segmentxend,")")
#                   temp=mypaste(temp,"yend=",input$segmentyend)
#                 } else{
#                   temp=mypaste(temp,"aes(xend=",input$segmentxend)
#                   temp=mypaste(temp,"yend=",input$segmentyend,")")
#                 }
#                 
#               }
              if(input$segmentdata!="") temp=mypaste(temp,"data=",input$segmentdata)
              
              temp=mypaste(temp,"aes(xend=",input$segmentxend)
            
              if(input$segmentx!="") temp=mypaste(temp,"x=",input$segmentx)
              if(input$segmenty!="") temp=mypaste(temp,"y=",input$segmenty)
              if((input$segmentcolor!="NA")&(!isValidColor(input$segmentcolor))) temp=mypaste(temp,"colour='",input$segmentcolor,"'")     
              temp=mypaste(temp,"yend=",input$segmentyend,")")
              
              if(isValidColor(input$segmentcolor)) temp=mypaste(temp,"colour='",input$segmentcolor,"'")
              
              if(input$segmentsize!=1)  temp=mypaste(temp,"size=",input$segmentsize)
              if(input$segmentposition!="identity")  temp=mypaste(temp,"position=",input$segmentposition)
              if(input$segmentlineend!="butt") temp=mypaste(temp,"lineend='",input$segmentlineend,"'")
              if(input$segmentlinetype!=-1) temp=mypaste(temp,"linetype=",input$segmentlinetype)
              if(input$segmentalpha!=1) temp=mypaste(temp,"alpha=",input$segmentalpha)
              
              p<-p+eval(parse(text=paste("geom_segment(",temp,")"))) 
              myorder=paste(myorder,"+ \n geom_segment(",temp,")",sep="")
              
            }  
            if(input$curve) {
              temp=""
#               allnumeric=0
#               
#               ## 모두 숫자로 되어있는지 검사
#               if(grepl("^[[:digit:]]",input$curvexend)) allnumeric=allnumeric+1
#               if(grepl("^[[:digit:]]",input$curveyend)) allnumeric=allnumeric+2
#               if(allnumeric==3) {
#                 temp=mypaste(temp,"xend=",input$curvexend)
#                 temp=mypaste(temp,"yend=",input$curveyend)
#               } else {
#                 
#                 if(allnumeric==1) {
#                   temp=mypaste(temp,"aes(yend=",input$curveyend,")")
#                   temp=mypaste(temp,"xend=",input$curvexend)
#                 } else if(allnumeric==2){
#                   temp=mypaste(temp,"aes(xend=",input$curvexend,")")
#                   temp=mypaste(temp,"yend=",input$curveyend)
#                 } else{
#                   temp=mypaste(temp,"aes(xend=",input$curvexend)
#                   temp=mypaste(temp,"yend=",input$curveyend,")")
#                 }
#                 
#               }
              
              if(input$curvedata!="") temp=mypaste(temp,"data=",input$curvedata)
              temp=mypaste(temp,"aes(xend=",input$curvexend)
              if(input$curvex!="") temp=mypaste(temp,"x=",input$curvex)
              if(input$curvey!="") temp=mypaste(temp,"y=",input$curvey)
              if(input$curvecolor!="NA"){
                if(!isValidColor(input$curvecolor)) temp=mypaste(temp,"colour='",input$curvecolor,"'")
              } 
              temp=mypaste(temp,"yend=",input$curveyend,")")
              
              if(isValidColor(input$curvecolor)) temp=mypaste(temp,"colour='",input$curvecolor,"'")
              #if(input$curvecolor!="NA") temp=mypaste(temp,"colour='",input$curvecolor,"'")
              if(input$curvesize!=1)  temp=mypaste(temp,"size=",input$curvesize)
              if(input$curveposition!="identity")  temp=mypaste(temp,"position=",input$curveposition)
              if(input$curvelineend!="butt") temp=mypaste(temp,"lineend='",input$curvelineend,"'")
              if(input$curvelinetype!=-1) temp=mypaste(temp,"linetype=",input$curvelinetype)
              if(input$curvealpha!=1) temp=mypaste(temp,"alpha=",input$curvealpha)
              if(input$curvature!=0.5) temp=mypaste(temp,"curvature=",input$curvature)
              if(input$angle!=90) temp=mypaste(temp,"angle=",input$angle)
              if(input$ncp!=5) temp=mypaste(temp,"ncp=",input$ncp)
              
              p<-p+eval(parse(text=paste("geom_curve(",temp,")"))) 
              myorder=paste(myorder,"+ \n geom_curve(",temp,")",sep="")
              
            }  
            if(input$violin) {
              temp=""
              if(input$violinscale!="NA") temp=mypaste(temp,"scale='",input$violinscale,"'")
              if(input$violincolor!="NA") temp=mypaste(temp,"colour='",input$violincolor,"'")
              if(input$violinfill!="NA") temp=mypaste(temp,"fill='",input$violinfill,"'")
              if(input$violinadjust!=1) temp=mypaste(temp,"adjust=",input$violinadjust)
              if(input$violinalpha!=1) temp=mypaste(temp,"alpha=",input$violinalpha)
              if(input$violintrim==FALSE) temp=mypaste(temp,"trim=FALSE")
              p<-p+eval(parse(text=paste("geom_violin(",temp,")"))) 
              myorder=paste(myorder,"+ \n geom_violin(",temp,")",sep="")
            }  
            
            if(input$polygon) {
              temp=""
              
              if(input$polygoncolor!="NA") temp=mypaste(temp,"colour='",input$polygoncolor,"'")
              if(input$polygonfill!="NA") temp=mypaste(temp,"fill='",input$polygonfill,"'")
              if(input$polygonsize!=0.5) temp=mypaste(temp,"size=",input$polygonsize)
              if(input$polygonalpha!=1) temp=mypaste(temp,"alpha=",input$polygonalpha)
              
              p<-p+eval(parse(text=paste("geom_polygon(",temp,")"))) 
              myorder=paste(myorder,"+ \n geom_polygon(",temp,")",sep="")
            }  
            if(input$tile) {
              temp=""
              
              if(input$tilecolor!="NA") temp=mypaste(temp,"colour='",input$tilecolor,"'")
              if(input$tilefill!="NA") temp=mypaste(temp,"fill='",input$tilefill,"'")
              if(input$tilesize!=0.1) temp=mypaste(temp,"size=",input$tilesize)
              if(input$tilealpha!=1) temp=mypaste(temp,"alpha=",input$tilealpha)
              
              if(input$raster){
                p<-p+eval(parse(text=paste("geom_raster(",temp,")"))) 
                myorder=paste(myorder,"+ \n geom_raster(",temp,")",sep="")
                
              } else{
                p<-p+eval(parse(text=paste("geom_tile(",temp,")"))) 
                myorder=paste(myorder,"+ \n geom_tile(",temp,")",sep="")
              }
            }  
            if(input$path) {
              temp=""
              if(input$pathcolor!="NA") temp=mypaste(temp,"colour='",input$pathcolor,"'")
              if(input$pathlinetype!=1) temp=mypaste(temp,"linetype=",input$pathlinetype)
              if(input$pathsize!=1) temp=mypaste(temp,"size=",input$pathsize)
              if(input$pathalpha!=1) temp=mypaste(temp,"alpha=",input$pathalpha)
              
              temp=mypaste(myassign,temp)
              
              p<-p+eval(parse(text=paste("geom_path(",temp,")"))) 
              myorder=paste(myorder,"+ \n geom_path(",temp,")",sep="")
            }  
            if(input$boxplot) {
              temp=""
              
              if(input$boxcolor!="NA") temp=mypaste(temp,"colour='",input$boxcolor,"'")
              if(input$boxfill!="NA") temp=mypaste(temp,"fill='",input$boxfill,"'")
              if(input$boxoutcolor!="None"){
                if(input$boxoutcolor=="NA") temp=mypaste(temp,"outlier.colour=NA")
                else temp=mypaste(temp,"outlier.colour='",input$boxoutcolor,"'")
              }
              if(input$boxwidth!=1) temp=mypaste(temp,"width=",input$boxwidth)
              if(input$boxoutshape!=16) temp=mypaste(temp,"outlier.shape=",input$boxoutshape)
              if(input$boxoutsize!=2) temp=mypaste(temp,"outlier.size=",input$boxoutsize)
              if(input$boxalpha!=1) temp=mypaste(temp,"alpha=",input$boxalpha)
              if(input$boxnotch==TRUE) temp=mypaste(temp,"notch=TRUE")
              p<-p+eval(parse(text=paste("geom_boxplot(",temp,")"))) 
              myorder=paste(myorder,"+ \n geom_boxplot(",temp,")",sep="")
            }  
            
            if(input$jitter) {
              temp=""
              if(input$jitterwidth!=0.4) temp=mypaste(temp,"width=",input$jitterwidth)
              if(input$jitterheight!=0) temp=mypaste(temp,"height=",input$jitterheight)
              if(temp!="") temp=paste("position=position_jitter(",temp,")",sep="")
              if(input$jittercolor!="NA") temp=mypaste(temp,"colour='",input$jittercolor,"'")
              if(input$jitterfill!="NA") temp=mypaste(temp,"fill='",input$jitterfill,"'")
              if(input$jitteralpha!=1) temp=mypaste(temp,"alpha=",input$jitteralpha)
              if(input$jittersize!=2) temp=mypaste(temp,"size=",input$jittersize)
              if(input$jittershape!=16) temp=mypaste(temp,"shape=",input$jittershape)
              
              if(temp==""){
                p<-p+geom_jitter()
                myorder=paste(myorder,"+ geom_jitter()",sep="") 
              } else{
                p<-p+eval(parse(text=paste("geom_jitter(",temp,")",sep="")))
                myorder=paste(myorder,"+ \n geom_jitter(",temp,")",sep="")
              }
            }
            if(input$statsummary){
              temp=""
              
              if(input$sumcolor!="NA") temp=mypaste(temp,"colour='",input$sumcolor,"'")
              if(input$sumfill!="NA") temp=mypaste(temp,"fill='",input$sumfill,"'")
              if(input$sumgeom!="NA") temp=mypaste(temp,"geom='",input$sumgeom,"'")
              if(input$sumfuny!="NA") temp=mypaste(temp,"fun.y=",input$sumfuny)
              temp=mypaste(temp,"shape=",input$sumshape)
              temp=mypaste(temp,"size=",input$sumsize)
              
              p<-p+eval(parse(text=paste("stat_summary(",temp,")"))) 
              myorder=paste(myorder,"+ \n stat_summary(",temp,")",sep="")
              
            }
            if(input$statdensity2d) {
              temp1=""
              temp=""
              
              if(input$tdcolor=="..level..") temp1=mypaste(temp1,"colour=..level..")
              if(input$tdfill=="..density..") temp1=mypaste(temp1,"fill=..density..")
              if(input$tdfill=="..level..") temp1=mypaste(temp1,"fill=..level..")
              if(input$tdalpha=="..density..") temp1=mypaste(temp1,"alpha=..density..")
              if(myassign!="") {
                myassign=unlist(strsplit(myassign,")"))[1]
                if(temp1!="") temp1=mypaste(myassign,temp1,")")
              }         
              else if(temp1!="") temp1=paste0("aes(",temp1,")")
              temp=temp1
              if(input$tdgeom!="density2d") temp=mypaste(temp,"geom='",input$tdgeom,"'")
              if(!(input$tdcolor %in% c("NA","..level.."))) temp=mypaste(temp,"colour='",input$tdcolor,"'")
              if(!(input$tdfill %in% c("NA","..density..","..level.."))) temp=mypaste(temp,"fill='",input$tdfill,"'")
              if(!(input$tdalpha %in% c("","..density.."))) temp=mypaste(temp,"alpha=",input$tdalpha)
              if(as.numeric(input$tdsize)!="0.5") temp=mypaste(temp,"size=",input$tdsize)
              #if(input$tdbins!="") temp=mypaste(temp,"bins=",input$tdbins)
              if(input$tdh!="") temp=mypaste(temp,"h=",input$tdh)
              if(input$tdcontour==FALSE) temp=mypaste(temp,"contour=FALSE")
              
              p<-p+eval(parse(text=paste("stat_density2d(",temp,")"))) 
              myorder=paste(myorder,"+ \n stat_density2d(",temp,")",sep="")
              
            }   
            if(input$statcontour & (input$contourzvar!="None")) {
              temp=paste0("aes(z=",input$contourzvar)
              if(input$contourcolor=="..level..") temp=mypaste(temp,"colour=..level..")
              temp=paste0(temp,")")
              if(input$contourgeom!="path") temp=mypaste(temp,"geom='",input$contourgeom,"'")
              if(input$contourbinwidth!=0) temp=mypaste(temp,"binwidth=",input$contourbinwidth)
              if(input$contoursize!=0.5) temp=mypaste(temp,"size=",input$contoursize)
              if(!(input$contourcolor %in% c("NA","..level.."))) 
                temp=mypaste(temp,"colour='",input$contourcolor,"'")
              
              p<-p+eval(parse(text=paste("stat_contour(",temp,")"))) 
              myorder=paste(myorder,"+ \n stat_contour(",temp,")",sep="")
              
            }
          }    
          if(input$line) {
            temp=""
            if(input$linestat!="identity") temp=mypaste(temp,"stat='",input$linestat,"'")
            if(input$linecolor!="NA") temp=mypaste(temp,"colour='",input$linecolor,"'")
            if(input$lineposition=="dodge(0.3)") temp=mypaste(temp,"position=position_",input$lineposition)
            else if(input$lineposition!="identity") temp=mypaste(temp,"position='",input$lineposition,"'")
            if(input$linetype!=-1) temp=mypaste(temp,"linetype=",input$linetype)
            if(input$linesize!=0.5)  temp=mypaste(temp,"size=",input$linesize)
            if(input$lineadjust!=1) temp=mypaste(temp,"adjust=",input$lineadjust)
            p<-p+eval(parse(text=paste("geom_line(",temp,")"))) 
            myorder=paste(myorder,"+ \n geom_line(",temp,")",sep="")
            
          }    
        if(input$text) {
          if(input$textlabel!=""){
            temp=""
            if(myassign==""){
              if(input$textdata!="") temp=mypaste(temp,"data=",input$textdata)
              temp=mypaste(temp,"aes(label=",input$textlabel)
              if(input$texty!="") temp=mypaste(temp,"y=",input$texty)
              if(input$textx!="") temp=mypaste(temp,"x=",input$textx)
              temp=paste(temp,")",sep="")
              
            } else {
              if(input$textdata!="") {
                temp=mypaste(temp,"data=",input$textdata)
              } else {
                temp=mypaste(temp,"data=",input$mydata)
              }
              temp=mypaste(temp,"aes(label=",input$textlabel)
              if(input$textx!="") {
                temp=mypaste(temp,"x=",input$textx)
              } else{
                temp=mypaste(temp,"x=",input$xvar)
              }
              if(input$texty!=""){
                temp=mypaste(temp,"y=",input$texty)
              } else{
                temp=mypaste(temp,"y=",input$yvar)
              }
              
              temp=paste(temp,")",sep="")

            }
            if(input$textstat!="identity") temp=mypaste(temp,"stat='",input$textstat,"'")          
            if(input$textposition!="identity") {
            if(input$textposition=="dodge") temp=mypaste(temp,"position=position_dodge(",input$textdodgeposition,")")
            else if(input$textposition=="stack") temp=mypaste(temp,"position=position_stack(vjust=0.5)")
            else if(input$textposition=="fill") temp=mypaste(temp,"position=position_fill(vjust=0.5)")
            else temp=mypaste(temp,"position='",input$textposition,"'")
            }
            if(input$textvjust!="") temp=mypaste(temp,"vjust=",input$textvjust)
            if(input$texthjust!="") temp=mypaste(temp,"hjust=",input$texthjust)
            if(input$textnudge_x!="0") temp=mypaste(temp,"nudge_x=",input$textnudge_x)
            if(input$textnudge_y!="0") temp=mypaste(temp,"nudge_y=",input$textnudge_y)
            if(input$textsize!=5) temp=mypaste(temp,"size=",input$textsize)
            if(input$textcolor!="NA") temp=mypaste(temp,"colour='",input$textcolor,"'") 
            if(input$fonts!="NA") temp=mypaste(temp,"family='",input$fonts,"'") 
            if(!input$geom_label & input$check_overlap) temp=mypaste(temp,"check_overlap=TRUE") 
            if(input$inherit.aes==FALSE) temp=mypaste(temp,"inherit.aes=FALSE") 
            if(input$textother!="") temp=mypaste(temp,input$textother) 
            
            if(input$geom_label==TRUE){
              p<-p+eval(parse(text=paste("geom_label(",temp,")",sep="")))
              myorder=paste(myorder,"+ \n geom_label(",temp,")",sep="")
                
            } else {
              p<-p+eval(parse(text=paste("geom_text(",temp,")",sep="")))
              myorder=paste(myorder,"+ \n geom_text(",temp,")",sep="")
            }
          }
        }
        if(input$hline & (input$hlineintercept!="")) {
          temp=""
          try(a<-eval(parse(text=input$hlineintercept)))
          if(is.numeric(a)) {
             temp=paste0("yintercept=",input$hlineintercept)
          } else {
            temp=paste0("aes(yintercept=",input$hlineintercept,")")  
          }
          if(input$hlinecolor!="NA") temp=mypaste(temp,"colour='",input$hlinecolor,"'")
          if(input$hlinetype!=-1) temp=mypaste(temp,"linetype=",input$hlinetype)
          if(input$hlinesize!=0.5)  temp=mypaste(temp,"size=",input$hlinesize)
          p<-p+eval(parse(text=paste("geom_hline(",temp,")"))) 
          myorder=paste(myorder,"+ \n geom_hline(",temp,")",sep="")
          
        }    
        if(input$vline & (input$vlineintercept!="")) {
          temp=""
          try(a<-eval(parse(text=input$vlineintercept)))
          if(is.numeric(a)) {
            temp=paste0("xintercept=",input$vlineintercept)
          } else {
            temp=paste0("aes(xintercept=",input$vlineintercept,")")  
          }
          if(input$vlinecolor!="NA") temp=mypaste(temp,"colour='",input$vlinecolor,"'")
          if(input$vlinetype!=-1) temp=mypaste(temp,"linetype=",input$vlinetype)
          if(input$vlinesize!=0.5)  temp=mypaste(temp,"size=",input$vlinesize)
          p<-p+eval(parse(text=paste("geom_vline(",temp,")"))) 
          myorder=paste(myorder,"+ \n geom_vline(",temp,")",sep="")
          
        }
        if(input$abline & (input$ablineintercept!="") &(input$ablineslope!="")) {
          temp=""
          try(a<-eval(parse(text=input$ablineintercept)))
          try(b<-eval(parse(text=input$ablineslope)))
          if(is.numeric(a) & is.numeric(b)) {
            temp=mypaste(temp,"intercept=",input$ablineintercept)
            temp=mypaste(temp,"slope=",input$ablineslope)
          } else if(is.numeric(a) | is.numeric(b)){
            if(is.numeric(b)){
              temp=paste0("aes(intercept=",input$ablineintercept,")")  
              temp=mypaste(temp,"slope=",input$ablineslope)
            } else {
              temp=paste0("aes(slope=",input$ablineslope,")")
              temp=mypaste(temp,"intercept=",input$ablineintercept)  
            }  
          } else{
            temp=paste0("aes(intercept=",input$ablineintercept)  
            temp=mypaste(temp,"slope=",input$ablineslope,")")
          }
          if(input$ablinecolor!="NA") temp=mypaste(temp,"colour='",input$ablinecolor,"'")
          if(input$ablinetype!=-1) temp=mypaste(temp,"linetype=",input$ablinetype)
          if(input$ablinesize!=0.5)  temp=mypaste(temp,"size=",input$ablinesize)
          
          p<-p+eval(parse(text=paste("geom_abline(",temp,")"))) 
          myorder=paste(myorder,"+ \n geom_abline(",temp,")",sep="")
          
        }
        
         
      
      if(input$dotplot) {
        temp=""
        if(input$dotmethod!="dotdensity") temp=mypaste(temp,"method='",input$dotmethod,"'")
        if(input$dotstackdir!="up") temp=mypaste(temp,"stackdir='",input$dotstackdir,"'")
        if(input$dotbinpositions!="bygroup") temp=mypaste(temp,"binpositions='",input$dotbinpositions,"'")
        if(input$dotbinaxis!="x") temp=mypaste(temp,"binaxis='",input$dotbinaxis,"'")
        if(input$dotcolor!="NA") temp=mypaste(temp,"colour='",input$dotcolor,"'")
        if(input$dotfill!="NA") temp=mypaste(temp,"fill='",input$dotfill,"'")
        if(input$dotalpha!=1) temp=mypaste(temp,"alpha=",input$dotalpha)
        if(input$dotbinwidth!=0) temp=mypaste(temp,"binwidth=",input$dotbinwidth)
        if(input$dotstackratio!=1) temp=mypaste(temp,"stackratio=",input$dotstackratio)
        
        p<-p+eval(parse(text=paste("geom_dotplot(",temp,")"))) 
        myorder=paste(myorder,"+ \n geom_dotplot(",temp,")",sep="")
        
      }
      if(input$errorbar) {
            if((input$ymin!="") & (input$ymax!="")){
            temp=""
            temp=mypaste(temp,"aes(ymin=",input$ymin,",ymax=",input$ymax,")")
            if(input$ebcolor!="NA") temp=mypaste(temp,"colour='",input$ebcolor,"'")
            if(input$ebsize!=0.5) temp=mypaste(temp,"size=",input$ebsize)
            if(input$ebwidth!=1) temp=mypaste(temp,"width=",input$ebwidth)
            if(input$ebpos!=0) temp=mypaste(temp,"position=position_dodge(",input$ebpos,")")
            
            p<-p+eval(parse(text=paste("geom_errorbar(",temp,")"))) 
            myorder=paste(myorder,"+ \n geom_errorbar(",temp,")",sep="")
            }
      }
      if(input$point) {
            temp=""
            if(input$pointposition=="dodge(0.3)") temp=mypaste(temp,"position=position_",input$pointposition)
            else if(input$pointposition!="identity") temp=mypaste(temp,"position='",input$pointposition,"'")
            if(input$pointcolor!="NA") temp=mypaste(temp,"colour='",input$pointcolor,"'")
            if(input$pointfill!="NA") temp=mypaste(temp,"fill='",input$pointfill,"'")
            if(input$pointsize!=2) temp=mypaste(temp,"size=",input$pointsize)
            if(input$pointshape!=19) temp=mypaste(temp,"shape=",input$pointshape)
            if(input$pointalpha!=1) temp=mypaste(temp,"alpha=",input$pointalpha)
            
            temp=mypaste(myassign,temp)
            p<-p+eval(parse(text=paste("geom_point(",temp,")",sep="")))
            myorder=paste(myorder,"+ \n geom_point(",temp,")",sep="")
      }
        
      if(input$count) {
        temp=""
        if(input$countposition=="dodge(0.3)") temp=mypaste(temp,"position=position_",input$countposition)
        else if(input$countposition!="identity") temp=mypaste(temp,"position='",input$countposition,"'")
        if(input$countcolor!="NA") temp=mypaste(temp,"colour='",input$countcolor,"'")
        if(input$countfill!="NA") temp=mypaste(temp,"fill='",input$countfill,"'")
        #if(input$pointsize!=2) temp=mypaste(temp,"size=",input$pointsize)
        if(input$countshape!=19) temp=mypaste(temp,"shape=",input$countshape)
        if(input$countalpha!=1) temp=mypaste(temp,"alpha=",input$countalpha)
        
        temp=mypaste(myassign,temp)
        p<-p+eval(parse(text=paste("geom_count(",temp,")",sep="")))
        myorder=paste(myorder,"+ \n geom_count(",temp,")",sep="")
      }  
      if(input$annotate) {
        temp=""
        
        annresult=0
        anntrue=0
        
        if(input$annx!="") annresult=annresult+10
        if(input$anny!="") annresult=annresult+10
        if(input$annxend!="") annresult=annresult+1
        if(input$annyend!="") annresult=annresult+1
        if(input$annlabel!="") annresult=annresult+100
        
        if(input$anngeom=="text") {
            if(annresult>=120) anntrue=1
        }
        else {
          if((annresult%%100)==22) anntrue=1
        }  
        if(anntrue) {
          temp=paste0("geom='",input$anngeom,"'")
          
          if(input$anngeom=="rect"){
            if(input$annx!="") temp=mypaste(temp,"xmin=",input$annx)
            if(input$annxend!="") temp=mypaste(temp,"xmax=",input$annxend)
            if(input$anny!="") temp=mypaste(temp,"ymin=",input$anny)
            if(input$annyend!="") temp=mypaste(temp,"ymax=",input$annyend)
            
          } else if(input$anngeom=="pointrange"){
            if(input$annx!="") temp=mypaste(temp,"x=",input$annx)
            if(input$annxend!="") temp=mypaste(temp,"ymin=",input$annxend)
            if(input$anny!="") temp=mypaste(temp,"y=",input$anny)
            if(input$annyend!="") temp=mypaste(temp,"ymax=",input$annyend)
          } else {
            if(input$annx!="") temp=mypaste(temp,"x=",input$annx)
            if(input$annxend!="") temp=mypaste(temp,"xend=",input$annxend)
            if(input$anny!="") temp=mypaste(temp,"y=",input$anny)
            if(input$annyend!="") temp=mypaste(temp,"yend=",input$annyend)
            
          } 
          
          if(input$anncolor!="NA") temp=mypaste(temp,"colour='",input$anncolor,"'")
          if(input$annfill!="NA") temp=mypaste(temp,"fill='",input$annfill,"'")
          if(input$annsize!=5) temp=mypaste(temp,"size=",input$annsize)
          if(input$annalpha!=1) temp=mypaste(temp,"alpha=",input$annalpha)
          if(input$annlabel!="") {
            if(input$annlabeltext) temp=mypaste(temp,"label='",input$annlabel,"'")
            else temp=mypaste(temp,"label=",input$annlabel)
          }  
          if(input$annarrow!="") temp=mypaste(temp,"arrow=",input$annarrow)
          if(input$annhjust!="") temp=mypaste(temp,"hjust=",input$annhjust)
          if(input$annvjust!="") temp=mypaste(temp,"vjust=",input$annvjust)
          if(input$annfamily!="NA") temp=mypaste(temp,"family='",input$annfamily,"'")
          if(input$annfontface!="plain") temp=mypaste(temp,"fontface='",input$annfontface,"'")
          if(input$annparse==TRUE) temp=mypaste(temp,"parse=TRUE")
          
          p<-p+eval(parse(text=paste("annotate(",temp,")",sep="")))
          myorder=paste(myorder,"+ \n annotate(",temp,")",sep="")
        
        }
      }
    
      if(input$anncustom & (input$grob!="")){
        if(any(class(try(table1<-eval(parse(text=input$grob))))=="grob")){
        temp=""
        temp=paste0("grob=",input$grob)
        
      
          if(input$acxmin!="-Inf") temp=mypaste(temp,"xmin=",input$acxmin)
          if(input$acxmax!="Inf") temp=mypaste(temp,"xmax=",input$acxmax)
          if(input$acymin!="-Inf") temp=mypaste(temp,"ymin=",input$acymin)
          if(input$acymax!="Inf") temp=mypaste(temp,"ymax=",input$acymax)
          
        
        p<-p+eval(parse(text=paste("annotation_custom(",temp,")",sep="")))
        myorder=paste(myorder,"+ \n annotation_custom(",temp,")",sep="")
      }
      }
      if(input$map & (input$mapid!="") & (input$mapmap!="")) {
            temp=""
            temp=paste0("map_id=",input$mapid)
            if(input$mapfill!="NA") {
              if(!(input$mapfill %in% colors())) temp=mypaste(temp,"fill=",input$mapfill)
            }  
            temp=paste0("aes(",temp,")")
            temp=mypaste(temp,"map=",input$mapmap)
            if(input$mapcolor!="NA") temp=mypaste(temp,"colour='",input$mapcolor,"'")
            if(input$mapfill %in% colors()) temp=mypaste(temp,"fill='",input$mapfill,"'")
            if(input$mapsize!=0.5) temp=mypaste(temp,"size=",input$mapsize)
            if(input$mapalpha!=1) temp=mypaste(temp,"alpha=",input$mapalpha)
            
            updateTextInput(session,"xlim",value=paste0(input$mapmap,"$long"))
            updateTextInput(session,"ylim",value=paste0(input$mapmap,"$lat"))
            
            p<-p+eval(parse(text=paste("geom_map(",temp,")"))) 
            myorder=paste(myorder,"+ \n geom_map(",temp,")",sep="")
      }      
        if(input$palette!="None") {
                if(input$palettecont){
                        if(input$palettecolor){
                                p<-p+eval(parse(text=paste("scale_colour_gradientn(colours=palcolors)",sep=""))) 
                                myorder=paste(myorder,"+ \n scale_colour_gradientn(colours=palcolors))",sep="") 
                        }
                        else{
                                p<-p+eval(parse(text=paste("scale_fill_gradientn(colours=palcolors)",sep=""))) 
                                myorder=paste(myorder,"+ \n scale_fill_gradientn(colours=palcolors)",sep="") 
                        }
                } else{
                        
                        if(input$palettecolor){
                                p<-p+scale_colour_brewer(palette=input$palette)
                                myorder=paste(myorder,"+ \n scale_colour_brewer(palette='",input$palette,"')",sep="") 
                        }
                        else{
                                if(input$palettereverse){
                                        p<-p+scale_fill_brewer(palette=input$palette,direction=-1)
                                        myorder=paste(myorder,"+ \n scale_fill_brewer(palette='",input$palette,"',direction=-1)",sep="")
                                } else{
                                        
                                        p<-p+scale_fill_brewer(palette=input$palette)
                                        myorder=paste(myorder,"+ \n scale_fill_brewer(palette='",input$palette,"')",sep="")
                                        
                                }
                        }
                }
        }
        if(input$cpalette!="") {
                if(input$cpalettec){
                        p<-p+eval(parse(text=paste("scale_colour_gradientn(colours=c(",input$cpalette,"))",sep=""))) 
                        myorder=paste(myorder,"+ \n scale_colour_gradientn(colours=c(",input$cpalette,"))",sep="") 
                }
                else{
                        p<-p+eval(parse(text=paste("scale_fill_gradientn(colours=c(",input$cpalette,"))",sep=""))) 
                        myorder=paste(myorder,"+ \n scale_fill_gradientn(colours=c(",input$cpalette,"))",sep="") 
                }
        }  
      if(input$coord_polar){
        temp=""
        
        if(input$theta!="x") temp=mypaste(temp,"theta='",input$theta,"'")
        if(input$start!=0) temp=mypaste(temp,"start=",input$start)
        if(input$direction!=1) temp=mypaste(temp,"direction=",input$direction)
        
        p<-p+eval(parse(text=paste("coord_polar(",temp,")",sep="")))
        myorder=paste(myorder,"+ \n coord_polar(",temp,")",sep="")
           
      }
      if(input$coordfixed){
            temp=""
            
            if(input$coordratio!="1") temp=mypaste(temp,"ratio=",input$coordratio)
            
            p<-p+eval(parse(text=paste("coord_fixed(",temp,")",sep="")))
            myorder=paste(myorder,"+ \n coord_fixed(",temp,")",sep="")
      }  
      if(input$coordmap){
            temp=""
            
            if(input$projection!="mercator") temp=mypaste(temp,"projection='",input$projection,"'")
            
            p<-p+eval(parse(text=paste("coord_map(",temp,")",sep="")))
            myorder=paste(myorder,"+ \n coord_map(",temp,")",sep="")
      }      
     
      if(input$legendreverse) {
        if(input$fillvar!="None"){
          p<-p+guides(fill=guide_legend(reverse=TRUE))
          myorder=paste(myorder,"+ \n guides(fill=guide_legend(reverse=TRUE))",sep="")
        }
        if(input$colorvar!="None"){
          p<-p+guides(colour=guide_legend(reverse=TRUE))
          myorder=paste(myorder,"+ \n guides(colour=guide_legend(reverse=TRUE))",sep="")
        }
      }  
      
      if(input$facetrow!="None"| input$facetcol!="None") {    
        temp=""
        temp <- paste(ifelse(input$facetrow=="None",".",input$facetrow), '~', ifelse(input$facetcol=="None",".",input$facetcol))
        if(input$labeller!="label_value") temp=mypaste(temp,"labeller=",input$labeller)
        if(input$facetswitch!="None") temp=mypaste(temp,"switch='",input$facetswitch,"'")
        if(input$facetscales!="fixed") temp=mypaste(temp,"scales='",input$facetscales,"'")
        if(input$facetspace!="fixed") temp=mypaste(temp,"space='",input$facetspace,"'")
        
        p <- p + eval(parse(text=paste("facet_grid(",temp,")",sep="")))
        myorder=paste(myorder,"+ \n facet_grid(",temp,")",sep="")
        
      }  
      if(input$facetwrap!="None"){
          temp=input$facetwrap
          if(input$labeller!="label_value") temp=mypaste(temp,"labeller=",input$labeller)
          if(input$facetswitch!="None") temp=mypaste(temp,"switch='",input$facetswitch,"'")
          if(input$facetscales!="fixed") temp=mypaste(temp,"scales='",input$facetscales,"'")
          if(input$facetwrapncol!=0) temp=mypaste(temp,"ncol=",input$facetwrapncol)
          p<-eval(parse(text=paste("p+ facet_wrap(~",temp,")",sep="")))
          myorder=paste(myorder,"+ \n facet_wrap(~",temp,")",sep="")
          
      }   
  
      if(input$coordflip) {
        temp=""
        if(input$xlim!="") temp=mypaste(temp,"xlim=c(",input$xlim,")")
        if(input$ylim!="") temp=mypaste(temp,"ylim=c(",input$ylim,")")
        p=p+eval(parse(text=paste("coord_flip(",temp,")"))) 
        myorder=paste(myorder,"+ \n coord_flip(",temp,")",sep="")
      }  else {
        if(input$xlim!="") {
          a=unlist(strsplit(input$xlim,","))
          if (length(a)==1) temp=paste0("expand_limits(x=",a,")")
          else if(a[1]=="") temp=paste0("expand_limits(xend=",a[2],")")
          else temp=paste0("xlim(",input$xlim,")")
           p=p+eval(parse(text=temp)) 
           myorder=paste(myorder,"+ \n",temp,sep="")
        }           
        if(input$ylim!="") {
          a=unlist(strsplit(input$ylim,","))
          if (length(a)==1) temp=paste0("expand_limits(y=",a,")")
          else if(a[1]=="") temp=paste0("expand_limits(yend=",a[2],")")
          else temp=paste0("ylim(",input$ylim,")")
          p=p+eval(parse(text=temp)) 
          myorder=paste(myorder,"+ \n",temp,sep="")
        }  
      }
      if(input$addrorder) {
        myorder=paste(myorder,"+ \n", input$rordertext,sep="")
        p<-eval(parse(text=paste("p+",input$rordertext))) 
        
      }  
      
      if(input$title!=""){
        p<-p+ggtitle(input$title)
        if(input$fonts!="NA") p=eval(parse(text=paste("p+ theme(title=element_text(family='",input$fonts,"'))",sep="")))
        myorder=paste(myorder,"+ \n ggtitle(\"",input$title,"\")",sep="")
      }
      if(input$theme!="grey"){
        temp=""
        if(input$fontsize!=12) temp=mypaste(temp,"base_size=",input$fontsize)
        if(input$fonts!="NA") temp=mypaste(temp,"base_family='",input$fonts,"'")
        myorder=paste(myorder,"+ \n theme_",input$theme,"(",temp,")",sep="")
        p<-eval(parse(text=paste("p+ theme_",input$theme,"(",temp,")",sep="")))
      }
      else {
        temp=""
        if(input$fontsize!=12) temp=mypaste(temp,"base_size=",input$fontsize)
        if(input$fonts!="NA") temp=mypaste(temp,"base_family='",input$fonts,"'")
        if(temp!=""){
          myorder=paste(myorder,"+ \n theme_grey(",temp,")",sep="")
          p<-eval(parse(text=paste("p+ theme_grey(",temp,")",sep="")))
        }
      } 
      if(input$ggthemes=="solarized-light"){
          myorder=paste(myorder,"+ \n theme_solarized()",sep="")
          myorder=paste(myorder,"+ \n scale_colour_solarized('blue')",sep="")
          p<-p+eval(parse(text=paste("theme_solarized()")))
          p<-eval(parse(text=paste("p+","scale_colour_solarized('blue')",sep="")))
      } else if(input$ggthemes=="solarized-dark"){
        myorder=paste(myorder,"+ \n theme_solarized(light=FALSE)",sep="")
        myorder=paste(myorder,"+ \n scale_colour_solarized('red')",sep="")
        p<-eval(parse(text="p+ theme_solarized(light=FALSE)"))
        p<-eval(parse(text="p+ scale_colour_solarized('red')"))
      } else if(input$ggthemes=="solarized-alternative"){ 
        myorder=paste(myorder,"+ \n theme_solarized_2()",sep="")
        myorder=paste(myorder,"+ \n scale_colour_solarized('blue')",sep="")
        p<-eval(parse(text="p+ theme_solarized_2()"))
        p<-eval(parse(text="p+ scale_colour_solarized('blue')"))
        
      } else if(input$ggthemes=="tableau"){ 
        myorder=paste(myorder,"+ \n theme_igray()",sep="")
        myorder=paste(myorder,"+ \n scale_colour_tableau()",sep="")
        p<-eval(parse(text="p+ theme_igray()"))
        p<-eval(parse(text="p+ scale_colour_tableau()"))
        
      } else if(input$ggthemes=="excel"){ 
        
        myorder=paste(myorder,"+ \n theme_",input$ggthemes,"()",sep="")
        myorder=paste(myorder,"+ \n scale_colour_",input$ggthemes,"()",sep="")
        myorder=paste(myorder,"+ \n scale_fill_",input$ggthemes,"()",sep="")
        p<-eval(parse(text=paste("p+ theme_",input$ggthemes,"()",sep="")))
        p<-eval(parse(text=paste("p+ scale_colour_",input$ggthemes,"()",sep="")))
        p<-eval(parse(text=paste("p+ scale_fill_",input$ggthemes,"()",sep="")))
      }  else if(input$ggthemes=="pander"){ 
        
        myorder=paste(myorder,"+ \n theme_",input$ggthemes,"()",sep="")
        myorder=paste(myorder,"+ \n scale_colour_",input$ggthemes,"()",sep="")
        myorder=paste(myorder,"+ \n scale_fill_",input$ggthemes,"()",sep="")
        p<-eval(parse(text=paste("p+ theme_",input$ggthemes,"()",sep="")))
        p<-eval(parse(text=paste("p+ scale_colour_",input$ggthemes,"()",sep="")))
        p<-eval(parse(text=paste("p+ scale_fill_",input$ggthemes,"()",sep="")))
      }  else if(input$ggthemes=="hc-dark"){ 
        
        myorder=paste(myorder,"+ \n theme_hc(bgcolor='darkunica')",sep="")
        myorder=paste(myorder,"+ \n scale_colour_hc('darkunica')",sep="")
        myorder=paste(myorder,"+ \n scale_fill_hc('darkunica')",sep="")
        p<-eval(parse(text=paste("p+ theme_hc(bgcolor='darkunica')",sep="")))
        p<-eval(parse(text=paste("p+ scale_colour_hc('darkunica')",sep="")))
        p<-eval(parse(text=paste("p+ scale_fill_hc('darkunica')",sep="")))
      } else if(input$ggthemes=="clean"){ 
        
        myorder=paste(myorder,"+ \n theme_clean()",sep="")
        p<-eval(parse(text=paste("p+ theme_clean()",sep="")))
      }  
      else if(input$ggthemes!="None"){
        
        myorder=paste(myorder,"+ \n theme_",input$ggthemes,"()",sep="")
        myorder=paste(myorder,"+ \n scale_colour_",input$ggthemes,"()",sep="")
        p<-eval(parse(text=paste("p+ theme_",input$ggthemes,"()",sep="")))
        p<-eval(parse(text=paste("p+ scale_colour_",input$ggthemes,"()",sep="")))
      }
      if(input$legendposition!="right") {
        p<-p+theme(legend.position=input$legendposition)
        myorder=paste(myorder,"+ \n theme(legend.position='",input$legendposition,"')",sep="")
      }  
          if(input$xlab=="NA"){
            p=p+theme(axis.title.x=element_blank())
            myorder=paste(myorder,"+ \n theme(axis.title.x=element_blank())",sep="")
          } else if(input$xlab!=""){
            p<-p+xlab(input$xlab)
            myorder=paste(myorder,"+ \n xlab('",input$xlab,"')",sep="")
          } 
          if(input$xreverse){
            p=p+scale_x_reverse()
            myorder=paste(myorder,"+ \n scale_x_reverse()",sep="")
          }
          if(input$remove.x.text){
            p=p+theme(axis.text.x=element_blank())
            myorder=paste(myorder,"+ \n theme(axis.text.x=element_blank())",sep="")
          }  
          if(input$log.x){
            p=p+scale_x_log10()
            myorder=paste(myorder,"+ \n scale_x_log10()",sep="")
          }
          if(input$ylab=="NA"){
            p=p+theme(axis.title.y=element_blank())
            myorder=paste(myorder,"+ \n theme(axis.title.y=element_blank())",sep="")
          } else if(input$ylab!=""){
            p<-p+ylab(input$ylab)
            myorder=paste(myorder,"+ \n ylab('",input$ylab,"')",sep="")
          } 
          if(input$yreverse){
            p=p+scale_y_reverse()
            myorder=paste(myorder,"+ \n scale_y_reverse()",sep="")
          }
          if(input$remove.y.text){
            p=p+theme(axis.text.y=element_blank())
            myorder=paste(myorder,"+ \n theme(axis.text.y=element_blank())",sep="")
          }
          if(input$log.y){
            p=p+scale_y_log10()
            myorder=paste(myorder,"+ \n scale_y_log10()",sep="")
          }
          
          if(input$remove.tick){
            p=p+theme(axis.ticks=element_blank())
            myorder=paste(myorder,"+ \n theme(axis.ticks=element_blank())",sep="")
          }   
          if(input$axis.blank){
            p=p+theme_axis_blank()
            myorder=paste(myorder,"+ \n theme_axis_blank()",sep="")
          }
        if(input$strip.bg.blank){
          p=p+theme(strip.background=element_blank())
          myorder=paste(myorder,"+ \n theme(strip.background=element_blank())",sep="")
        }
      
      updateTextInput(session,"rorder",value=myorder)
      p
      
      }
      
})

output$session=renderPrint({
   print(sessionInfo())
})


observeEvent(input$ClearAll,{
  
    mylist=paste0("ppt",1:10)
    for(i in 1:10) updateTextInput(session,mylist[i],value="")
    
})

output$about=renderUI({
        
        if(input$language=="en") includeMarkdown("aboute.md")
        else includeMarkdown("about.md")
})

output$ipmain=renderPrint({
        text=makePlotCode()
        cat(text)
})

observe({
        input$iplotmain
        
        text=makePlotCode()
        updateTextInput(session,"iplotcode",value=text)
})

makePlotCode=reactive({
        
        input$iplotmain
        
        if(input$iplotmain=="ggSpine") {
                text=paste0("ggSpine(",input$mydata)
                text=paste0(text,input$mapping)
                # aestemp=c()
                # if(input$ix5!="None") aestemp=c(aestemp,paste0("x=",input$ix5))
                # if(input$iy5!="None") aestemp=c(aestemp,paste0("y=",input$iy5))
                # if(input$ifill5!="None") aestemp=c(aestemp,paste0("fill=",input$ifill5))
                # text=add_aes(text,aestemp)
                if(input$stat5!="count") text=paste0(text,",stat='",input$stat5,"'")
                if(input$position5!="fill") text=paste0(text,",position='",input$position5,"'")
                if(input$palette5!="Blues") text=paste0(text,",palette='",input$palette5,"'")
                if(input$colour5!="black") text=paste0(text,",colour='",input$colour5,"'")
                if(input$width5!=0) text=paste0(text,",width=",input$width5)
                if(input$size!=0.2) text=paste0(text,",size=",input$size)
                if(input$addlabel==TRUE) text=paste0(text,",addlabel=TRUE")
                if(input$polar5==TRUE) text=paste0(text,",polar=TRUE")
                
        } else if(input$iplotmain=="ggBar") {
                text=paste0("ggBar(",input$mydata)
                text=paste0(text,input$mapping)
                # aestemp=c()
                # if(input$ix6!="None") aestemp=c(aestemp,paste0("x=",input$ix6))
                # if(input$iy6!="None") aestemp=c(aestemp,paste0("y=",input$iy6))
                # if(input$ifill6!="None") aestemp=c(aestemp,paste0("fill=",input$ifill6))
                # text=add_aes(text,aestemp)
                if(input$stat6!="count") text=paste0(text,",stat='",input$stat6,"'")
                if(input$position6!="stack") text=paste0(text,",position='",input$position6,"'")
                if(input$palette6!="None") text=paste0(text,",palette='",input$palette6,"'")
                if(input$colour6!="None") text=paste0(text,",colour='",input$colour6,"'")
                if(input$size6!=0.2) text=paste0(text,",size=",input$size6)
                if(input$addlabel6==TRUE) text=paste0(text,",addlabel=TRUE")
                if(input$horizontal6==TRUE) text=paste0(text,",horizontal=TRUE")
                if(input$width6!=0) text=paste0(text,",width=",input$width6)
                if(input$polar6==TRUE) text=paste0(text,",polar=TRUE")
                if(input$reverse6==TRUE) text=paste0(text,",reverse=TRUE")
                
        } else if(input$iplotmain=="ggRose") {
                text=paste0("ggRose(",input$mydata)
                text=paste0(text,input$mapping)
                aestemp=c()
                # if(input$ix9!="None") aestemp=c(aestemp,paste0("x=",input$ix9))
                # if(input$iy9!="None") aestemp=c(aestemp,paste0("y=",input$iy9))
                # if(input$ifill9!="None") aestemp=c(aestemp,paste0("fill=",input$ifill9))
                # text=add_aes(text,aestemp)
                if(input$position9!="stack") text=paste0(text,",position='",input$position9,"'")
                if(input$palette9!="Reds") text=paste0(text,",palette='",input$palette9,"'")
                if(input$colour9!="black") text=paste0(text,",colour='",input$colour9,"'")
                if(input$size9!=0.2) text=paste0(text,",size=",input$size9)
                if(input$addlabel9==TRUE) text=paste0(text,",addlabel=TRUE")
                if(input$reverse9==TRUE) text=paste0(text,",reverse=TRUE")
                
                
                
        } else if(input$iplotmain=="ggHeatmap") {
                text=paste0("ggHeatmap(",input$mydata)
                text=paste0(text,input$mapping)
                # aestemp=c()
                # if(input$ix8!="None") aestemp=c(aestemp,paste0("x=",input$ix8))
                # if(input$iy8!="None") aestemp=c(aestemp,paste0("y=",input$iy8))
                # if(input$ifill8!="None") aestemp=c(aestemp,paste0("fill=",input$ifill8))
                # if(input$ifacet8!="None") aestemp=c(aestemp,paste0("facet=",input$ifacet8))
                # text=add_aes(text,aestemp)
                if(input$stat8!="count") text=paste0(text,",stat='",input$stat8,"'")
                if(input$palette8!="Blues") text=paste0(text,",palette='",input$palette8,"'")
                if(input$reverse8==TRUE) text=paste0(text,",reverse=TRUE")
                if(input$colour8!="grey50") text=paste0(text,",colour='",input$colour8,"'")
                if(input$size8!=0.1) text=paste0(text,",size=",input$size8)
                if(input$yangle8!=0) text=paste0(text,",yangle=",input$yangle8)
                if(input$addlabel8==TRUE) text=paste0(text,",addlabel=TRUE")
                if(input$polar8==TRUE) text=paste0(text,",polar=TRUE")
                
        } else if(input$iplotmain=="ggPoints") {
                text=paste0("ggPoints(",input$mydata)
                text=paste0(text,input$mapping)
                # aestemp=c()
                # if(input$ix7!="None") aestemp=c(aestemp,paste0("x=",input$ix7))
                # if(input$iy7!="None") aestemp=c(aestemp,paste0("y=",input$iy7))
                # if(input$icolour7!="None") aestemp=c(aestemp,paste0("color=",input$icolour7))
                # if(input$ifill7!="None") aestemp=c(aestemp,paste0("fill=",input$ifill7))
                # if(input$ifacet7!="None") aestemp=c(aestemp,paste0("facet=",input$ifacet7))
                # text=add_aes(text,aestemp)
                if(input$method7!="auto") text=paste0(text,",method='",input$method7,"'")
                if(input$formula7!="y~x") text=paste0(text,",formula=",input$formula7)
                if(input$level7!=0.95) text=paste0(text,",level=",input$level7)
                text=paste0(text,",se=",input$se7)
                text=paste0(text,",fullrange=",input$fullrange7)
                
                
        } else if(input$iplotmain=="ggRadar") {
                text=paste0("ggRadar(",input$mydata)
                text=paste0(text,input$mapping)
                # aestemp=c()
                # if(input$igroup4!="None") aestemp=c(aestemp,paste0("group=",input$igroup4))
                # if(length(input$ix4)>1) aestemp=c(aestemp,paste0("x=c(",Reduce(pastecomma,input$ix4),")"))
                # text=add_aes(text,aestemp)
                if(input$colour4!="red") text=paste0(text,",colour='",input$colour4,"'")
                if(input$rescale==FALSE) text=paste0(text,",rescale=FALSE")
                if(input$ylim4=="c(0,1)") text=paste0(text,",ylim=c(0,1)")
        } else if(input$iplotmain=="ggAncova"){
                # if(input$formula1!=""){
                #         text=paste0("ggAncova(",input$formula1,",",input$mydata)
                # } else{
                        text=paste0("ggAncova(",input$mydata)
                        text=paste0(text,input$mapping)
                        # aestemp=c()
                        # if(input$ix1!="None") aestemp=c(aestemp,paste0("x=",input$ix1))
                        # if(input$iy1!="None") aestemp=c(aestemp,paste0("y=",input$iy1))
                        # if(input$iA1!="None") aestemp=c(aestemp,paste0("color=",input$iA1))
                        # text=add_aes(text,aestemp)
                        if(input$ilabel1!="None") text=paste0(text,",label=",input$ilabel1)
                        if(input$idigits1!=1) text=paste0(text,",digits=",input$idigits1)
                #}
        }  else if(input$iplotmain=="ggCatepillar") {
                
                text=paste0("ggCatepillar(",input$mydata)
                text=paste0(text,input$mapping)
                # aestemp=c()
                # if(input$ix2!="None") aestemp=c(aestemp,paste0("x=",input$ix2))
                # if(input$iy2!="None") aestemp=c(aestemp,paste0("y=",input$iy2))
                # if(input$igroup2!="None") aestemp=c(aestemp,paste0("group=",input$igroup2))
                # text=add_aes(text,aestemp)
                
                if(input$idigits2!=1) text=paste0(text,",digits=",input$idigits2)
        } else if(input$iplotmain=="ggErrorBar") {
               
                        text=paste0("ggErrorBar(",input$mydata)
                        text=paste0(text,input$mapping)
                        # aestemp=c()
                        # if(input$ix10!="None") aestemp=c(aestemp,paste0("x=",input$ix10))
                        # if(input$iy10!="None") aestemp=c(aestemp,paste0("y=",input$iy10))
                        # if(input$igroup10!="None") aestemp=c(aestemp,paste0("group=",input$igroup10))
                        # text=add_aes(text,aestemp)
                        
                if(input$idigits10!=1) text=paste0(text,",digits=",input$idigits10)
        } else if(input$iplotmain=="ggDensity") {
                text=paste0("ggDensity(",input$mydata)
                text=paste0(text,input$mapping)
                # if(input$donuts!="None") text=paste0(text,",aes(donuts=",input$donuts,")")
                
                if(input$addhist==FALSE) text=paste0(text,",addhist=FALSE")
                
        } else if(input$iplotmain=="ggEffect") {
                text=paste0("ggEffect(",input$mydata)
                text=paste0(text,input$mapping)
                # aestemp=c()
                # if(input$ix3!="None") aestemp=c(aestemp,paste0("x=",input$ix3))
                # if(input$iy3!="None") aestemp=c(aestemp,paste0("y=",input$iy3))
                # if(input$icolor3!="None") aestemp=c(aestemp,paste0("color=",input$icolor3))
                # text=add_aes(text,aestemp)
                if(input$probs!="c(0.1,0.5,0.9)") text=paste0(text,",probs=",input$probs)
        } else if(input$iplotmain=="ggPieDonut") {
                text=paste0("ggPieDonut(",input$mydata)
                text=paste0(text,input$mapping)
                # aestemp=c()
                # if(input$pies!="None") aestemp=c(aestemp,paste0("pies=",input$pies))
                # if(input$donuts2!="None") aestemp=c(aestemp,paste0("donuts=",input$donuts2))
                # text=add_aes(text,aestemp)
                if(input$count2!="None") text=paste0(text,",count='",input$count2,"'")
                if(input$labelposition2!=1) text=paste0(text,",labelposition=",input$labelposition2)
        }  else if(input$iplotmain=="ggChoropleth") {
                text=paste0("ggChoropleth(",input$mydata,",aes(map_id=",input$map_id)
                if(length(input$ifillfill)>0) text=paste0(text,paste0(",fill=c(",Reduce(pastecomma,input$ifillfill),")"))
                text=paste0(text,")")
                if(input$map11!="none") text=paste0(text,",map=",input$map11)
                else text=past0(text,input$maptext)
                if(input$tooltip!="None") text=paste0(text,",tooltip='",input$tooltip,"'")
                if(input$palette11!="OrRd") text=paste0(text,",palette='",input$palette11,"'")
                if(input$reverse11==TRUE) text=paste0(text,",reverse=TRUE")
                if(input$color11!="grey50") text=paste0(text,",color='",input$color11,"'")
                if(input$title11!="") text=paste0(text,",title=",input$title11)
                
        }  else if(input$iplotmain=="ggPair") {
                text=paste0("ggPair(",input$mydata)
                text=paste0(text,input$mapping)
                # aestemp=c()
                # if(length(input$ix12)>1) aestemp=c(aestemp,paste0("x=c(",Reduce(pastecomma,input$ix12),")"))
                # if(input$icolor12!="None") aestemp=c(aestemp,paste0("color=",input$icolor12))
                # text=add_aes(text,aestemp)
                if(input$ihorizontal12==TRUE) text=paste0(text,",horizontal=TRUE")
                
                
        } else if(input$iplotmain=="ggBoxplot") {
                text=paste0("ggBoxplot(",input$mydata)
                text=paste0(text,input$mapping)
                # aestemp=c()
                # if(length(input$ix16)>1) aestemp=c(aestemp,paste0("x=c(",Reduce(pastecomma,input$ix16),")"))
                # if(input$icolor16!="None") aestemp=c(aestemp,paste0("color=",input$icolor16))
                # text=add_aes(text,aestemp)
                if(input$ihorizontal16==TRUE) text=paste0(text,",horizontal=TRUE")
                if(input$irescale16==TRUE) text=paste0(text,",rescale=TRUE")
                
                
        }else if(input$iplotmain=="ggCLE") {
                text=paste0("ggCLE(",input$mydata)
                text=paste0(text,input$mapping)
                # aestemp=c()
                # if(input$ix13!="None") aestemp=c(aestemp,paste0("x=",input$ix13))
                # if(input$iy13!="None") aestemp=c(aestemp,paste0("y=",input$iy13))
                # if(input$icolor13!="None") aestemp=c(aestemp,paste0("color=",input$icolor13))
                # if(input$ifacet13!="None") aestemp=c(aestemp,paste0("facet=",input$ifacet13))
                # text=add_aes(text,aestemp)
                if(input$ino13!=0) text=paste0(text,",no=",input$ino13)
                if(input$ireorder13==FALSE) text=paste0(text,",reorderByX=FALSE")
                if(input$idecreasing13==FALSE) text=paste0(text,",decreasing=FALSE")
                
                
        } else if(input$iplotmain=="ggCor") {
                text=paste0("ggCor(",input$mydata)
                if(input$ialternative14!="two.sided") text=paste0(text,",alternative='",input$ialternative14,"'")
                if(input$imethod14!="pearson") text=paste0(text,",method='",input$imethod14,"'")
                if(input$ici14!=0.95) text=paste0(text,",conf.level=",input$ici14)
                if(input$ilabel14!="0") text=paste0(text,",label=",input$ilabel14)
                
                
                
        } else if(input$iplotmain=="ggDot") {
                text=paste0("ggDot(",input$mydata)
                text=paste0(text,input$mapping)
                # aestemp=c()
                # if(input$ix15!="None") aestemp=c(aestemp,paste0("x=",input$ix15))
                # if(input$iy15!="None") aestemp=c(aestemp,paste0("y=",input$iy15))
                # if(input$ifill15!="None") aestemp=c(aestemp,paste0("fill=",input$ifill15))
                # text=add_aes(text,aestemp)
                if(input$istackdir15!="center") text=paste0(text,",stackdir='",input$istackdir15,"'")
                if(input$ibinaxis15!="x") text=paste0(text,",binaxis='",input$ibinaxis15,"'")
                if(input$imethod15!="dotdensity") text=paste0(text,",method='",input$imethod15,"'")
                if(input$iboxfill15!="NULL") text=paste0(text,",boxfill='",input$iboxfill15,"'")
                if(input$iposition15!=0.2) text=paste0(text,",position=",input$iposition15)
                if(input$iboxwidth15!=0.25) text=paste0(text,",boxwidth=",input$iboxwidth15)
                if(input$ibinwidth15!=0.5) text=paste0(text,",binwidth=",input$ibinwidth15)

                
                
        }else{
                temp=""
                for(i in 1:length(selections)){
                        if(input[[selections[i]]]!="None") {
                                temp=paste0(temp,",",varnames[i],"='",input[[selections[i]]],"'")
                        }
                }
                text=paste0("ggplot(",input$mydata,temp)
        }       
        #text=paste0(text,")")
        
        if(input$doPreprocessing) {
                text=paste(input$preprocessing,"\n",text)
        }
        text=paste0(text,")")
        text
})

pastecomma=function(...){
                paste(...,sep=",")
}    

add_aes=function(text,aestemp){
        if(length(aestemp)>0){
                temp=Reduce(pastecomma,aestemp)
                text=paste0(text,",aes(",temp,")")
        }
        text
}

observeEvent(input$Reset,{
        mydf=df()
        
        ggchoice=c("None",colnames(mydf))
        for(i in 1:length(selections))
                updateSelectInput(session,selections[i],choices=c(ggchoice,1))
        for(i in 1:length(multiselections))
                updateSelectInput(session,multiselections[i],choices=ContinuousVar(mydf))
        #updateSelectInput(session,"iplotmain",selected="ggplot")
        
})

make_interactive_mapping=reactive({
        text=""
        aestemp=c()
        if(length(input$ixx)>0){
                temp=Reduce(pastecomma,input$ixx)
                temp=setdiff(temp,"None")
                if(length(input$ixx)==1){
                        aestemp=c(aestemp,paste0("x=",temp))
                } else{
                        aestemp=c(aestemp,paste0("x=c(",temp,")"))
                }
        }
        if(input$ix1!="None") aestemp=c(aestemp,paste0("x=",input$ix1))
        if(input$iyy!="None") aestemp=c(aestemp,paste0("y=",input$iyy))
        if(input$icolorcolor!="None") aestemp=c(aestemp,paste0("color=",input$icolorcolor))
        if(input$ifill1!="None") aestemp=c(aestemp,paste0("fill=",input$ifill1))
        if(length(input$ifillfill)>0){
                temp=Reduce(pastecomma,input$ifillfill)
                temp=setdiff(temp,"None")
                if(length(input$ifillfill)==1){
                        aestemp=c(aestemp,paste0("fill=",temp))
                } else{
                        aestemp=c(aestemp,paste0("fill=c(",temp,")"))
                }
        }
        if(input$igroupgroup!="None") aestemp=c(aestemp,paste0("group=",input$igroupgroup))
        if(input$ifacetfacet!="None") aestemp=c(aestemp,paste0("facet=",input$ifacetfacet))
        if(input$pies!="None") aestemp=c(aestemp,paste0("pies=",input$pies))
        if(input$donuts!="None") aestemp=c(aestemp,paste0("donuts=",input$donuts))
        text=add_aes(text,aestemp)
})


observe({
        updateTextInput(session,"mapping",value=make_interactive_mapping())        
        interactivemode=1
        if(input$iplotmain %in% c("ggPair","ggRadar","ggBoxplot")) interactivemode=2
        updateTextInput(session,"interactivemode",value=interactivemode)        
})

output$iplots=renderPlot({

       
  input$makeIplot
        
        
           text=makePlotCode()
           print(text)
           printiplot()
       

})


output$iploti=renderggiraph({

 
        
                # text=paste0(makePlotCode(),",interactive=TRUE)")
                # print(text)
            p<-NULL
                try(p<-eval(parse(text=input$iplotcode)))
        if(!is.null(p)){
                
                hover_css="r:6px;cursor:pointer;stroke:gold;stroke-width:4px;"
                tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:20px 20px 20px 20px;"
                
                # if(input$iplotmain %in% c("ggChoropleth","ggSpine","ggBar","ggRose","ggBidirectionalBar",
                #                                  "PopPyramid","ggDonut","ggPieDonut")){
                #         p<-ggiraph(code=print(p),zoom_max=10,tooltip_extra_css = tooltip_css)
                # }
                # else 
                p<-ggiraph(code=print(p),zoom_max=10,tooltip_extra_css = tooltip_css,hover_css=hover_css)
                p
        }


})

printiplot=function(){
        p<-eval(parse(text=input$iplotcode))
        print(p)
}

output$downloadPlot4 = downloadHandler(
        filename="Plot.png",
        content=function(file){
                
                # temporarily switch to the temp dir, in case you do not have write
                # permission to the current working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                p<-eval(parse(text=input$iplotcode))
                ggsave(file,p,width=input$plotWidth,height=input$plotHeight,units=input$plotUnit,dpi=input$plotRes)
                #plotPNG(plotlogPlot,filename=file,width=input$plotWidth,height=input$plotHeight,units=input$plotUnit,
                #        res=input$plotRes)
                #try(eval(parse(text=input$preprocessing)))
                #try(p<-eval(parse(text=paste(input$rorder))))
                #if(!is.null(p)) ggsave(file,p,width=input$plotWidth,height=input$plotHeight,
                #units=input$plotUnit,dpi=input$plotRes)
                
                
        },
        contentType="image/png"
)
output$downloadPDF4 = downloadHandler(
        filename="myplot.pdf",
        content=function(file){
                
                # temporarily switch to the temp dir, in case you do not have write
                # permission to the current working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                p<-eval(parse(text=input$iplotcode))
                ggsave(file,p,width=input$plotWidth,device=cairo_pdf,height=input$plotHeight,units=input$plotUnit,dpi=input$plotRes)
                #plotPNG(plotlogPlot,filename=file,width=input$plotWidth,height=input$plotHeight,units=input$plotUnit,
                #        res=input$plotRes)
                #embed_fonts(file)
                #eval(parse(text=input$preprocessing))
                #try(p<-eval(parse(text=paste(input$rorder))))
                #if(!is.null(p)) ggsave(file,p,width=input$plotWidth,height=input$plotHeight,
                #                       units=input$plotUnit,dpi=input$plotRes)
                
        },
        contentType="application/pdf"
)


output$downloadPPT4 = downloadHandler(
       
        filename="interactive.pptx",
        content=function(file){
                
                
                # temporarily switch to the temp dir, in case you do not have write
                # permission to the current working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                
                p<-eval(parse(text=input$iplotcode))
                plot2pptx(p,target=file)
               
                
                #zip(zipfile=file,files=c("ggplot.pptx"))
        },
        contentType="application/vnd-ms-powerpoint"
)



output$downloadHTML = downloadHandler(
        
        filename="interactive.html",
        content=function(file){
                
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                
                p<-eval(parse(text=input$iplotcode))
                
                hover_css="r:6px;cursor:pointer;stroke:gold;stroke-width:4px;"
                tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:20px 20px 20px 20px;"
                
                # if(input$iplotmain=="ggPoints") {
                #         p<-as.widget(ggplotly(p))
                # } else 
                # if(input$iplotmain %in% c("ggChoropleth","ggSpine","ggBar","ggRose","ggBidirectionalBar",
                #                           "PopPyramid","ggDonut","ggScatter","ggPieDonut")){
                #         p<-ggiraph(code=print(p),tooltip_extra_css = tooltip_css)
                # }
                # else 
                p<-ggiraph(code=print(p),tooltip_extra_css = tooltip_css,hover_css=hover_css)
                
                htmlwidgets::saveWidget(p,file=file,selfcontained = TRUE)
                #zip(zipfile=file,files=c("ggplot.pptx"))
        },
        contentType="text/html"
)

observeEvent(input$saveToMultiplot4,  {
        
        if(input$plotinset1=="") updateTextInput(session,'plotinset1',value=input$iplotcode)
        else if(input$plotinset2=="") updateTextInput(session,'plotinset2',value=input$iplotcode)
        else if(input$plotinset3=="") updateTextInput(session,'plotinset3',value=input$iplotcode)
        else if(input$plotinset4=="") updateTextInput(session,'plotinset4',value=input$iplotcode)
        updateCheckboxInput(session,"doMultiplot",value=TRUE)
        
})

observeEvent(input$saveToPPTList4,  {
        
        mylist=paste0("ppt",1:10)
        
        for(i in 1:10) {
                if(input[[mylist[i]]]==""){
                        updateTextInput(session,mylist[i],value=input$iplotcode)
                        break
                }
        }
        
})

makeHTML=reactive({
        
        result=0
        text=paste0(makePlotCode(),",interactive=TRUE)")
        p<-NULL
        try(p<-eval(parse(text=text)))
        
        if(!is.null(p)) {
                htmlwidgets::saveWidget(p,"temp.html")
                result=1
        }
        result        
})

output$testHTML=renderPrint({
    
    
                text=paste0(makePlotCode(),",interactive=TRUE)")
                #print(text)
                # p<-NULL
                # try(p<-eval(parse(text=text)))
                # if(!is.null(p)){
                #      htmlwidgets::saveWidget(p,"temp.html")
                #      includeHTML("temp.html")
                # }
                p<-eval(parse(text=text))
                saveWidget(p,"temp.html")
                includeHTML("temp.html")
     
      
})

observe({

        if(input$main=="Interactive plot"){
                if(input$iplotEx!="None") {
                        updateTextInput(session,"mydata",value=input$iplotEx)
                        data()
                        
                } 
                
                text=makePlotCode()
                #print(text)
                p<-NULL
                try(p<-eval(parse(text=text)))
                # if((input$iplotmain=="ggPoints") &(!is.null(p))){ 
                #         updateTextInput(session,"plotlyok",value=1)
                # } else{
                #         updateTextInput(session,"plotlyok",value=0)
                # }
        }
})

output$plotlyplot=renderPlotly({
        
        
               p<-eval(parse(text=input$iplotcode))
               p<-ggplotly(p)
               p
       
      
        
})


output$downloadReport <- downloadHandler(
        filename = function() {
                paste('interactivePlot', sep = '.','html')
        },
        
        content = function(file) {
                src <- normalizePath('inplot.Rmd')
                
                # temporarily switch to the temp dir, in case you do not have write
                # permission to the current working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                file.copy(src, 'report.Rmd')
                
                out <- render('report.Rmd', html_document())
                file.rename(out, file)
        }
)

output$ggplotEx <- renderggiraph({
        
        df=readRDS("ggplotEx.RDS")
        df
        if(input$language!="kor") df<-df[1:42,]
        ncol=7
        total=nrow(df)
        df$data_id=1:total
        df$xmax=df$data_id%%ncol
        df$xmax[df$xmax==0]=ncol
        df$xmin=df$xmax-1
        df$ymin=(total- df$data_id)%/%ncol
        df$ymax=df$ymin+1
        df$data_id=as.character(df$data_id)
        
        tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:20px 20px 20px 20px;"
        
        p<-ggplot(df,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax))
        for(j in 1:total) {
                
                image=png::readPNG(paste0("thumb_",j,".png"))
                xmax=j%%ncol
                if(xmax==0) xmax=ncol
                xmin=xmax-1
                (ymin=(total-j)%/%ncol)
                ymax=ymin+1
                p<-p+annotation_raster(image,xmin,xmax,ymin,ymax,interpolate=FALSE)
        }        
        p<-p+geom_rect_interactive(aes(tooltip=tooltip,data_id=data_id),
                                   colour="white",size=0.1,alpha=0.1)
        
        p<-p+coord_fixed()+theme_clean()+xlim(0,7)+ylim(0,7)
        
        ggiraph(code=print(p),selection_type = "single",
                tooltip_extra_css = tooltip_css,
                hover_css="cursor:pointer;stroke:gold;stroke-width:2px;")
        
})

selected_ggplot <- reactive({
        input$ggplotEx_selected
})

observeEvent(selected_ggplot(), {
        # updateNumericInput(session, "choice", value = as.numeric(selected_plot()) )
        # updateTextInput(session, "Preprocessing", value = data$Preprocessing[as.numeric(selected_plot())] )
        # updateTextInput(session, "Rcode", value = data$Code[as.numeric(selected_plot())] )
        data=readRDS("ggplotEx.RDS")
        updateSelectInput(session, "example", selected = data$no[as.numeric(selected_ggplot())] )
        
})
})