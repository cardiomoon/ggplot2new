require(shiny)
require(markdown)
require(shinythemes)
require(RColorBrewer)
require(extrafont)
require(ggiraph)
require(plotly)
#library(rhandsontable)

source("textInput2.R")

#loadfonts(device="postscript")

insetchoices=c("full"=0,"upper"=6,"lower"=5,"right"=8,"left"=7,"RUQ"=4,
               "RLQ"=2,"LUQ"=3,"LLQ"=1,"custom"=9,
               "centers"=10,"uppers"=16,"lowers"=15,"rights"=18,"lefts"=17,"RUQs"=14,
               "RLQs"=12,"LUQs"=13,"LLQs"=11)
popdata=readRDS("data/level1.RDS")
fillchoice=names(popdata)[17:36]
areachoices=c("전국"=0,"서울특별시"=11,"부산광역시"=21,"대구광역시"=22,"인천광역시"=23,
              "광주광역시"=24,"대전광역시"=25,"울산광역시"=26,"경기도"=31,"강원도"=32,"충청북도"=33,
              "충청남도"=34,"전라북도"=35,"전라남도"=36,"경상북도"=37,"경상남도"=38,"제주특별자치도"=39)

levelchoices=c("시도"=1,"시군구"=2,"읍면동"=3)

iwidth=140

shinyUI(fluidPage(
  
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: green;
      }
    "))
  ),
  
  div(id="Title1",h1("Web-R.org: Learn ggplot2")),
  hr(),
  div(id="introduction","With this app, you can learn `ggplot2`. Please wait a minute ! This message is disappeared WHEN READY ! "),
  hr(),
  radioButtons(inputId = "language", label = "Select Language",
               choices = list("English" = "en", "한국어(Korean)" = "kor"),
               selected = "en",inline=TRUE),
  singleton(
          tags$head(tags$script(src = "message-handler.js"))
  ),
  navbarPage( "Web-R.org",
              tabPanel("ggplot2",
                       fluidPage(
                         fluidRow(
                                 div(id="intro1"),
                           column(4,wellPanel(
                             fileInput("file", div(id="file")),
                             
                             
                             radioButtons("radio", label = "Select data",
                                          choices = list("Salaries", "mtcars","acs","radial","iris","heightweight"),
                                          selected = "Salaries"),
                             hr(),
                             div(id="intro2"),
                             actionButton("resetVar","Reset Variables/Options"),
                             checkboxInput("showDataHelp","show help for data",value=FALSE)
                             
                           )),
                           column(8,wellPanel(
                                   fluidRow(
                                           column(4,checkboxInput('doPreprocessing',"do Preprocessing",value=FALSE,width=150)),
                                           column(4,actionButton("clearPreprocessing","Reset preprocessing"))),
                                  textareaInput('preprocessing',
                                                label='You can preprocess the data by entering the R command(s) here. Please uncheck the checkbox before enter/change the R command(s) and recheck the checkbox. ',
                                                rows=7,width=510),
                                  textInput2('mydata','Enter the name of data',width=510),
                                  
                                 
                                  selectInput("PreProcessingExample","Select data preprocessing",
                                               c("None"=0,"열 이름 바꾸기"=4,
                                                 "범주형 변수로 바꾸기"=7,
                                                 "열 추가하기/ 삭제하기"=5,
                                                 "자료 정렬/필터"=6,
                                                 "연속형변수를 k개의 구간으로 나누기"=8,
                                                 "평균과 신뢰구간으로 데이타 요약하기"=1,
                                                 "넓은 데이타 프레임을 길게 변환하기"=2,
                                                 "긴 데이타 프레임을 넓게 변환하기"=3),width=510),
                                         actionButton("goback",div(id="goback")),
                                         actionButton("gohome",div(id="gohome")),
                                  hr(),
                                  selectInput("example","Select Example",
                                              c("None"=0,"Diamonds Are Forever"=21,
                                                "성악가의 키(1)"=14,"성악가의 키(2)"=15,"성악가의 키(3)"=16,
                                                "대학 교수의 연봉(1)"=11,"대학 교수의 연봉(2)"=12,"대학 교수의 연봉(3)"=13,
                                                "대학 교수의 연봉(4)"=17,
                                                "전국 시도 인구분포도"=18,
                                                "동별 서울 인구분포도"=19,
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
                                                "geom_count예제*"=53),selected=0,width=510),
                                  conditionalPanel(
                                    condition="input.example==35",
                                    fluidRow(
                                      column(12, 
                                        selectInput3("maparea","지역선택",choices=areachoices,selected=0,selectize=TRUE,width=150),
                                        selectInput3("maplevel","지도경계선택",choices=levelchoices,selected=1,selectize=TRUE,width=150),
                                        selectInput3("mapselect","통계항목선택",choices=fillchoice,selected="총인구_명",selectize=TRUE,width=150)
                                      )
                                    )
                                  ),
                               
                                  conditionalPanel(
                                          condition="input.PreProcessingExample==6",
                                          fluidRow(
                                                  column(12,wellPanel(  
                                                          selectInput3("sortvar1","sort by column1",c("None"),selectize=TRUE,multiple=TRUE,width=200),
                                                          selectInput3("sortvar2","sort by column2",c("None"),selectize=TRUE,multiple=TRUE,width=200),
                                                          actionButton("sort","Sort by column(s)"),
                                                          br(),
                                                          checkboxInput3("desc1","descending order",value=FALSE,width=200),
                                                          checkboxInput3("desc2","descending order",value=FALSE,width=200),
                                                          br(),
                                                          br(),
                                                          textInput3("filtercondition","Filtering condition",value="",width=400),
                                                          actionButton("filter","Apply Filter"),
                                                          br(),
                                                          p("Ex : Sepal.Length >6"),
                                                          br(),
                                                          br(),
                                                          selectInput3("selectcol","Columns to be selected",c("None"),selectize=TRUE,multiple=TRUE,width=200),
                                                          actionButton("select","Select column(s)")
                                                          
                                                          
                                                          
                                                  ))
                                          )
                                  ),
                                  conditionalPanel(
                                          condition="input.PreProcessingExample==5",
                                          fluidRow(
                                                  column(12,wellPanel(  
                                                          selectInput3("delcolname","Select column(s) to delete",c("None"),multiple=TRUE,selectize=TRUE,width=150),
                                                          actionButton("delcol","Delete Column(s)"),
                                                          br(),
                                                          textInput3("colnameadd","Enter the name of row number column",value="id",width=150),
                                                          actionButton("addcol","Add row number column"),
                                                          br(),
                                                          textInput3("addcolorder","Enter the compute equation",value="",width=300),
                                                          actionButton("addcol2","Compute New Column"),
                                                          br(),
                                                          div(id="intro4")
                                                  ))
                                          )
                                  ),
                                  conditionalPanel(
                                          condition="input.PreProcessingExample==4",
                                          fluidRow(
                                                  column(8,wellPanel(  
                                                          selectInput("oldcolname","Select column name to rename",c("None"),selectize=TRUE),
                                                          textInput("newcolname","Enter new column name",value="")
                                                  )),  
                                                  actionButton("changecolname","Change column name")
                                          )
                                  ),
                                  conditionalPanel(
                                          condition="input.PreProcessingExample==8",
                                          fluidRow(
                                                  column(8,wellPanel(
                                                          # p("연속형 변수를 기준으로 순위를 정하고 임의의 k개의 구간으로 나누어줍니다.
                                                          #   예를 들어 age를 기준으로 4개의 그룹으로 나누어 새로운 변수 agegroup에 저장하고 싶은 경우
                                                          #   기준변수에 age를 선택하고 구간수에 4를 입력하고 새로운 변수 이름에 agegroup을 입력후 
                                                          #   'k개의 구간으로 나누기'를 선택하세요."),
                                                          div(id="intro3"),          
                                                          selectInput("base","Select the criterion variable",c("None"),selectize=TRUE),
                                                          textInput("rankname","Enter the new rank name",value=""),
                                                          numericInput("rankk","How many groups to be created?",min=2,max=10,value=4)
                                                  )),  
                                                  actionButton("rankcol","Grouping into k-groups")
                                          )
                                  ),
                                  conditionalPanel(
                                          condition="input.PreProcessingExample==7",
                                          fluidRow(
                                                  column(8,wellPanel(  
                                                          selectInput("oldfname","Select column name to encode as a factor",c("None"),selectize=TRUE),
                                                          textInput("newfname","Enter new name(if omitted, use current name)",value=""),
                                                          selectInput("relevel","Specify the order(if omitted, alphabetical)",c("None"),multiple=TRUE,selectize=TRUE)
                                                          
                                                  )),  
                                                  actionButton("makefactor","Encode as a factor")
                                          )
                                  ),
                                  conditionalPanel(
                                          condition="input.PreProcessingExample==3",
                                          fluidRow(
                                                  column(8,wellPanel(  
                                                          selectInput("id.var2","select id var(iable)(s)",c("None"),multiple=TRUE,selectize=TRUE),
                                                          selectInput("measure.vars2","names of column to be moved",c("."),selectize=TRUE,multiple=TRUE),
                                                          selectInput("value.var2","name of column which stores values",c("None"),selectize=TRUE)
                                                  )),  
                                                  actionButton("long2wide","Transfor to wide form")
                                          )
                                  ),
                                  conditionalPanel(
                                          condition="input.PreProcessingExample==2",
                                          fluidRow(
                                                  column(8,wellPanel(  
                                                          selectInput("id.var","select id var(iable)(s)","",selectize=TRUE,multiple=TRUE),
                                                          selectInput("measure.vars","select measure vars(if blank, will use all non id vars)","",selectize=TRUE,multiple=TRUE),
                                                          textInput("variable.name","Name of variable used to store measured variable names",value="variable"),
                                                          textInput("value.name","Name of variable used to store values",value="value")
                                                          
                                                  )),  
                                                  
                                                  actionButton("wide2long","Transform to long form")
                                          )
                                  ),
                                  conditionalPanel(
                                          condition="input.PreProcessingExample==1",
                                          fluidRow(
                                                  column(8,wellPanel(  
                                                          selectInput("measurevar1","Select the variables to be summarized",c("None"),selectize=TRUE),
                                                          selectInput("groupvar1","Select the grouping variables",c("None"),multiple=TRUE,selectize=TRUE)
                                                          
                                                          
                                                  )),  
                                                  actionButton("doSummary","DO Summary")
                                          )
                                  )
                                  
                                  ))),
                         #verbatimTextOutput("test"),
                         conditionalPanel(condition="input.showDataHelp==true",
                                          h3("Help for Data"),
                                          htmlOutput("helpData"),
                                          hr()
                         ),
                         fluidRow(
                          column(12,
                         checkboxInput("browseExamples","Browse Examples",value = FALSE),
                         conditionalPanel(condition="input.browseExamples==true",
                                          ggiraphOutput("ggplotEx")
                                          
                         ))),
                         checkboxInput("showtable","always show data table",value= FALSE),
                         tableOutput("table"),
                         #conditionalPanel(condition="input.showtable==true",
                         # span(id="dbinfo","dbinfo"),
                         # a(id="update"," Update",href="#"),
                         # #a(id="missed","전체자료/누락된 자료보기"),
                         # rHandsontableOutput("hot",height=400),
                         # hr(),
                         # downloadButton("downloadMydf","download edited data"),
                         # hr(),
                         #),
                         #uiOutput("table.ui"),
                         uiOutput("plot.ui"),
                           
                         hr(),
                         fluidRow(
                          column(3,
                             div(id="assignvar"),
                             hr(),
                             selectInput("xvar","x축 변수 선택:",c("None"),selectize=TRUE,width=140),
                             checkboxInput("factorxvar","범주형변수로 취급",value=FALSE,width=140),
                             checkboxInput("reorderx","y축변수순 정렬",value=FALSE),
                             selectInput("yvar","y축변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             checkboxInput("reordery","x축변수순 정렬",value=FALSE),
                             selectInput("groupvar","group 변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput("colorvar","color 변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput("fillvar","fill 변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput("alphavar","alpha 변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput("linetypevar","linetype 변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput("sizevar","size 변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             checkboxInput("sizearea","크기 면적에 비례",value=FALSE,width=140),
                             numericInput("maxsize","maxsize",value=15,step=1,min=1,max=100,width=140),
                             selectInput("shapevar","shape 변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput("facetrow","가로로 면 분할:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput("facetcol","세로로 면 분할:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput("facetwrap","2차원 면분할:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput("labeller","면분할라벨:",c("label_value","label_both","label_context","label_parsed","label_wrap_gen"),selectize=TRUE,selected="label_value",width=140),
                             selectInput("facetswitch","면분할switch:",c("None","x","y","both"),selectize=TRUE,selected="None",width=140),
                             selectInput("facetscales","면분할 scales:",c("fixed","free_x","free_y","free"),selectize=TRUE,selected="fixed",width=140),
                             selectInput("facetspace","면분할 space:",c("fixed","free_x","free_y","free"),selectize=TRUE,selected="fixed",width=140),
                             numericInput("facetwrapncol","ncol",value=0,step=1,min=0,width=140),
                             checkboxInput("strip.bg.blank","면분할라벨배경제거",value=FALSE,width=140)
                             
                           ),
                          column(6,
                                 h4("Geometry Options"),
                                 wellPanel(
                             
                             checkboxInput("point","point",value=FALSE),
                             conditionalPanel(
                                condition="input.point==true",
                                wellPanel(  
                                 selectInput3('pointposition', 'position', 
                                                 choices = c("identity","stack","jitter","dodge(0.3)")), 
                                 selectInput3('pointcolor','color',c('NA',colors()),selected='NA'),
                                 selectInput3('pointfill','fill',c('NA',colors()),selected='NA'),
                                 br(),
                                  numericInput3('pointsize','size',value=2,width=100),
                                  numericInput3('pointshape','shape',value=19,width=100),
                                  numericInput3('pointalpha','alpha',value=1,step=0.1,width=100)
                                  )
                             ),
                             checkboxInput("count","count",value=FALSE),
                             conditionalPanel(
                               condition="input.count==true",
                               wellPanel(  
                                 selectInput3('countposition', 'position', 
                                              choices = c("identity","stack","jitter","dodge(0.3)")), 
                                 selectInput3('countcolor','color',c('NA',colors()),selected='NA'),
                                 selectInput3('countfill','fill',c('NA',colors()),selected='NA'),
                                 br(),
                                 #numericInput3('countsize','size',value=2,width=100),
                                 numericInput3('countshape','shape',value=19,width=100),
                                 numericInput3('countalpha','alpha',value=1,step=0.1,width=100)
                               )
                             ),
                             checkboxInput("text","text/label",value=FALSE),
                             conditionalPanel(
                               condition="input.text==true",
                               wellPanel( 
                                 checkboxInput('geom_label','use geom_label instead geom_text',value=FALSE),
                                 br(),
                                 selectInput3('textstat','stat',c("identity","bin","count"),selected="identity"),
                                 selectInput3('textposition','position',c("identity","stack","dodge","fill")),
                                 selectInput3('textcolor','colour',c("NA",colors()),selected="NA"),
                                 br(),
                                 textInput3('textdata','data',value=""),
                                 textInput3('textlabel', 'label',value="",width=200), 
                                 textInput3('textdodgeposition','dodgeposition',value=""),
                                 
                                 br(),
                                 textInput3('textx','x',value="",width=200),
                                 textInput3('texthjust','hjust',value="",width=100),
                                 textInput3('textnudge_x','nudge_x',value="0",width=100),
                                 numericInput3('textsize','size',value=5,step=1,min=1),
                                 br(),
                                 textInput3('texty','y',value="",width=200),
                                 textInput3('textvjust','vjust',value="",width=100),
                                 textInput3('textnudge_y','nudge_y',value="0",width=100),
                                 br(),
                                 checkboxInput('check_overlap','check_overlap',value=FALSE),
                                 checkboxInput('inherit.aes','inherit.aes',value=TRUE),
                                 textInput3('textother','...',value="",width=200)
                                 )
                             ),
                             checkboxInput("segment","segment",value=FALSE),
                             conditionalPanel(
                               condition="input.segment==true",
                               wellPanel(
                                 selectInput3('segmentcolor','colour',c("NA","segment","curve",colors()),selected="NA"),
#                                  conditionalPanel(
#                                    condition="input.segmentcolor=='Enter...'",
#                                    textInput3('segmentcolortext',"",value="segment")  
#                                  ), 
                                 selectInput3('segmentposition','position',c("identity","stack"),selected="identity"),
                                 selectInput3('segmentlineend','lineend',c('butt','round','square')),
                                 textInput3('segmentdata','data',value=""),
                                 textInput3('segmentx','x',value=""),
                                 textInput3('segmenty','y',value=""),
                                 textInput3('segmentxend','xend',value="0"),
                                 textInput3('segmentyend','yend',value="0"),
                                 numericInput3('segmentsize','size',value=1,step=.1),
                                 numericInput3('segmentalpha','alpha',value=1,step=0.1),
                                 selectInput3('segmentlinetype','linetye',
                                              c("NA"=-1,"blank"=0,"solid"=1,"dashed"=2,"dotted"=3,
                                                "dotdash"=4,"longdash"=5,"twodash"=6),selected=-1)
                                 
                               )   
                             ),
                             checkboxInput("curve","curve",value=FALSE),
                             conditionalPanel(
                               condition="input.curve==true",
                               wellPanel(
                                 selectInput3('curvecolor','colour',c("NA","curve","segment",colors()),selected="NA"),
                                 selectInput3('curveposition','position',c("identity","stack"),selected="identity"),
                                 selectInput3('curvelineend','lineend',c('butt','round','square')),
                                 textInput3('curvedata','data',value=""),
                                 textInput3('curvex','x',value=""),
                                 textInput3('curvey','y',value=""),
                                 textInput3('curvexend','xend',value="0"),
                                 textInput3('curveyend','yend',value="0"),
                                 numericInput3('curvature','curvature',value=0.5,step=.1),   
                                 numericInput3('angle','angle',value=90,min=0,max=180,step=1),   
                                 numericInput3('ncp','ncp',value=5,min=1,step=1),   
                                 numericInput3('curvesize','size',value=1,step=.1),
                                 numericInput3('curvealpha','alpha',value=1,step=0.1),
                                 selectInput3('curvelinetype','linetye',
                                              c("NA"=-1,"blank"=0,"solid"=1,"dashed"=2,"dotted"=3,
                                                "dotdash"=4,"longdash"=5,"twodash"=6),selected=-1)
                                 
                               )   
                             ),
                             checkboxInput("jitter","jitter",value=FALSE),
                             conditionalPanel(
                               condition="input.jitter==true",
                               wellPanel(
                                  numericInput3('jitterwidth','width',value=0.4,min=0,step=.1),   
                                  numericInput3('jitterheight','height',value=0,min=0,step=.1),   
                                  selectInput3('jittercolor','color',c('NA',colors()),selected='NA'),
                                  selectInput3('jitterfill','fill',c('NA',colors()),selected='NA'),
                                  numericInput3('jitteralpha','alpha',value=1,width=100),
                                  numericInput3('jittersize','size',value=2,width=100),
                                  numericInput3('jittershape','shape',value=16,width=100)
                               )   
                             ),
                             checkboxInput("line","line",value=FALSE),
                             conditionalPanel(
                               condition="input.line==true",
                               wellPanel(
                                 selectInput3('linestat','stat',c("identity","density")),
                                 selectInput3('linecolor','color',c("NA",colors())),
                                 selectInput3('lineposition','position',
                                              c("identity","stack","dodge(0.3)"),selected="NA"),
                                 selectInput3('linetype','linetye',
                                              c("NA"=-1,"blank"=0,"solid"=1,"dashed"=2,"dotted"=3,
                                                "dotdash"=4,"longdash"=5,"twodash"=6),selected=-1),
                                 numericInput3('lineadjust','adjust',value=1,min=.1,max=5,step=.1),
                                 numericInput3('linesize','size',value=0.5,step=.1)
                                 
                               )   
                               ),
                             checkboxInput("bar","bar",value=FALSE),
                             conditionalPanel(
                               condition="input.bar==true",
                               wellPanel(
                                 selectInput3('barstat','stat',c('bin','identity')),
                                 selectInput3('barposition', 'position', 
                                              choices = c("stack","dodge","fill","identity")), 
                                 selectInput3('barcolor','color',c('NA',colors()),selected='NA'),
                             
                                 selectInput3('barfill','fill',c('NA',colors()),selected='NA'),
                                 numericInput3('barwidth','width',value=.9,min=.1,max=1,step=.1),
                                 numericInput3('baralpha','alpha',value=1,min=0.1,max=1,step=0.1),
                                 numericInput3('barsize','size',value=0.5,step=0.1),
                                 textInput3('barother','...',value="",width=200),
                                 actionButton('barlabel','label 자동으로 추가')
                                 
                               )),   
                             checkboxInput("area","area",value=FALSE),
                             conditionalPanel( 
                               condition="input.area==true",
                               wellPanel(
                                 selectInput3('areastat','stat',c("identity","density")),
                                 selectInput3('areacolor','color',c("NA",colors())),
                                 selectInput3('areafill','fill',c("NA",colors())),
                          
                                 numericInput3('areasize','size',value=.5,step=.1),
                                 numericInput3('areaalpha','alpha',value=1,min=0.1,max=1,step=0.1)
                               )   
                               ),
                             checkboxInput("histogram","histogram",value=FALSE),
                             conditionalPanel( 
                               condition="input.histogram==true",
                               wellPanel(
                             
                             selectInput3('histpos','position',c("stack","identity","dodge","fill"),selected="stack"),
                             selectInput3('histcolor','color',c('NA',colors()),selected='NA'),
                             selectInput3('histfill','fill',c('NA','..count..',colors()),selected='NA'),
                         
                             numericInput3('histbinwidth','binwidth',value=0,min=0,step=1),
                             numericInput3('histsize','size',value=0.5,min=0,step=0.1),
                             numericInput3('histalpha','alpha',value=1,min=0.1,max=1,step=0.1)
                               )
                               ),
                             checkboxInput("density","density",value=FALSE),
                             conditionalPanel( 
                               condition="input.density==true",
                               wellPanel(
                                 selectInput3('densitycolor','color',c("None","NA",colors())),
                                 selectInput3('densityfill','fill',c("NA",colors())),
                                 selectInput3('densityposition','position',c("identity","stack")),
                      
                                 numericInput3('densitysize','size',value=0.5,step=.1),
                                 numericInput3('densityadjust','adjust',value=1,min=.1,max=5,step=.1),
                                 numericInput3('densityalpha','alpha',value=1,min=0.1,max=1,step=0.1)
                               )   
                             ),
                             checkboxInput("smooth","smooth",value=FALSE),
                             conditionalPanel( 
                               condition="input.smooth==true",
                               wellPanel(
                                 selectInput3("smoothmethod", label = "method",
                                              choices = list("loess", "lm","glm","rlm","gam"), 
                                              selected ="loess"),
                                 selectInput3("smoothformula", label = "formula",
                                              choices = list("y~x", "y~log(x)","y~sqrt(x)","y~poly(x,2)","y~poly(x,3)",
                                                             "y~ns(x,1)",y="ns(x,2)","y~ns(x,3)"), 
                                              selected ="y~x"),
                                 textInput3("smoothformula2","(직접입력)",value=""),
                                 selectInput3("smoothcolor","colour",c("NA",colors())),
                                 selectInput3('smoothlinetype','linetye',
                                              c("NA"=-1,"blank"=0,"solid"=1,"dashed"=2,"dotted"=3,
                                                "dotdash"=4,"longdash"=5,"twodash"=6),selected=-1),
                                 textInput3("smoothfamily","family",value=""),
                                 numericInput3("level","level",value=0.95,step=0.01),
                                 numericInput3("smoothsize","size",value=0.5,step=.1),
                                 checkboxInput3("se","show se",value=TRUE),
                                 checkboxInput3("fullrange","fullrange",value=FALSE)
                               )
                               ),
                             checkboxInput("boxplot","boxplot",value=FALSE),
                             conditionalPanel( 
                               condition="input.boxplot==true",
                               wellPanel(
                                 selectInput3('boxfill','fill',c("NA",colors())),
                                 selectInput3('boxcolor','color',c("NA",colors())),
                                 selectInput3('boxoutcolor','outlier color',c("None","NA",colors()),selected="None"),
                              
                                 numericInput3('boxwidth','width',value=1,min=.1,step=.1,width=60),
                                 numericInput3('boxoutsize','out.size',value=2,min=0.1,step=0.1,width=60),
                                 numericInput3('boxoutshape','out.shape',value=16,min=1,step=1,width=60),
                                 numericInput3('boxalpha','alpha',value=1,min=0.1,step=0.1,width=60),
                                 checkboxInput3('boxnotch','notch',value=FALSE,width=70)
                               )   
                               ),
                             checkboxInput("violin","violin",value=FALSE),
                             conditionalPanel( 
                               condition="input.violin==true",
                               wellPanel(
                                 selectInput3('violinfill','fill',c("NA",colors())),
                                 selectInput3('violincolor','color',c("NA",colors())),
                                 selectInput3('violinscale','scale',c("NA","count")),
                             
                                 numericInput3('violinadjust','adjust',value=1,min=.1,step=.1),
                                 numericInput3('violinalpha','alpha',value=1,min=.1,step=.1),
                                 checkboxInput3('violintrim','trim',value=TRUE)
                               )   
                               ),
                             checkboxInput("polygon","polygon",value=FALSE),
                             conditionalPanel( 
                               condition="input.polygon==true",
                               wellPanel(
                                 selectInput3('polygonfill','fill',c("NA",colors())),
                                 selectInput3('polygoncolor','color',c("NA",colors())),
                                 numericInput3('polygonsize','size',value=0.5,min=.1,step=.1),
                               
                                 numericInput3('polygonalpha','alpha',value=1,min=.1,step=.1)
                                 
                               )   
                             ),
                             checkboxInput("tile","tile/raster",value=FALSE),
                             conditionalPanel( 
                               condition="input.tile==true",
                               wellPanel(
                                 checkboxInput("raster","use raster instead of tile",value=TRUE),
                                 selectInput3('tilefill','fill',c("NA",colors())),
                                 selectInput3('tilecolor','color',c("NA",colors())),
                                 numericInput3('tilesize','size',value=0.1,min=.1,step=.1),
                                 numericInput3('tilealpha','alpha',value=1,min=.1,step=.1)
                                 
                               )   
                             ),
                             checkboxInput("path","path",value=FALSE),
                             conditionalPanel( 
                               condition="input.path==true",
                               wellPanel(
                                 
                                 selectInput3('pathcolor','color',c("NA",colors())),
                                 selectInput3('pathlinetype','linetye',
                                              c("NA"=-1,"blank"=0,"solid"=1,"dashed"=2,"dotted"=3,
                                                "dotdash"=4,"longdash"=5,"twodash"=6),selected=-1),
                                 numericInput3('pathsize','size',value=1,min=.1,step=.1),
                                 numericInput3('pathalpha','alpha',value=1,min=.1,step=.1)
                                 
                               )   
                             ),
                             checkboxInput("dotplot","dotplot",value=FALSE),
                             conditionalPanel( 
                               condition="input.dotplot==true",
                               wellPanel(
                                 selectInput3('dotmethod','method',c("dotdensity","histodot")),
                                 selectInput3('dotstackdir','stackdir',c("up","down","center","centerwhole")),
                                 selectInput3('dotbinpositions','binpositions',c("bygroup","all")),
                                 selectInput3('dotbinaxis','binaxis',c("x","y")),
                                 selectInput3('dotcolor','color',c("NA",colors())),
                                 selectInput3('dotfill','fill',c("NA",colors())),
                                 numericInput3('dotalpha','alpha',min=0,max=1,step=0.1,value=1),
                                 numericInput3('dotbinwidth','binwidth',min=0,max=10,step=0.25,value=0),
                                 numericInput3('dotstackratio','stackratio',value=1,min=.1,max=2,step=0.1)
                                 
                               )   
                             ),
                             checkboxInput("rug","geom_rug",value=FALSE),
                             checkboxInput("errorbar","errorbar",value=FALSE),
                             conditionalPanel( 
                               condition="input.errorbar==true",
                               wellPanel(
                                 actionButton("ebauto","sd, se 자동계산"),
                                 textInput3("ymin","ymin",value=""),
                                 textInput3("ymax","ymax",value=""),
                                 selectInput3('ebcolor','color',c("NA",colors())),
                                 numericInput3('ebCI','CI',value=0.95,min=0,max=1,step=0.01),
                                 numericInput3('ebpos','position',value=0,min=0,step=0.1),
                                 numericInput3('ebwidth','width',value=0.2,min=0,step=.1),
                                 numericInput3('ebsize','size',value=0.5,min=0,step=.1)
                                 
                               )   
                             ),
                             checkboxInput("hline","hline",value=FALSE),
                             conditionalPanel( 
                               condition="input.hline==true",
                               wellPanel(
                                 textInput3('hlineintercept','yintercept',value=""),
                                 selectInput3('hlinecolor','colour',c("NA",colors())),
                                 selectInput3('hlinetype','linetye',
                                              c("NA"=-1,"blank"=0,"solid"=1,"dashed"=2,"dotted"=3,
                                                "dotdash"=4,"longdash"=5,"twodash"=6),selected=-1),
                                 numericInput3('hlinesize','size',value=0.5,min=.1,step=.1)
                                 
                               )   
                             ),
                             checkboxInput("vline","vline",value=FALSE),
                             conditionalPanel( 
                               condition="input.vline==true",
                               wellPanel(
                                 textInput3('vlineintercept','xintercept',value=""),
                                 selectInput3('vlinecolor','colour',c("NA",colors())),
                                 selectInput3('vlinetype','linetye',
                                              c("NA"=-1,"blank"=0,"solid"=1,"dashed"=2,"dotted"=3,
                                                "dotdash"=4,"longdash"=5,"twodash"=6),selected=-1),
                                 numericInput3('vlinesize','size',value=0.5,min=.1,step=.1)
                                 
                               )   
                             ),
                             checkboxInput("abline","abline",value=FALSE),
                             conditionalPanel( 
                               condition="input.abline==true",
                               wellPanel(
                                 textInput3('ablineintercept','intercept',value=""),
                                 textInput3('ablineslope','slope',value=""),
                                 selectInput3('ablinecolor','colour',c("NA",colors())),
                                 selectInput3('ablinetype','linetye',
                                              c("NA"=-1,"blank"=0,"solid"=1,"dashed"=2,"dotted"=3,
                                                "dotdash"=4,"longdash"=5,"twodash"=6),selected=-1),
                                 numericInput3('ablinesize','size',value=0.5,min=.1,step=.1)
                                 
                               )   
                             ),
                             checkboxInput("statsummary","stat_summary",value=FALSE),
                             conditionalPanel( 
                               condition="input.statsummary==true",
                               wellPanel(
                                 selectInput3('sumfill','fill',c("NA",colors())),
                                 selectInput3('sumcolor','color',c("NA",colors())),
                                 selectInput3('sumgeom','geom',c("NA","point"),selected="point"),
                                 selectInput3('sumfuny','fun.y',c("mean","median")),
                                 numericInput3('sumshape','shape',value=21,min=1,step=1),
                                 numericInput3('sumsize','size',value=2,min=.1,step=.1)
                                 
                               )   
                               ),
                               checkboxInput("statdensity2d","stat_density2d",value=FALSE),
                             conditionalPanel( 
                               condition="input.statdensity2d==true",
                               wellPanel(
                                 selectInput3('tdgeom','geom',c("density2d","tile","raster","polygon")),
                                 selectInput3('tdcolor','color',c("NA","..level..",colors())),
                                 selectInput3('tdfill','fill',c("NA","..density..","..level..",colors())),
                                 textInput3('tdsize','size',value="0.5"),
                                 textInput3('tdbins','bins',value=""),
                                 textInput3('tdalpha','alpha',value="1"),
                                 textInput3('tdh','h',value=""),
                                 checkboxInput3("tdcontour","contour",value=TRUE)
                               )   
                             ),
                               checkboxInput("statcontour","stat_contour",value=FALSE),
                             conditionalPanel( 
                               condition="input.statcontour==true",
                               wellPanel(
                                 selectInput3('contourzvar','z 변수선택',c("None")),
                                 selectInput3('contourgeom','geom',c("path","polygon")),
                                 selectInput3('contourcolor','color',c("NA","..level..",colors())),
                                 numericInput3('contourbinwidth','binwidth',value=0,min=1,step=1),
                                 numericInput3('contoursize','size',value=0.5,min=0,step=.1)
                               )   
                             ),
                             checkboxInput("annotate","annotate",value=FALSE),
                             conditionalPanel( 
                               condition="input.annotate==true",
                               wellPanel(
                                 
                                 selectInput3('anngeom','geom',c("text","rect","segment","pointrange"),selected="text"),
                                 selectInput3('anncolor','color',c("NA",colors())),
                                 selectInput3('annfill','fill',c("NA",colors())),
                                 numericInput3('annsize','size',value=5,min=0,step=.1),
                                 textInput3('annx','x',value=""),
                                 textInput3('annxend','xend',value=""),
                                 textInput3('anny','y',value=""),
                                 textInput3('annyend','yend',value=""),
                                 numericInput3('annalpha','alpha',value=1,min=0,step=.1),
                                 textInput3('annhjust','hjust',value=""),
                                 textInput3('annvjust','vjust',value=""),
                                 textInput3('annarrow','arrow',value="",width=200),
                                 conditionalPanel( 
                                   condition="input.anngeom=='text'",
                                   textInput3('annlabel','label',value="",width=200),  
                                   checkboxInput3('annlabeltext','as text',value=TRUE),
                                   actionButton('reglabel','회귀식추가'),
                                   br(),
                                   selectInput3('annfamily','family',
                                                choices=c("NA","Helvetica","Times","Courier",fonts()),selected="NA"),
                                   selectInput3('annfontface','fontface',
                                                choices=c("plain","bold","italic","bold.italic")),
                                   checkboxInput3('annparse','PARSE',value=FALSE)
                                   
                                 )   
                                 
                               )   
                             ),
                             checkboxInput("anncustom","annotation_custom",value=FALSE),
                             conditionalPanel( 
                               condition="input.anncustom==true",
                               wellPanel(
                                 
                                 textInput3('grob','grob',value="",width=200),
                                 textInput3('acxmin','xmin',value="-Inf"),
                                 textInput3('acxmax','xmax',value="Inf"),
                                 textInput3('acymin','ymin',value="-Inf"),
                                 textInput3('acymax','ymax',value="Inf"),
                                 actionButton('regtable','회귀결과 표 추가')
                                 
                               )   
                             ),
                             checkboxInput("map","map",value=FALSE),
                             conditionalPanel( 
                               condition="input.map==true",
                               wellPanel(
                                 
                                 textInput3('mapid','map_id',value=""),
                                 textInput3('mapmap','map',value=""),
                                 numericInput3('mapsize','size',value=0.5,min=.1,step=.1),
                                 numericInput3('mapalpha','alpha',value=1,min=.1,step=.1),
                                 selectInput3('mapfill','fill',c("NA",colors())),
                                 selectInput3('mapcolor','color',c("NA",colors()))
                                 
                               )   
                             ),
                             checkboxInput3("qmap","qmap",value=FALSE),
                             textInput2('location','location',value="",width=200),
                             conditionalPanel( 
                               condition="input.qmap==true",
                               wellPanel(
                                 
                                 textInput3('lon','lon',value="",width=100),
                                 textInput3('lat','lat',value="",width=100),
                                 numericInput3('zoom','zoom',value=10,min=3,max=21,step=1),
                                 selectInput3('maptype','maptype',
                                              c("terrain","satellite","roadmap","hybrid","toner",
                                                "watercolor")),
                                 selectInput3('source','source',c("google","osm","stamen","cloudmade"),
                                              selected="google"),
                                 selectInput3('extent','extent',c("device","normal","panel"),
                                              selected="device"),
                                 selectInput3('legend','legend',
                                              c("left","right","top","bottom","topleft","topright",
                                                "bottomleft","bottomright","none"),selected="right")
                               )   
                             ),
                               checkboxInput("addrorder","ggplot2 명령어 추가",value=FALSE),
                               textareaInput('rordertext',
                                             #'추가할 ggplot2 명령어를 입력하고 체크박스를 체크하세요.\n명령어를 고칠때는 체크박스를 해제하고 고친후 다시 체크하세요',
                                             'Uncheck the add ggplot2 code checkbox, edit ggplot2 code and then check the add ggplot2 code checkbox. ',
                                             rows=4,width=370)
                             
                           )
                          ),
                           column(3,#wellPanel(
                                  h4("Other Options"),
                                  hr(),
                             checkboxInput("coord_polar","원형그래프그리기",value=FALSE,width=140),
                             conditionalPanel( 
                               condition="input.coord_polar==true",
                                 selectInput3('theta','theta',c("x","y"),width=70),
                                 selectInput3('direction','direction',c("CW"=1,"CCW"=-1),width=70),
                                 textInput3('start','start(radian)',value=0,width=140)
                             ),
                             checkboxInput("coordflip","x축과 y축 뒤바꾸기",value=FALSE,width=140),
                             checkboxInput("coordfixed","축 비율 고정하기",value=FALSE,width=140),
                             conditionalPanel( 
                               condition="input.coordfixed==true",
                               textInput3('coordratio','ratio',value=1,width=140)
                             ),
                             checkboxInput("coordmap","coord_map()",value=FALSE,width=140),
                             conditionalPanel( 
                               condition="input.coordmap==true",
                               textInput3('projection','projection',value="mercator",width=140)
                             ),
                             selectInput3("palette","Palette 바꾸기(범주형변수)",c("None",rownames(brewer.pal.info)),selected="None",width=220),
                             checkboxInput("palettereverse","reverse palette",value=FALSE,width=220),
                             checkboxInput("palettecont","apply to continuous var",value=FALSE,width=220),
                             checkboxInput("palettecolor","color에 적용",value=FALSE,width=220),
                             textInput3("cpalette","연속형 변수색깔 바꾸기","",width=220),
                             checkboxInput("cpalettec","color에 적용",value=FALSE,width=220),
                             selectInput3("legendposition","범례 위치",c("top","left","right","bottom","none"),selected="right",width=140),
                             checkboxInput("legendreverse","범례순서 반대로",value=FALSE,width=220),
                             textInput3("title","그래프제목","",width=220),
                             textInput3("xlab","x축 제목","",width=220),
                             checkboxInput("xreverse","x축뒤집기",value=FALSE,width=140),
                             checkboxInput("remove.x.text","눈금라벨없애기",value=FALSE,width=140),
                             checkboxInput("log.x","로그축으로 변경",value=FALSE,width=140),
                             textInput3("ylab","y축 제목","",width=220),
                             checkboxInput("yreverse","y축뒤집기",value=FALSE,width=140),
                             checkboxInput("remove.y.text","눈금라벨없애기",value=FALSE,width=140),
                             checkboxInput("log.y","로그축으로 변경",value=FALSE,width=140),
                             checkboxInput("remove.tick","눈금표시없애기",value=FALSE,width=140),
                             checkboxInput("axis.blank","축제목라벨모두없애기",value=FALSE,width=220),
                             textInput3("xlim","x축 범위","",width=140),
                             textInput3("ylim","y축 범위","",width=140),
                             selectInput3("fonts","폰트종류",choices=c("NA","Helvetica","Times","Courier","NanumGothic",fonts()),selected="NA",width=140),
                             numericInput3("fontsize","폰트크기",value=12,step=1,width=140)
                             
                           #)
                           )   
                         )),
                       # hr(),
                       # radioButtons("useEditedData","Select data to plot",
                       #              choices=c("edited data"=1,"original data"=0),selected=1,
                       #              inline=TRUE),
                       hr(),
                       fluidRow(
                         column(2,downloadButton("downloadPlot","download fig")),
                         #column(2,downloadButton("downloadPDF","download PDF")),
                         column(2,downloadButton("downloadPPT","download PPT"),
                                checkboxInput("includecode","code 포함",value=FALSE)),
                         column(2,actionButton("saveToMultiplot","MultiPlot으로 저장")),
                         column(2,actionButton("saveToPPTList","PPT List에 저장"))
                         
                       ),
                       hr(),

                       fluidRow(
                         
                          column(6,
                                 selectInput3("theme","테마 변경",choices=c("grey","bw","linedraw","light",
                                                                        "minimal","classic"),selected="grey",width=140),
                                 selectInput3("ggthemes","추가테마적용",choices=c("None","clean","economist","stata","excel","solarized-light",
                                                                            "solarized-dark","solarized-alternative","tableau","fivethirtyeight",
                                                                            "few","wsj","gdocs","calc","pander","hc","hc-dark"),selected="grey",width=260),
                                 
                                 textareaInput('rorder','ggplot2 code.',rows=8,width=403)
                          ),
                          column(6,
                                 wellPanel(    
                                   h4("plot size"),
                                   numericInput3("plotWidth","Plot width",value=7,step=0.5),
                                   numericInput3("plotHeight","Plot height",value=5,step=0.5),
                                   numericInput3("plotRes","Resolution(ppi)",value=300,step=1),
                                   selectInput3("plotUnit","units",choices=c("in","cm","mm"),selected="in")
                                   
                                 ), 
                                 checkboxInput("showsummary",'show ggplot summary',value=FALSE),
                                 verbatimTextOutput('ggplotText'))
                       ),

                      imageOutput("myImage",width="900px",height="642px")

                       ),
        
tabPanel("MultiPlot",
         fluidPage(
           fluidRow(
             column(6,
                    div(id='plot1'),
                    checkboxInput('transparent1',"배경을 투명하게",value=FALSE),
                    selectInput3('inset1choice',"viewport 선택",
                                 choices=insetchoices,selected=3,width=100),
                    textInput3('inset1vp',"또는 viewport 입력",value="",width=260),
                    textInput3('label1',"label",value="A",width=40),
                    textareaInput('plotinset1','Enter code for the first plot here',width=420),
                    hr(),
                    div(id='plot2'),
                    checkboxInput('transparent2',"배경을 투명하게",value=FALSE),
                    selectInput3('inset2choice',"viewport 선택",
                        choices=insetchoices,selected=11,width=100),
                    textInput3('inset2vp',"또는 viewport 입력",value="",width=260),
                    textInput3('label2',"label",value="B",width=40),
                    textareaInput('plotinset2','Enter code for the 2nd plot here',width=420)
             ),       
             column(6,
                    div(id='plot3'),
                    checkboxInput('transparent3',"배경을 투명하게",value=FALSE),
                    selectInput3('inset3choice',"viewport 선택",
                                choices=insetchoices,selected=14,width=100),
                    textInput3('inset3vp',"또는 viewport 입력",value="",width=260),
                    textInput3('label3',"label",value="C",width=40),
                    textareaInput('plotinset3','Enter code for the 3rd plot here',width=420),
                    hr(),
                    div(id='plot4'),
                    checkboxInput('transparent4',"배경을 투명하게",value=FALSE),
                    selectInput3('inset4choice',"viewport 선택",
                                choices=insetchoices,selected=12,width=100),
                    textInput3('inset4vp',"또는 viewport 입력",value="",width=260),
                    textInput3('label4',"label",value="D",width=40),
                    textareaInput('plotinset4','Enter code for the 4th plot here',width=420)
             )       
           ),
           fluidRow(
           column(6,
              checkboxInput('doMultiplot',"Multiplot 만들기",value=FALSE),
              selectInput('MultiplotExample',"Multiplot 예제선택",
                                choices=c("None"=0,"Upper + RLQ,LLQ"=1,"Left + RUQ,RLQ"=2,"Full + RLQs,LUQs"=3,
                                          "Four plots(2 by 2)"=4),selected=0),
              downloadButton("downloadMultiplot","download Multiplot"),
              #downloadButton("downloadMultiplotpdf","download pdf"),
              downloadButton("downloadPPT2","download PPT")
           ),
           
           column(6,
           wellPanel(    
             h4("Multiplot size"),
             numericInput3("plotWidth2","Plot width",value=7,step=0.5),
             numericInput3("plotHeight2","Plot height",value=5,step=0.5),
             numericInput3("plotRes2","Resolution(ppi)",value=300,step=1),
             selectInput3("plotUnit2","units",choices=c("in","cm","mm"),selected="in")
             
           ))), 
           imageOutput("myInsetImage",width="900px",height="700px")  
         )     
),
tabPanel("Powerpoint List",
    fluidPage(
       fluidRow(
           column(6,
                  textareaInput("ppt1","ppt1",row=8,width=400),
                  textareaInput("ppt2","ppt2",row=8,width=400),
                  textareaInput("ppt3","ppt3",row=8,width=400),
                  textareaInput("ppt4","ppt4",row=8,width=400),
                  textareaInput("ppt5","ppt5",row=8,width=400)
            ),
           column(6,
                  textareaInput("ppt6","ppt6",row=8,width=400),
                  textareaInput("ppt7","ppt7",row=8,width=400),
                  textareaInput("ppt8","ppt8",row=8,width=400),
                  textareaInput("ppt9","ppt9",row=8,width=400),
                  textareaInput("ppt10","ppt10",row=8,width=400)
           )
       ),
       column(3,
              actionButton("ClearAll","Clear All")
       ),
       column(3,
           downloadButton("downloadPPT3","download PPT"),
           checkboxInput("includecode3","code 포함",value=FALSE)
       )
    )
),
tabPanel("SessionInfo",
          fluidPage(
            fluidRow(
              verbatimTextOutput("session")
            )
          )
         ),
tabPanel("About",
         uiOutput("about")
),
tabPanel("Interactive plot",
         div(id="interactive","In this tab, you can make intercative ggplot2 using packages ggiraph and ggiraphExtra."),
         hr(),
         selectInput3("iplotEx", 'Select Data',c("None","acs","rose","browsers","taco","mtcars","heightweight","radial","Salaries",
                                                "iris","crimes","tophitters2001"),"None",selectize=TRUE,width=200),
         checkboxInput3("showDataHelp2","show help for data",value=FALSE,width=200),
         conditionalPanel(condition="input.showDataHelp2==true",
                          h3("Help for Data"),
                          htmlOutput("helpData2"),
                          hr()
         ),
         tableOutput("table1"),
         hr(),
         conditionalPanel(condition="true==false",
                          textInput("mapping","mapping",value="",width='500px'),
                          textInput("interactivemode","mode",value="")
         ),
         
         fluidRow(
         column(3,
                h4("Assign Variables"),
                 conditionalPanel(condition="input.interactivemode==1",
                        selectInput3("ix1","x",c(""),width=iwidth)
                 ),
                 conditionalPanel(condition="input.interactivemode==2",
                        selectInput3("ixx","x",c(""),width=iwidth,multiple=TRUE)
                 ),
                 selectInput3("iyy","y",c(""),width=iwidth),
                 selectInput3("icolorcolor","color",c(""),width=iwidth),
                 conditionalPanel(condition="input.iplotmain!='ggChoropleth'",
                     selectInput3("ifill1","fill",c(""),width=iwidth)
                 ),
                 conditionalPanel(condition="input.iplotmain=='ggChoropleth'",
                        selectInput3("ifillfill","fill",c(""),width=iwidth,multiple=TRUE)
                 ),
                 selectInput3("igroupgroup","group",c(""),width=iwidth),
                 selectInput3("ifacetfacet","facet",c(""),width=iwidth),
                 selectInput3("pies","pies",c(""),width=iwidth),
                 selectInput3("donuts","donuts",c(""),width=iwidth)
         ),
         
      
                 
                 column(3,
                        h4("Select function"),
                        selectInput("iplotmain",NULL,
                                    c("ggAncova","ggBar","ggBoxplot","ggCatepillar","ggChoropleth",
                                      "ggCLE","ggCor","ggDensity","ggDot","ggEffect",
                                      "ggErrorBar","ggHeatmap","ggPieDonut","ggPair",
                                      "ggPoints","ggRadar","ggRose","ggSpine"),
                                    selectize=FALSE,selected="ggAncova",size=18),
                        actionButton("Reset","Reset main function"),
                        checkboxInput("showFunctionHelp","show help for function",value=FALSE)
                        ),
                 column(6,
                        h4("Other Parameters"),
                        conditionalPanel(condition="input.iplotmain=='ggAncova'",
                                         
                                         wellPanel(
                                                 # selectInput3("iy1","y",c(""),width=iwidth),
                                                 # selectInput3("ix1","x",c(""),width=iwidth),
                                                 # selectInput3("iA1","color",c(""),width=iwidth),
                                                 selectInput3("ilabel1","label",c(""),width=iwidth),
                                                 numericInput3("idigits1","digits",min=0,max=10,step=1,value=1,width=iwidth)
                                                 #textInput3("formula1","formula","",width=iwidth*2)
                                         )             
                        ),
                        # conditionalPanel(condition="input.iplotmain=='ggBidirectionalBar'",
                        #                  h4("Parameters"),
                        #                  wellPanel(
                        #                          selectInput3("left","left",c(""),width=iwidth),
                        #                          selectInput3("right","right",c(""),width=iwidth),
                        #                          selectInput3("ilabel2","label",c(""),width=iwidth)
                        #                  )
                        # ),
                        conditionalPanel(condition="input.iplotmain=='ggCatepillar'",
                                       
                                         wellPanel(
                                                 # selectInput3("iy2","y",c(""),width=iwidth),
                                                 # selectInput3("ix2","x",c(""),width=iwidth),
                                                 # selectInput3("igroup2","group",c(""),width=iwidth),
                                                 numericInput3("idigits2","digits",min=0,max=10,step=1,value=1,width=iwidth)
                                                 #textInput3("formula2","formula","",width=iwidth*2)
                                         )
                        ),
                        conditionalPanel(condition="input.iplotmain=='ggDensity'",
                                      
                                         wellPanel(
                                                 checkboxInput3("addhist","add histogram",value=TRUE,width=200)
                                         )
                        ),
                        conditionalPanel(condition="input.iplotmain=='ggEffect'",
                                  
                                         wellPanel(
                                                 # selectInput3("iy3","y",c(""),width=iwidth),
                                                 # selectInput3("ix3","x",c(""),width=iwidth),
                                                 # selectInput3("icolor3","color",c(""),width=iwidth),
                                                 textInput3("probs","probs",value = "c(0.1,0.5,0.9)",width=iwidth*2)
                                         )
                        ),
                        conditionalPanel(condition="input.iplotmain=='ggErrorBar'",
                            
                                         wellPanel(
                                                 # selectInput3("iy10","y",c(""),width=iwidth),
                                                 # selectInput3("ix10","x",c(""),width=iwidth),
                                                 # selectInput3("igroup10","group",c(""),width=iwidth),
                                                 numericInput3("idigits10","digits",min=0,max=10,step=1,value=1,width=iwidth)
                                                 #textInput3("formula10","formula","",width=iwidth*3)
                                         )
                        ),
                        conditionalPanel(condition="input.iplotmain=='ggPieDonut'",
                                      
                                         wellPanel(
                                                 # selectInput3("pies","pies",c(""),width=iwidth),
                                                 # selectInput3("donuts2","donuts",c(""),width=iwidth),
                                                 selectInput3("count2","count",c(""),width=iwidth),
                                                 numericInput3("labelposition2","labelposition",value=1,width=iwidth)
                                         )
                        ),
                        conditionalPanel(condition="input.iplotmain=='ggRadar'",
                                        
                                         wellPanel(
                                                 # selectInput3("igroup4","group",c(""),width=iwidth),
                                                 # selectInput3("ix4","x",c(""),width=iwidth,multiple=TRUE),
                                                 # #selectInput3("iy4","y",c(""),width=iwidth),
                                                 selectInput3("colour4","colour",choices =colours(),selected="red"),
                                                 checkboxInput3("rescale","rescale",value=TRUE),
                                                 selectInput3("ylim4","ylim",choices = c("NULL","c(0,1)"),selected=NULL)
                                         )
                        ),
                        conditionalPanel(condition="input.iplotmain=='ggSpine'",
                                        
                                         wellPanel(
                                                 # selectInput3("ix5","x",c(""),width=iwidth),
                                                 # selectInput3("ifill5","fill",c(""),width=iwidth),
                                                 # selectInput3("iy5","y",c(""),width=iwidth),
                                                 selectInput3("stat5","stat",c("count","identity"),width=iwidth),
                                                 selectInput3("position5","position",c("fill","stack","dodge"),width=iwidth),
                                                 selectInput3("palette5","palette",
                                                              c("None",rownames(brewer.pal.info)),selectize=TRUE,selected="Blues",width=iwidth),
                                                 selectInput3("colour5","colour",choices =colours(),selected="black",width=iwidth),
                                                 checkboxInput3("addlabel","addlabel",value=TRUE),
                                                 checkboxInput3("polar5","polar",value=FALSE),
                                                 numericInput3("width5","width",value=0,step=0.1,max=1,min=0,width=iwidth),
                                                 numericInput3("size","size",value=0.2)
                                         )
                        ),
                        conditionalPanel(condition="input.iplotmain=='ggBar'",
                                      
                                         wellPanel(
                                                 # selectInput3("ix6","x",c(""),width=iwidth),
                                                 # selectInput3("ifill6","fill",c(""),width=iwidth),
                                                 # selectInput3("iy6","y",c(""),width=iwidth),
                                                 selectInput3("stat6","stat",c("count","identity"),width=iwidth),
                                                 selectInput3("position6","position",c("stack","fill","dodge"),width=iwidth),
                                                 selectInput3("palette6","palette",
                                                              c("None",rownames(brewer.pal.info)),selectize=TRUE,selected="None",width=iwidth),
                                                 checkboxInput3("reverse6","reverse palette",value=FALSE,width=iwidth),
                                                 selectInput3("colour6","colour",choices =colours(),selected="black",width=iwidth),
                                                 checkboxInput3("addlabel6","addlabel",value=TRUE,width=iwidth),
                                                 checkboxInput3("polar6","polar",value=FALSE,width=iwidth),
                                                 checkboxInput3("horizontal6","horizontal",value=FALSE,width=iwidth),
                                                 numericInput3("width6","width",value=0,step=0.1,max=1,min=0,width=iwidth),
                                                 numericInput3("size6","size",value=0.2,width=iwidth)
                                         )    
                                        
                        ),
                        conditionalPanel(condition="input.iplotmain=='ggRose'",
                                        
                                         wellPanel(
                                                 # selectInput3("ix9","x",c(""),width=iwidth),
                                                 # selectInput3("ifill9","fill",c(""),width=iwidth),
                                                 # selectInput3("iy9","y",c(""),width=iwidth),
                                                 #selectInput3("stat9","stat",c("count","identity"),width=iwidth),
                                                 selectInput3("position9","position",c("stack","fill","dodge"),width=iwidth),
                                                 selectInput3("palette9","palette",
                                                              c("None",rownames(brewer.pal.info)),selectize=TRUE,selected="Reds",width=iwidth),
                                                 checkboxInput3("reverse9","reverse palette",value=FALSE,width=iwidth),
                                                 selectInput3("colour9","colour",choices =colours(),selected="black",width=iwidth),
                                                 checkboxInput3("addlabel9","addlabel",value=FALSE,width=iwidth),
                                                 numericInput3("size9","size",value=0.2,width=iwidth)
                                         )
                        ),
                        conditionalPanel(condition="input.iplotmain=='ggHeatmap'",
                                  
                                         wellPanel(
                                                 # selectInput3("ix8","x",c(""),width=iwidth),
                                                 # selectInput3("iy8","y",c(""),width=iwidth),
                                                 # selectInput3("ifill8","fill",c(""),width=iwidth),
                                                 # selectInput3("ifacet8","facet",c(""),width=iwidth),
                                                 selectInput3("stat8","stat",c("count","identity"),width=iwidth),
                                                 selectInput3("colour8","colour",choices =colours(),selected="grey50",width=iwidth),
                                                 selectInput3("palette8","palette",c(rownames(brewer.pal.info)),selected="Blues",width=iwidth),
                                                 checkboxInput3("reverse8","reverse",value=FALSE),
                                                 checkboxInput3("addlabel8","addlabel",value=FALSE),
                                                 checkboxInput3("polar8","polar",value=FALSE),
                                                 numericInput3("size8","size",value=0.1,step=0.1,min=0,width=iwidth),
                                                 numericInput3("yangle8","yangle",value=0,min=0,max=360,step=1,width=iwidth)
                                         )
                        ),
                       
                        conditionalPanel(condition="input.iplotmain=='ggChoropleth'",
                                       
                                         wellPanel(
                                                 selectInput3("map11","map",choices=c("states_map","kormap1","kormap2","kormap3","none"),width=iwidth),
                                                 # selectInput3("ifill11","fill",c(""),width=iwidth,multiple=TRUE),
                                                 selectInput3("map_id","map_id",c(""),width=iwidth), 
                                                 selectInput3("tooltip","tooltip",c(""),width=iwidth), 
                                                 selectInput3("palette11","palette",c(rownames(brewer.pal.info)),selected="OrRd",width=iwidth),
                                                 checkboxInput3("reverse11","reverse",value=FALSE),
                                                 selectInput3("color11","colors",choices=colors(),selected="grey50",width=iwidth),
                                                 textInput3("maptext","maptext",value="",width=iwidth),
                                                 
                                                 textInput3("title11","title",value="",width=iwidth)
                                         )), 
                        conditionalPanel(condition="input.iplotmain=='ggPoints'",
                                      
                                         wellPanel(
                                                 # selectInput3("ix7","x",c(""),width=iwidth),
                                                 # selectInput3("iy7","y",c(""),width=iwidth),
                                                 # selectInput3("icolour7","colorvar",c(""),width=iwidth),
                                                 # selectInput3("ifill7","fill",c(""),width=iwidth),
                                                 # selectInput3("ifacet7","facet",c(""),width=iwidth),
                                                 selectInput3("method7","method",
                                                              c("auto","lm","glm","gam","loess","rlm"),width=iwidth),
                                                 selectInput3("formula7","formula",
                                                              choices = c("y~x", "y~poly(x,2)","y~poly(x,3)"),width=iwidth),
                                                 numericInput3("level7","level",value=0.95,setp=0.01,min=0.01,max=1,width=iwidth),
                                                 checkboxInput3("se7","show se",value=TRUE,width=iwidth),
                                                 checkboxInput3("fullrange7","fullrange",value=FALSE,width=iwidth)
                                                 
                                         )), 
                        conditionalPanel(condition="input.iplotmain=='ggPair'",
                                    
                                         wellPanel(
                                                 #selectInput3("ix12","x",c("None"),width=iwidth,multiple=TRUE),
                                                # selectInput3("iy12","var2",c("None"),width=iwidth),
                                                 #selectInput3("icolor12","color",c("None"),width=iwidth),
                                                 checkboxInput3("ihorizontal12","horizontal",value=FALSE,width=iwidth)
                                                 
                                                 
                                         )), 
                        conditionalPanel(condition="input.iplotmain=='ggBoxplot'",
                                         
                                         wellPanel(
                                                # selectInput3("ix16","x",c("None"),width=iwidth,multiple=TRUE),
                                                 #selectInput3("icolor16","color",c("None"),width=iwidth),
                                                 checkboxInput3("irescale16","rescale",value=FALSE,width=iwidth),
                                                 checkboxInput3("ihorizontal16","horizontal",value=FALSE,width=iwidth)
                                                 
                                                 
                                         )), 
                        conditionalPanel(condition="input.iplotmain=='ggCLE'",
                                       
                                         wellPanel(
                                                 # selectInput3("ix13","x",c("None"),width=iwidth),
                                                 # selectInput3("iy13","y",c("None"),width=iwidth),
                                                 # selectInput3("icolor13","color",c("None"),width=iwidth),
                                                 # selectInput3("ifacet13","facet",c("None"),width=iwidth),
                                                 numericInput3("ino13","no",value=0,min=0,step=1,width=iwidth),
                                                 checkboxInput3("ireorder13","reorderByX",value=TRUE,width=iwidth),
                                                 checkboxInput3("idecreasing13","decreasing",value=TRUE,width=iwidth)
                                                 
                                         )), 
                        conditionalPanel(condition="input.iplotmain=='ggCor'",
                                       
                                         wellPanel(
                                                 selectInput3("ialternative14","alternative",c("two.sided","less","greater"),width=iwidth),
                                                 selectInput3("imethod14","alternative",c("pearson","kendall","spearman"),width=iwidth),
                                                 selectInput3("ilabel14","label",c("None"=0,"r value"=1,"r value*"=2),width=iwidth),
                                                 numericInput3("ici14","conf.level",value=0.95,step=0.01)
                                                 
                                         )), 
                        conditionalPanel(condition="input.iplotmain=='ggDot'",
                                       
                                         wellPanel(
                                                 # selectInput3("ix15","x",c("None"),width=iwidth),
                                                 # selectInput3("iy15","y",c("None"),width=iwidth),
                                                 # selectInput3("ifill15","fill",c("None"),width=iwidth),
                                                 selectInput3("istackdir15","stackdir",c("center","up","down","centerwhole"),width=iwidth),
                                                 selectInput3("ibinaxis15","binaxis",c("x","y"),width=iwidth),
                                                 selectInput3("imethod15","method",c("dotdensity","histodot"),width=iwidth),
                                                 selectInput3('iboxfill15','boxfill',c('NULL',colors()),selected='NULL',width=iwidth),
                                                 numericInput3("ibinwidth15","binwidth",value=0.5,min=0,step=0.1,width=iwidth),
                                                 numericInput3("iposition15","position",value=0.2,min=0,step=0.1,width=iwidth),
                                                 numericInput3("iboxwidth15","boxwidth",value=0.25,min=0,step=0.1,width=iwidth)
                                                 
                                                 
                                         )), 
                                                 
                        conditionalPanel(condition="input.iplotmain=='ggplot'",
                        h4("mapping"),
                        wellPanel(
                        selectInput3("ix","x",c(""),width=iwidth),
                        selectInput3("iy","y",c(""),width=iwidth),
                        selectInput3("iA","A",c(""),width=iwidth),
                        selectInput3("ilabel","label",c(""),width=iwidth),
                        selectInput3("icolour","colour",c(""),width=iwidth),
                        selectInput3("igroup","group",c(""),width=iwidth),
                        selectInput3("ifill","fill",c(""),width=iwidth),
                        selectInput3("ialpha","alpha",c(""),width=iwidth),
                        selectInput3("ifacet","2Dfacet",c(""),width=iwidth),
                        numericInput3("inofacet","ncol(facet)",value=0,min=0,width=iwidth)
                        ),
                        h4("setting"),
                        wellPanel(
                        selectInput3("istat","stat",c("count","identity"),width=iwidth),
                        selectInput3("iposition","position",c("fill","stack","dodge"),width=iwidth),
                        selectInput3("ipalette","palette",
                                     c("None",rownames(brewer.pal.info)),selectize=TRUE,selected="None",width=iwidth),
                        numericInput3("iwidth","width",min=0,max=1,step=0.01,value=1,width=iwidth),
                        numericInput3("idigits","digits",min=0,max=10,step=1,value=1,width=iwidth),
                        selectInput3("icolorset","color",c("",colours()),width=iwidth),
                        numericInput3("isize","size",min=0,step=0.1,value=1,width=iwidth),
                        textInput3("formula","formula","",width=iwidth*3),
                        textInput3("ylim5","ylim","",width=iwidth),
                        checkboxInput("rescale","rescale",value=TRUE)
                        )
                        ),
                        p("R code(You can edit the following R code)"),
                        textareaInput("iplotcode","R code for plot",value="",rows=3,width=450)
                        )
         ),
         conditionalPanel(condition="input.showFunctionHelp==true",
                          h3("Help for Function"),
                          htmlOutput("helpFunction"),
                          hr()
         ),
         # hr(),
         # verbatimTextOutput('ipmain'),
         # hr(), 
         conditionalPanel(condition="true==false",
         actionButton("makeIplot","Make Plot"),
         radioButtons("iplotoption","Plot Option",choices = c("interactive","static"),
                      inline=TRUE),
         textInput("plotlyok","plotlyok",value=0)
         ),
     
         ggiraph::ggiraphOutput("iploti"),
     
         conditionalPanel(condition="input.iplotoption=='static'",
                 plotOutput("iplots",width="100%",height="700px")
         ),
         
         # conditionalPanel(condition="input.iplotmain!='ggPoints'",
         # htmlOutput("testHTML")
         # ),
         # conditionalPanel(condition="input.plotlyok==1",
         #                  plotlyOutput("plotlyplot",height="700px") 
         # ),
         fluidRow(
         column(2,downloadButton("downloadPlot4","download fig")),
         #column(2,downloadButton("downloadPDF4","download PDF")),
         column(2,downloadButton("downloadPPT4","download PPT")),
         column(2,downloadButton("downloadReport","download HTML")),
         column(2,actionButton("saveToMultiplot4","save to MultiPlot")),
         column(2,actionButton("saveToPPTList4","save to PPTList"))
         )

),      
            id='main',
            theme=shinytheme("united")
  )  
)
)              

