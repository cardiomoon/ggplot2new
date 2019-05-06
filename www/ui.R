require(shiny)
require(markdown)
require(shinythemes)
require(RColorBrewer)
require(extrafont)

source("textInput2.R")

#loadfonts(device="postscript")

insetchoices=c("full"=0,"upper"=6,"lower"=5,"right"=8,"left"=7,"RUQ"=4,
               "RLQ"=2,"LUQ"=3,"LLQ"=1,"custom"=9,
               "centers"=10,"uppers"=16,"lowers"=15,"rights"=18,"lefts"=17,"RUQs"=14,
               "RLQs"=12,"LUQs"=13,"LLQs"=11)

shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: green;
      }
    "))
  ),
  
  # Application title
  h1("웹에서 하는 R 통계 : ggplot2 갤러리"),
  HTML(markdownToHTML(fragment.only=TRUE, text=c(
    "`ggplot2` 패키지를 사용하여 여러가지 plot을 그립니다. `데이타`와 그림에 사용할 `변수들을` 선택하세요."
  ))),
  hr(),
  navbarPage( "Web-R.org",
              tabPanel("ggplot2갤러리",
                       fluidPage(
                         fluidRow(
                           HTML(markdownToHTML(fragment.only=TRUE, 
                                               text=c("`데이타를 입력`하시거나 자료를 `업로드` 하세요.
                                                      ( xlsx 또는 csv화일을 읽을 수 있읍니다.)"))),
                           column(4,wellPanel(
                             fileInput("file", label = "자료 업로드(*.xlsx,*.csv)"),
                             
                             HTML(markdownToHTML(fragment.only=TRUE, 
                                                 text=c("`예제 데이타를 선택`하시기 전 꼭 `변수/옵션초기화`를 누르세요"))),
                             
                             radioButtons("radio", label = "데이타 선택",
                                          choices = list("Salaries", "acs","radial","iris","heightweight"),
                                          selected = "Salaries"),
                             singleton(
                               tags$head(tags$script(src = "message-handler.js"))
                             ),
                             actionButton("resetVar","변수/옵션 초기화")
                             
                           )),
                           column(8,wellPanel(
                                  checkboxInput2('doPreprocessing','데이타 전처리하기',value=FALSE,width=400),
                                  textareaInput('preprocessing',
                                                '자료의 preprocessing이 필요한 경우는 이곳에 입력하십시요.
예제갤러리의 전처리 예제를 참조하세요. \n업로드한 화일은 다음과 같이 접근하실 수 있읍니다.\nmy_readfile(input$file)
명령어를 고칠때는 체크박스를 해제하고 고친후 다시 체크하세요\n',rows=7,width=510),
                                  textInput2('mydata','사용할 데이타를 입력하세요',width=510),
                                  selectInput3("example","예제갤러리",
                                              c("None"=0,"Diamonds Are Forever"=21,
                                                "성악가의 키(1)"=14,"성악가의 키(2)"=15,"성악가의 키(3)"=16,
                                                "대학 교수의 연봉(1)"=11,"대학 교수의 연봉(2)"=12,"대학 교수의 연봉(3)"=13,
                                                "대학 교수의 연봉(4)"=17,
                                                "전국 시도 인구분포도"=18,
                                                "동별 서울 인구분포도"=19,
                                                "area plot wiith palette"=20,
                                                "polar plot"=22,
                                                "horizontal bar plot"=28,
                                                "heat map"=23,
                                                "boxplot"=24,
                                                "taco-HSD"=27,
                                                "Cleveland Dot Plot"=29,
                                                "Cleveland Dot Plot2"=30,
                                                "Cleveland Dot Plot3"=31,
                                                "화우 화산의 등고선"=32,
                                                "barplot with errorbar"=25,
                                                "linechart with errorbar"=26,
                                                "overlapped densities"=10,"Bubble plot"=1,"histogram with density"=2,
                                                "violin with boxplot"=3,"dotplot with boxplot"=4,
                                                "stat_density2d (1)"=5,"stat_density2d (2)"=6,
                                                "stat_density2d (3)"=7,"항목강조를 위한 전처리"=8,
                                                "전처리로 분할면에 각각 주석 넣기"=9),selected=0,width=250),
                                  selectInput3("PreProcessingExample","데이타 전처리 선택",
                                              c("None"=0,"열 이름 바꾸기"=4,
                                                "열 추가하기/ 삭제하기"=5,
                                                "평균과 신뢰구간으로 데이타 요약하기"=1,
                                                "넓은 데이타 프레임을 길게 변환하기"=2,
                                                "긴 데이타 프레임을 넓게 변환하기"=3),width=255),
                                  conditionalPanel(
                                    condition="input.PreProcessingExample==5",
                                    fluidRow(
                                      column(12,wellPanel(  
                                        selectInput3("delcolname","삭제할 열(들) 선택",c("None"),multiple=TRUE,selectize=TRUE,width=150),
                                        actionButton("delcol","열 삭제하기"),
                                        br(),
                                        textInput3("addcolname","추가할 열 이름 입력",value="id",width=150),
                                        actionButton("addcol","행번호열 추가하기"),
                                        br(),
                                        textInput3("addcolorder","기존열로 계산하기",value="",width=300),
                                        actionButton("addcol2","계산하여 추가하기")
                                      ))
                                    )
                                  ),
                                  conditionalPanel(
                                    condition="input.PreProcessingExample==4",
                                    fluidRow(
                                      column(8,wellPanel(  
                                        selectInput("oldcolname","이름을 바꿀 열 이름을 선택하세요",c("None"),selectize=TRUE),
                                        textInput("newcolname","새 이름을 입력하세요",value="")
                                      )),  
                                      actionButton("changecolname","열이름 바꾸기")
                                    )
                                  ),
                                  conditionalPanel(
                                    condition="input.PreProcessingExample==3",
                                    fluidRow(
                                      column(8,wellPanel(  
                                        selectInput("id.var2","아이디로 사용할 변수들을 선택하세요",c("None"),multiple=TRUE,selectize=TRUE),
                                        selectInput("measure.vars2","위로 옮겨질 변수들을 선택하세요",c("."),selectize=TRUE,multiple=TRUE),
                                        selectInput("value.var2","값을 채울 변수의 이름을 입력하세요",c("None"),selectize=TRUE)
                                      )),  
                                      actionButton("long2wide","wide form으로 변환")
                                    )
                                  ),
                                  conditionalPanel(
                                    condition="input.PreProcessingExample==2",
                                    fluidRow(
                                    column(8,wellPanel(  
                                      selectInput("id.var","아이디로 사용할 변수들(id.vars)를 선택하세요",c("None"),multiple=TRUE,selectize=TRUE),
                                      selectInput("measure.vars","측정변수들(measure.vars)을 선택하세요",c("."),selectize=TRUE,multiple=TRUE),
                                      textInput("variable.name","새로 만드는 변수 이름을 선택하세요",value="variable"),
                                      textInput("value.name","값을 저장할 이름을 입력하세요",value="value")
                                    )),  
                                    actionButton("wide2long","long form으로 변환")
                                    )
                                  ),
                                  conditionalPanel(
                                    condition="input.PreProcessingExample==1",
                                    fluidRow(
                                      column(8,wellPanel(  
                                        selectInput("measurevar1","평균을 구할 변수를 선택하세요",c("None"),selectize=TRUE),
                                        selectInput("groupvar1","그룹으로 나눌 변수들을 선택하세요",c("None"),selectize=TRUE,multiple=TRUE)
                                      )),  
                                      actionButton("doSummary","자료요약 실행")
                                    )
                                  )
                                  
                                  
                                  ))),
                         tableOutput("table"),
                         uiOutput("plot.ui"),
                           
                         
                         hr(),
                         fluidRow(
                          column(3,
                             h4("변수선택"),
                             selectInput3("xvar","x축 변수 선택:",c("None"),selectize=TRUE,width=140),
                             checkboxInput3("factorxvar","범주형변수로 취급",value=FALSE,width=140),
                             checkboxInput3("reorderx","y축변수순 정렬",value=FALSE,width=140),
                             selectInput3("yvar","y축변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             checkboxInput3("reordery","x축변수순 정렬",value=FALSE,width=140),
                             selectInput3("groupvar","group 변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput3("colorvar","color 변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput3("fillvar","fill 변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput3("alphavar","alpha 변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput3("linetypevar","linetype 변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput3("sizevar","size 변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             checkboxInput3("sizearea","크기 면적에 비례",value=FALSE,width=140),
                             numericInput2("maxsize","maxsize",value=15,step=1,min=1,max=100,width=60),
                             selectInput3("shapevar","shape 변수 선택:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput3("facetrow","가로로 면 분할:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput3("facetcol","세로로 면 분할:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput3("facetwrap","2차원 면분할:",c("None"),selectize=TRUE,selected="None",width=140),
                             selectInput3("facetscales","면분할 scales:",c("fixed","free_x","free_y","free"),selectize=TRUE,selected="fixed",width=140),
                             selectInput3("facetspace","면분할 space:",c("fixed","free_x","free_y","free"),selectize=TRUE,selected="fixed",width=140),
                             numericInput2("facetwrapncol","ncol",value=3,step=1,min=1,width=60)
                             
                           ),
                          column(6,wellPanel(
                             h4("geometry options"),
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
                                  numericInput3('pointshape','shape',value=16,width=100),
                                  numericInput3('pointalpha','alpha',value=1,step=0.1,width=100)
                                  )
                             ),
                             checkboxInput("text","text",value=FALSE),
                             conditionalPanel(
                               condition="input.text==true",
                               wellPanel( 
                                 selectInput3('textstat','stat',c("identity","bin")),
                                 selectInput3('textposition','position',c("identity","stack","dodge")),
                                 selectInput3('textcolor','colour',c("NA",colors()),selected="NA"),
                                 br(),
                                 textInput3('textdata','data',value=""),
                                 textInput3('textlabel', 'label',value=""), 
                                 textInput3('textdodgeposition','dodgeposition',value=""),
                                 
                                 br(),
                                 textInput3('textx','x',value=""),
                                 textInput3('texthjust','hjust',value="",width=100),
                                 numericInput3('textsize','size',value=5,step=1,min=1),
                                 br(),
                                 textInput3('texty','y',value=""),
                                 textInput3('textvjust','vjust',value="",width=100),
                                 textInput3('textother','...',value="",width=200)
                                 )
                             ),
                             checkboxInput("segment","segment",value=FALSE),
                             conditionalPanel(
                               condition="input.segment==true",
                               wellPanel(
                                 selectInput3('segmentcolor','colour',c("NA",colors()),selected="NA"),
                                 selectInput3('segmentposition','position',c("identity","stack"),selected="identity"),
                                 selectInput3('segmentlineend','lineend',c('butt','round','square')),
                                 textInput3('segmentxend','xend',value="0"),
                                 textInput3('segmentyend','yend',value="0"),
                                 numericInput3('segmentsize','size',value=1,step=.1)   
                                 
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
                                 selectInput3("smoothcolor","colour",c("NA",colors())),
                                 selectInput3('smoothlinetype','linetye',
                                              c("NA"=-1,"blank"=0,"solid"=1,"dashed"=2,"dotted"=3,
                                                "dotdash"=4,"longdash"=5,"twodash"=6),selected=-1),
                                 textInput3("smoothfamily","family",value=""),
                                 numericInput3("level","level",value=0.95,step=0.01),
                                 numericInput3("smoothsize","size",value=0.5,step=.1),
                                 checkboxInput3("se","show se",value=TRUE)
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
                             checkboxInput("tile","tile",value=FALSE),
                             conditionalPanel( 
                               condition="input.tile==true",
                               wellPanel(
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
                                 numericInput3('ebpos','position',value=0.3,min=0,step=0.1),
                                 numericInput3('ebwidth','width',value=0.2,min=0,step=.1),
                                 numericInput3('ebsize','size',value=1,min=0,step=.1)
                                 
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
                                 selectInput3('tdgeom','geom',c("density2d","tile","raster")),
                                 selectInput3('tdcolor','color',c("NA","..level..",colors())),
                                 selectInput3('tdfill','fill',c("NA","..density..",colors())),
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
                               checkboxInput("addrorder","ggplot2 명령어 추가",value=FALSE),
                               textareaInput('rordertext','추가할 ggplot2 명령어를 입력하고 체크박스를 체크하세요.\n명령어를 고칠때는 체크박스를 해제하고 고친후 다시 체크하세요',rows=4,width=370)
                             
                           )
                          ),
                           column(3,#wellPanel(
                             checkboxInput3("coord_polar","원형그래프그리기",value=FALSE,width=140),
                             conditionalPanel( 
                               condition="input.coord_polar==true",
                                 selectInput3('theta','theta',c("x","y"),width=70),
                                 selectInput3('direction','direction',c("CW"=1,"CCW"=-1),width=70),
                                 textInput3('start','start(radian)',value=0,width=140)
                             ),
                             selectInput3("palette","Palette 바꾸기(범주형변수)",c("None",rownames(brewer.pal.info)),selectize=TRUE,selected="None",width=140),
                             checkboxInput3("palettecolor","color에 적용",value=FALSE,width=140),
                             textInput3("cpalette","연속형 변수색깔 바꾸기","",width=140),
                             checkboxInput3("cpalettec","color에 적용",value=FALSE,width=140),
                             selectInput3("legendposition","범례 위치",c("top","left","right","bottom","none"),selected="right",width=140),
                             checkboxInput3("legendreverse","범례순서 반대로",value=FALSE,width=140),
                             textInput3("title","그래프제목","",width=140),
                             textInput3("xlab","x축 제목","",width=140),
                             checkboxInput3("xreverse","x축뒤집기",value=FALSE,width=140),
                             checkboxInput3("remove.x.text","눈금라벨없애기",value=FALSE,width=140),
                             checkboxInput3("log.x","로그축으로 변경",value=FALSE,width=140),
                             textInput3("ylab","y축 제목","",width=140),
                             checkboxInput3("yreverse","y축뒤집기",value=FALSE,width=140),
                             checkboxInput3("remove.y.text","눈금라벨없애기",value=FALSE,width=140),
                             checkboxInput3("log.y","로그축으로 변경",value=FALSE,width=140),
                             checkboxInput3("remove.tick","눈금표시없애기",value=FALSE,width=140),
                             checkboxInput3("coordflip","x축과 y축 뒤바꾸기",value=FALSE,width=140),
                             textInput3("xlim","x축 범위","",width=140),
                             textInput3("ylim","y축 범위","",width=140),
                             selectInput3("fonts","폰트종류",choices=c("NA","Helvetica","Times","Courier",fonts()),selected="NA",width=140),
                             numericInput3("fontsize","폰트크기",value=12,step=1,width=140)
                             
                           #)
                           )   
                         )),
                       
                       fluidRow(
                         column(4,downloadButton("downloadPlot","download figure")),
                         column(4,downloadButton("downloadPDF","download as PDF")),
                         column(4,actionButton("saveToMultiplot","MultiPlot으로 저장"))
                         
                       ),
                       hr(),
selectInput3("theme","테마 변경",choices=c("grey","bw","linedraw","light",
                                       "minimal","classic"),selected="grey",width=140),
selectInput3("ggthemes","추가테마적용",choices=c("None","economist","stata","excel","solarized-light",
                                                "solarized-dark","solarized-alternative","tableau","fivethirtyeight",
                                                "few","wsj","gdocs","calc","pander","hc","hc-dark"),selected="grey",width=260),

                       fluidRow(
                         
                          column(6,
                                 textareaInput('rorder','ggplot2 명령어 입니다.',rows=8,width=450)
                          ),
                          column(6,
                                 checkboxInput("showsummary",'show ggplot summary',value=FALSE),
                                 verbatimTextOutput('ggplotText'))
                       ),

                      imageOutput("myImage",width="900px",height="642px")

                       ),
              
tabPanel("MultiPlot",
         fluidPage(
           fluidRow(
             column(6,
                    h4('첫번째 plot'),
                    checkboxInput('transparent1',"배경을 투명하게",value=FALSE),
                    selectInput3('inset1choice',"viewport 선택",
                                 choices=insetchoices,selected=3,width=100),
                    textInput3('inset1vp',"또는 viewport 입력",value="",width=260),
                    textInput3('label1',"label",value="A",width=40),
                    textareaInput('plotinset1','첫번째 plot을 그리는데 사용할 코드를 여기에 넣으세요',width=420),
                    hr(),
                    h4('두번째 plot'),
                    checkboxInput('transparent2',"배경을 투명하게",value=FALSE),
                    selectInput3('inset2choice',"viewport 선택",
                        choices=insetchoices,selected=11,width=100),
                    textInput3('inset2vp',"또는 viewport 입력",value="",width=260),
                    textInput3('label2',"label",value="B",width=40),
                    textareaInput('plotinset2','두번째 plot을 그리는데 사용할 코드를 여기에 넣으세요',width=420)
             ),       
             column(6,
                    h4('세번째 plot'),
                    checkboxInput('transparent3',"배경을 투명하게",value=FALSE),
                    selectInput3('inset3choice',"viewport 선택",
                                choices=insetchoices,selected=14,width=100),
                    textInput3('inset3vp',"또는 viewport 입력",value="",width=260),
                    textInput3('label3',"label",value="C",width=40),
                    textareaInput('plotinset3','세번째 plot을 그리는데 사용할 코드를 여기에 넣으세요',width=420),
                    hr(),
                    h4('네번째 plot'),
                    checkboxInput('transparent4',"배경을 투명하게",value=FALSE),
                    selectInput3('inset4choice',"viewport 선택",
                                choices=insetchoices,selected=12,width=100),
                    textInput3('inset4vp',"또는 viewport 입력",value="",width=260),
                    textInput3('label4',"label",value="D",width=40),
                    textareaInput('plotinset4','네번째 plot을 그리는데 사용할 코드를 여기에 넣으세요',width=420)
             )       
           ),
           checkboxInput('doMultiplot',"Multiplot 만들기",value=FALSE),
           fluidRow(
                 column(6,
                    wellPanel(    
                      h4("plot download 옵션"),
                      numericInput3("plotWidth","Plot width",value=7,step=0.5),
                      numericInput3("plotHeight","Plot height",value=5,step=0.5),
                      numericInput3("plotRes","Resolution(ppi)",value=300,step=1),
                      selectInput("plotUnit","units",choices=c("in","cm","mm"),selected="in",width='110px')
                      
                    ) 
             )),
           fluidRow(
             column(6,selectInput('MultiplotExample',"Multiplot 예제선택",
                                  choices=c("None"=0,"Upper + RLQ,LLQ"=1,"Left + RUQ,RLQ"=2,"Full + RLQs,LUQs"=3,
                                            "Four plots(2 by 2)"=4),selected=0)),
             column(6,downloadButton("downloadMultiplot","download Multiplot"))
             
           ),
           imageOutput("myInsetImage",width="900px",height="700px")  
         )     
),

              id='main',
              theme=shinytheme("united")
  )  
)
)              

