require(rhandsontable)

values=reactiveValues()
current<-""
currentdir=getwd()
tempdir=tempdir()
owd <- setwd(tempdir)
on.exit(setwd(owd)) 
setwd(currentdir)
setHot=function(x) values[["Hot"]]=x

origin=reactive({
  
  if(input$doPreprocessing) try(eval(parse(text=input$preprocessing)))
  validate(
    need(class(try(eval(parse(text=input$mydata))))=="data.frame","이용하실 R 데이타 이름을 입력하세요")
  )
  #resetPlot()
  df<-eval(parse(text=input$mydata)) 
  
  values$choice=input$Example
  values$new<-TRUE
  
  df
})

data=reactive({
  temp<-input$Example
  if(is.null(input$hot)) DF=origin()
  else {
    if((current=="")|(current==input$Example)){
      current<<-temp
      DF=hot_to_r(input$hot)
      
    } else {
      DF=origin()
      current<<-temp
    }       
    
  }       
  values[["DF"]]=DF
  DF
})


output$hot<-renderRHandsontable({
  DF=data() 
  
  hot=rhandsontable(DF) %>%
    hot_context_menu(
      customOpts = list(
        csv = list(name = "Remove all rows except 1",
                   callback = htmlwidgets::JS(
                     "function (key, options) {
                     this.alter('remove_row',1,this.countRows()-1);
}"))))
      
  hot
  })


df=reactive({
  values$DF
  
})