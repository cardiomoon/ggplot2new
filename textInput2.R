textareaInput<-function(inputId, label="",value="", rows=8, width=100){
  div(class="form-group shiny-input-container",
      tags$style(type="text/css", "textarea {width:100%}"),
      tags$textarea(id = inputId, placeholder = label, rows = rows, value=value,
                    style=paste("width: ",width,"px; display:inline-block;",sep=""))
  )    
}


textInput2<-function (inputId, label, value = "",width=100,...) 
{
  
  div(class="form-group shiny-input-container",style="display:inline-block;",
      tags$label(label, `for` = inputId, style="display:inline-block;"),
      tags$input(id = inputId, type = "text", class="form-control",value = value, 
                 style=paste("width: ",width,"px; display:inline-block;",sep=""),...)
  )
}

# textInput과 동일하나 폭 조절 가능(px)하고 side by side로 사용 가능
textInput3<-function (inputId, label, value = "",width=100,...) 
{
  div(style="display:inline-block;",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", class="form-control",value = value, 
                 style=paste("width: ",width,"px;",sep=""),...))
}

textInput4<-function (inputId, label, value = "",width=100,...) 
{
  div(
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", class="form-control",value = value, 
                 style=paste("color: green; width: ",width,"px;",sep=""),...))
}


numericInput2<-function (inputId, label, value , min=NA,max=NA,step=NA,width=100,...) 
{
  div(class="form-group shiny-input-container",style="display:inline-block;",
      tags$label(label, `for` = inputId,class="control-label"), 
      tags$input(id = inputId, type = "number", class="form-control",
                 value = value, min=min,max=max,step=step,style=paste("width: ",width,"px; display:inline-block;",sep=""),...)
  )
}

numericInput3<-function (inputId, label, value, min=NA,max=NA,step=NA,width=100,...) 
{
  div(style="display:inline-block;",
      tags$label(label, `for` = inputId,class="control-label"), 
      tags$input(id = inputId, type = "number", class="form-control",
                 value = value, min=min,max=max,step=step,style=paste("width: ",width,"px;",sep=""),...)
  )
}


selectInput3<-function(...,width=100){
  mywidth=paste(width,"px",sep="")
  div(style="display:inline-block;",selectInput(...,width=mywidth))
}

selectizeInput2<-function(inputId,label,...,width=100){
  mywidth=paste(width,"px",sep="")
  div(class="form-group shiny-input-container",
      selectizeInput(
        inputId, '',...,
        options = list(
          placeholder = label,
          onInitialize = I('function() { this.setValue(""); }')
        ),width=mywidth)) 
}

selectizeInput3<-function(inputId,label,...,width=100){
  mywidth=paste(width,"px",sep="")
  div(style="display:inline-block;",
      selectizeInput(
        inputId, '',...,
        options = list(
          placeholder = label,
          onInitialize = I('function() { this.setValue(""); }')
        ),width=mywidth)) 
}

checkboxInput2<-function(inputId,label,value=FALSE,width=100){
  if(value) 
    div(
        
        tags$input(id = inputId, type = "checkbox",checked = "checked"),
        tags$label(label, `for` = inputId, 
                   style=paste("width: ",width-15,"px;display:inline-block;",sep=""))
    )
  else
    div(
        tags$input(id = inputId, type = "checkbox"),
        tags$label(label, `for` = inputId, 
                   style=paste("width: ",width-15,"px; display:inline-block;",sep="")) 
    )
}    

checkboxInput3<-function(inputId,label,value=FALSE,width=100){
  if(value) 
    div(style="display:inline-block;",
        
        tags$input(id = inputId, type = "checkbox",checked = "checked"),
        tags$label(label, `for` = inputId, 
                   style=paste("width: ",width-15,"px;",sep=""))
    )
  else
    div(style="display:inline-block;",
        tags$input(id = inputId, type = "checkbox"),
        tags$label(label, `for` = inputId, style=paste("width: ",width-15,"px;",sep="")) 
    )
}    
