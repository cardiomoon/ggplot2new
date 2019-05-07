require(ggplot2)
require(ggiraph)
# require(moonBook2)
require(moonBook)
require(lattice)
require(car)
require(reshape2)
require(plyr)
require(dplyr)
require(MASS)
require(ggmap)
require(gcookbook)

# devtools::install_github("davidgohel/ggiraph",force = TRUE)
# 
# require(png)
# makeThumb <- function(file, height=72, width=72) {
# 
#         img <- png::readPNG(file)
# 
#         png(file = paste("thumb", file, sep = "_"), height = height, width = width)
#         par(mar=c(0,0,0,0), xaxs="i", yaxs="i", ann=FALSE)
#         plot(1:2, type='n', xaxt = "n", yaxt = "n", xlab = "", ylab = "")
#         lim <- par()
#         rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
#         dev.off()
# }

# for(i in 8:49){
#         file=paste0(i,".png")
#         makeThumb(file)
# }
data=readRDS("ggplotEx.RDS")
# data1=read.csv("ggplotEx.csv",stringsAsFactors = FALSE)
# data1$no
# data$no=data1$no
# data$np=NULL
# str(data)
# data$Preprocessing[22]="volcano3d=melt(volcano);\nnames(volcano3d)<-c('x','y','z') "
# data$Preprocessing[44]="tbc<-readRDS('data/tbc_long4.RDS');\nmap=kormap1"
# data$Preprocessing[46]="tbc<-readRDS('data/tbcinci.RDS');\nmap=kormap1"
# data$Preprocessing[33]="pg <- PlantGrowth;\n    pg$hl<-'no';\n    pg$hl[pg$group=='trt2']<-'yes' "
# data$Preprocessing[34]="mpg2<-mpg;\nlevels(mpg2$drv)<-c('4WD','Front','Rear');\nlm_labels <- function(dat) {\nmod <- lm(hwy ~ displ, data=dat);\nformula <- sprintf('italic(y) == %.2f %+.2f * italic(x)',\nround(coef(mod)[1], 2), round(coef(mod)[2], 2));\n                    r <- cor(dat$displ, dat$hwy);\nr2 <- sprintf('italic(R^2) == %.2f', r^2);\ndata.frame(formula=formula, r2=r2, stringsAsFactors=FALSE)\n};\nlabels <- ddply(mpg2, 'drv', lm_labels) "
# data$Preprocessing[35]="require(gridExtra);\ntop10=head(mtcars[order(mtcars$mpg,decreasing=TRUE),c('mpg','wt')],10);\ntable_grob=tableGrob(top10) "
# data$Preprocessing[36]="res=lm2table('mtcars','wt','mpg');\ntable_grob=tableGrob(res)"
# data$Preprocessing[37]="grid <- expand.grid(\nx = seq(-pi, pi, length = 50),\ny = seq(-pi, pi, length = 50)\n) %>% mutate(r = x ^ 2 + y ^ 2, z = cos(r ^ 2) * exp(-r / 6));\nCenter='Center' "
# data$Preprocessing[38]
# # data$Preprocessing[18]="b<-biopsy;\nb$malig=ifelse(b$class=='malignant',1,0)"
#saveRDS(data,"ggplotEx.RDS")
#data$Preprocessing[17]="taco=read.csv('taco_results.csv');\ntaco.anova <- aov(Rating~ShellType*AgeGroup,data = taco);\ntaco.hsd <- data.frame(TukeyHSD(taco.anova,'AgeGroup', conf.level=.95)$AgeGroup);\ntaco.hsd$Comparison <- row.names(taco.hsd) "
#data$Preprocessing[14]="taco=read.csv('taco_results.csv');\nfilling.results <- taco %>%\ngroup_by(Filling) %>%\nsummarise(Rating = mean(Rating)) "
#data$Preprocessing[11]= "crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests);\ncrimesm <- melt(crimes, id = 1);\nstates_map <- map_data('state') "

# data$Code[3]
# data$Code[3]="ggplot(data=singer,aes(x=voice.part,y=height))+ \n geom_violin(fill='lightblue')+ \n geom_boxplot(fill='lightgreen',width=0.2)+ \n ggtitle(\"Singer's Height(2)\")"
#write.csv(data,"ggplotEx.csv",row.names = FALSE)
# data$Code[4]="ggplot(data=singer,aes(x=height,fill=voice.part))+ \n geom_density()+ \n facet_grid(voice.part ~ .)+ \n ggtitle(\"Singer's Height(3)\")"
# data$Code[2]="ggplot(data=singer,aes(x=voice.part,y=height))+ 
#         geom_boxplot()+ 
#         ggtitle(\"Singer's Height\")"

plotEx <- function(data,ncol=7) {
        require(shiny)
        shinyApp(
                ui = fluidPage(
                        column(5,
                       ggiraphOutput("plot",width="520px",height="520px")),
                       column(7,
                       numericInput("choice","choice",value=1),
                       textInput("Preprocessing","Preprocessing",value=""),
                       textInput("Rcode","R code",value=""),
                       plotOutput("plot2")
                       )
                       
                ), 
                server = function(session,input, output) {
                        
                        
                        output$plot <- renderggiraph({
                              
                                df<-data
                                total=nrow(df)
                                df$data_id=1:total
                                df$xmax=df$data_id%%ncol
                                df$xmax[df$xmax==0]=ncol
                                df$xmin=df$xmax-1
                                df$ymin=(total- df$data_id)%/%ncol
                                df$ymax=df$ymin+1
                                df$data_id=as.character(df$data_id)
                                df
                                
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
                                
                                ggiraph(code=print(p),width="100%",height="700px",selection_type = "single",
                                        tooltip_extra_css = tooltip_css,
                                        hover_css="cursor:pointer;stroke:gold;stroke-width:2px;")
                                
                        })
                       
                        selected_plot <- reactive({
                                input$plot_selected
                        })
                        
                        observeEvent(selected_plot(), {
                                updateNumericInput(session, "choice", value = as.numeric(selected_plot()) )
                                updateTextInput(session, "Preprocessing", value = data$Preprocessing[as.numeric(selected_plot())] )
                                updateTextInput(session, "Rcode", value = data$Code[as.numeric(selected_plot())] )
                                              
                        })
                        
                        output$plot2=renderPlot({
                                input$choice
                                
                                eval(parse(text=input$Preprocessing))
                                p<-eval(parse(text=input$Rcode))
                                p
                        })
                }
        )
}

plotEx(data)
