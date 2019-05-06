library(ggplot2)
library(reshape2)
library(plyr)

df <- data.frame(segment = c("A", "B", "C", "D"), 
                 segpct = c(40, 30, 20, 10), 
                 Alpha = c(60,40, 30, 25), Beta = c(25, 30, 30, 25),
                 Gamma = c(10, 20, 20, 25), Delta = c(5,10, 20, 25))
df$xmax <- cumsum(df$segpct)
df$xmin <- df$xmax - df$segpct
df$segpct <- NULL
df

dfm <- melt(df, id = c("segment", "xmin", "xmax"))
dfm
dfm1 <- ddply(dfm, .(segment), transform, ymax = cumsum(value))
dfm1 <- ddply(dfm1, .(segment), transform, ymin = ymax - value) 
dfm1
dfm1$xtext <- with(dfm1, xmin + (xmax - xmin)/2)
dfm1$ytext <- with(dfm1, ymin + (ymax - ymin)/2)
dfm1
p <- ggplot(dfm1, aes(ymin = ymin, ymax = ymax,
                      xmin = xmin, xmax = xmax, fill = variable))
p1 <- p + geom_rect(colour = I("grey"))
p1
p2 <- p1 + geom_text(aes(x = xtext, y = ytext,
               label = ifelse(segment == "A", 
                              paste(variable," - ", value, "%", sep = ""), 
                              paste(value, "%", sep = ""))), size = 3.5)
p2
p3 <- p2 + geom_text(aes(x = xtext, y = 103,
                         label = paste("Seg ", segment)), size = 4)
p3
p4<-p3 + theme_bw() + labs(x = NULL, y = NULL,
                       fill = NULL) + theme(legend.position = "none",
                                           panel.grid.major = element_line(colour = NA),
                                           panel.grid.minor = element_line(colour = NA))
p4+scale_fill_brewer(palette="Pastel2")


makeplot_mosaic <- function(data, x, y, ...){
  xvar <- deparse(substitute(x))
  yvar <- deparse(substitute(y))
  mydata <- data[c(xvar, yvar)];
  mytable <- table(mydata);
  widths <- c(0, cumsum(apply(mytable, 1, sum)));
  heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))});
  
  alldata <- data.frame();
  allnames <- data.frame();
  for(i in 1:nrow(mytable)){
    for(j in 1:ncol(mytable)){
      alldata <- rbind(alldata, c(widths[i], widths[i+1], heights[j, i], heights[j+1, i]));
    }
  }
  colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")
  
  alldata[[xvar]] <- rep(dimnames(mytable)[[1]],rep(ncol(mytable), nrow(mytable)));
  alldata[[yvar]] <- rep(dimnames(mytable)[[2]],nrow(mytable));
  
  print(alldata)
  ggplot(alldata, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) + 
    geom_rect(color="black", aes_string(fill=yvar)) +
    xlab(paste(xvar, "(count)")) + ylab(paste(yvar, "(proportion)"))
}

makeplot_mosaic(mtcars, vs, gear)

require(moonBook)
makeplot_mosaic(acs, Dx, sex)

ggplot(data=acs,aes(x=Dx,fill=sex))+geom_bar(position="fill")
