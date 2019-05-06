theme_clean=function(base_size=12){
  theme_grey(base_size) %+replace%
    theme(
      axis.title=element_blank(),
      axis.text=element_blank(),
      panel.background=element_blank(),
      panel.grid=element_blank(),
      axis.ticks.length=unit(0,"cm"),
      axis.ticks.margin=unit(0,"cm"),
      panel.margin=unit(0,"lines"),
      plot.margin=unit(c(0,0,0,0),"lines"),
      complete=TRUE
    )
}

theme_axis_blank=function(){
  theme(axis.ticks=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
}