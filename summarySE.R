summarySE <- function(data=NULL, measurevar, groupvars=NULL,
                      conf.interval=.95, na.rm=TRUE, .drop=TRUE ) {
  require(plyr)
  # New version of length that can handle NAs: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col, na.rm) {
                   c( n = length2(xx[,col], na.rm=na.rm),
                      mean=mean (xx[,col],na.rm=na.rm),
                      sd = sd   (xx[,col],na.rm=na.rm)
                   )
                 }, 
                 measurevar,
                 na.rm
  )
  # Rename the "mean" column
  #datac
  colnames(datac)[colnames(datac)=="mean"]=measurevar
  #datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$n)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use
  #  df=n-1, or if n==0, use df=0
  ciMult <- qt(conf.interval/2 + .5, datac$n-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}
