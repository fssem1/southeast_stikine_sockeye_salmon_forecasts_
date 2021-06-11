#figure layouts
vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)

arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
  dots <- list(...)  
  n <- length(dots)	
  if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}	
  if(is.null(nrow)) { nrow = ceiling(n/ncol)}	
  if(is.null(ncol)) { ncol = ceiling(n/nrow)}        
  #grid.Newpage()
  pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )  
  ii.p <- 1	
  for(ii.row in seq(1, nrow)){	
    ii.table.row <- ii.row		
    if(as.table) {ii.table.row <- nrow - ii.table.row + 1}		
    for(ii.col in seq(1, ncol)){			
      ii.table <- ii.p			
      if(ii.p > n) break			
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))			
      ii.p <- ii.p + 1}}}

#function to extract data from ADMB report
reptoRlist = function(fn)
{
  ifile=scan(fn,what="character",flush=T,blank.lines.skip=F,quiet=T)
  idx=sapply(as.double(ifile),is.na)
  vnam=ifile[idx]  #list names
  nv=length(vnam)           #number of objects
  A=list()
  ir=0
  for(i in 1:nv)
  {
    ir=match(vnam[i],ifile)
    if(i!=nv) irr=match(vnam[i+1],ifile) else irr=length(ifile)+1 #next row
    dum=NA
    if(irr-ir==2) dum=as.double(scan(fn,skip=ir,nlines=1,quiet=T,what=""))
    if(irr-ir>2) dum=as.matrix(read.table(fn,skip=ir,nrow=irr-ir-1,fill=T))
    
    if(is.numeric(dum))#Logical test to ensure dealing with numbers
    {
      A[[ vnam[i ] ]]=dum
    }
  }
  return(A)
  A = reptoRlist(fn='code/admb/model.rep') 
}

#formatting axis function
fmt <- function(){
  function(x) format(x,nsmall = 2,scientific = FALSE)}
round2<-function(x){trunc(x+0.5)} 

#labelling axes
every_nth <- function(x, nth, empty = TRUE, inverse = FALSE)
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}
#to get rid of scientific notation
value_formatter <- function (val) {
  format(val, scientific =FALSE)
}
#test for normality
eda.norm <- function(x, ...)
{
  par(mfrow=c(2,2))
  if(sum(is.na(x)) > 0)
    warning("NA's were removed before plotting")
  x <- x[!is.na(x)]
  hist(x, main = "Histogram and non-\nparametric density estimate", prob = T)
  iqd <- summary(x)[5] - summary(x)[2]
  lines(density(x, width = 2 * iqd))
  boxplot(x, main = "Boxplot", ...)
  qqnorm(x)
  qqline(x)
  plot.ecdf(x, main="Empirical and normal cdf")
  LIM <- par("usr")
  y <- seq(LIM[1],LIM[2],length=100)
  lines(y, pnorm(y, mean(x), sqrt(var(x))))
  shapiro.test(x)
}