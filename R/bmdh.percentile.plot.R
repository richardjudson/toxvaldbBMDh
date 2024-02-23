#-------------------------------------------------------------------------------
#' Plot the structural vs pod distance
#' @param dir The directory where the lists are stored
#-------------------------------------------------------------------------------
bmdh.percentile.plot <- function(to.file=F,minstudies=10,cutoff.logsd=2) {
  printCurrentFunction()
  dir = "data/bmd/"
  file = paste0(dir,"bmdh chemical level.xlsx")
  print(file)
  mat = read.xlsx(file)
  mat = mat[mat$studies>=minstudies,]
  mat$highsd = "N"
  mat[mat$log.sd>cutoff.logsd,"highsd"] = "Y"
  #mat = mat[mat$log.sd<cutoff.logsd,]
  plist = c(5,10,15,20,25,30,35)
  clist = c("pod_05","pod_10","pod_15","pod_20","pod_25","pod_30","pod_35")
  hlist = c(" 5th percentile","10th percentile","15th percentile","20th percentile","25th percentile","30th percentile","35th percentile")
  nlist = c("percentile","column","abserr","rmse","r2","slope","pval","chemicals")
  res = as.data.frame(matrix(nrow=length(plist),ncol=length(nlist)))
  names(res) = nlist
  pdata = NULL
  tmat = mat[mat$studies>=minstudies,]
  for(i in 1:length(plist)) {
    col = clist[i]
    res[i,"percentile"] = plist[i]
    res[i,"column"] = col

    x = log10(tmat[,col])
    y = log10(tmat[,"pod_hra"])

    ptemp = tmat[,c(col,"pod_hra","log.sd")]
    ptemp[,1] = log10(ptemp[,1])
    ptemp[,2] = log10(ptemp[,2])
    names(ptemp) = c("experiment","RA","log.sd")
    ptemp$col = hlist[i]
    ptemp$highsd = "N"
    ptemp[ptemp$log.sd>cutoff.logsd,"highsd"] = "Y"
    pdata = rbind(pdata,ptemp)
    temp = lm(y~x+0)
    stemp = summary(temp)
    slope = stemp$coefficients[1,1]
    p = stemp$coefficients[1,4]
    r2 = stemp$adj.r.squared
    rmse = stemp$sigma

    res[i,"abserr"] = mean(x-y,na.rm=T)
    res[i,"rmse"] = rmse
    res[i,"r2"] = r2
    res[i,"slope"] = slope
    res[i,"pval"] = p
    res[i,"chemicals"] = length(y)
  }
  print(res)
  p = ggplot(data=pdata,aes(x=experiment,y=RA))  +
    ggtitle(paste0("BMDh Percentiles")) +
    geom_point(size=0.1) +
    theme_bw() +
    facet_grid(~col) +
    xlim(-3,3) + ylim(-3,3) +
    xlab("Experimental") +
    ylab("Human RA") +
    geom_segment(aes(x=-3,xend=3,y=-3,yend=3))
  print(p)

  file = paste0(dir,"bmdh.percentile.plot.xlsx")
  write.xlsx(res,file)

  if(to.file) {
    fname = paste0(dir,"bmdh.percentile.plot.pdf")
    ggsave(plot = p, width = 8, height = 2.5, dpi = 300, filename =fname)
    dev.off()
  }
  else browser()
}
