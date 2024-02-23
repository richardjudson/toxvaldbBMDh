#-------------------------------------------------------------------------------
#' Plot the different between the Aurisano and current BMDh values
#' @param dir The directory where the lists are stored
#-------------------------------------------------------------------------------
bmdh.aurisano.check.plot <- function(to.file=F,toxval.db="res_toxval_v95",sys.date="2024-01-04") {
  printCurrentFunction()
  dir = "data/bmd/"
  file = paste0(dir,"toxval_PODs_for_BMDh ",toxval.db," ",sys.date,".xlsx")
  print(file)
  res = read.xlsx(file)
  x = res$bmdh
  y = res$bmdh_aurisano
  x = x[!is.na(y)]
  y = y[!is.na(y)]
  p = ggplot(data=res,aes(x=bmdh,y=bmdh_aurisano))  +
    ggtitle(paste0("Check BMDh values")) +
    geom_point(size=0.1) +
    theme_bw() +
    #facet_grid(~col) +
    xlab("Current") +
    ylab("Aurisano") +
    scale_x_continuous(trans = "log10",limits=c(1E-4,1E4)) +
    scale_y_continuous(trans = "log10",limits=c(1E-4,1E4)) #+
    #xlim() +
    #ylim(1E-4,1E4)
    #geom_segment(aes(x=-3,xend=3,y=-3,yend=3))
  print(p)
  if(to.file) {
    fname = paste0(dir,"bmdh.aurisano.check.plot.pdf")
    ggsave(plot = p, width = 5, height = 5, dpi = 300, filename =fname)
    dev.off()
  }
  else browser()
}
