#-------------------------------------------------------------------------------
#' Plot the different between the Aurisano and current BMDh values
#'
#' `bmdh.per.study` plots the different between the Aurisano and current BMDh values
#' @param to.file If TRUE, send the plot to a file
#' @param toxval.db Database version
#' @param sys.date The date of the database export
#' @export
#-------------------------------------------------------------------------------
bmdh.aurisano.check.plot <- function(to.file=F,toxval.db="res_toxval_v95",sys.date="2024-02-23") {
  printCurrentFunction()
  dir = "data/"
  file = paste0(dir,"results/ToxValDB BMDh per study ",toxval.db," ",sys.date,".xlsx")
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
    fname = paste0(dir,"results/bmdh.aurisano.check.plot.pdf")
    ggsave(plot = p, width = 5, height = 5, dpi = 300, filename =fname)
    dev.off()
  }
  else browser()
}
