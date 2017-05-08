#' ---
#' title: Anscombe Data
#' author:  A Calatroni & J Wildfire
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'    github_document:
#'      toc: true
#' ---

#' ### set defaults 
knitr::opts_knit$set(root.dir = '../..')
knitr::opts_chunk$set(warning = FALSE, message = FALSE, comment = NA)

#' ### packages
pacman::p_load(lattice, latticeExtra, grid, reshape2)
pacman::p_load(tidyverse)

#' ### citation
citation("lattice", auto = FALSE) %>% 
  toBibtex()

citation("reshape2", auto = FALSE) %>% 
  toBibtex()

#' ### data
data(anscombe)
anscombe$id <- rownames(anscombe)

#' ### reshape
anscombe.m <- melt(anscombe,id.var="id")
anscombe.m$var1 <- substr(anscombe.m$variable,1,1)
anscombe.m$var2 <- substr(anscombe.m$variable,2,3)
anscombe.c <- dcast(anscombe.m,id+var2~var1)

#' ### figure
#+ fig.width=8, fig.height=8
xyplot(y~x|var2,data=anscombe.c,
       as.table = T, col="black", pch=16, cex=1.1, fill="black", asp=1,
       xlab="X",ylab="Y",
       between=list(x=0.25,y=0.25),
       scale=list(x=list(alternating=F,tck=c(1,0)),
                  y=list(alternating=3,tck=c(1,1))),
       par.settings=list(strip.background=list(col="gray80")),
       panel = function(x, y, ...) {
         grid.text(paste0("X = ", round(mean(x),0)," (",round(var(x),0),")") ,0.15,0.95, gp=gpar(cex=0.80, font=3))
         grid.text(paste0("Y = ", round(mean(y),0)," (",round(var(y),0),")") ,0.15,0.90, gp=gpar(cex=0.80, font=3))
         grid.text(paste0("Cor = ", round(cor(x,y),2)) ,0.15,0.85, gp=gpar(cex=0.80, font=3))
         panel.lmlineq(x, y, adj = c(1,0), lty = 1, at=0.8, rotate=T,
                       col.line = "grey", digits = 2)
         panel.xyplot(x, y, ...)}
)


