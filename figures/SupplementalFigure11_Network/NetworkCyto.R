#' ---
#' title: Network
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
pacman::p_load(tidyverse, rio, reshape2)
pacman::p_load(RColorBrewer)
pacman::p_load(qgraph)

#' ### citation
citation("qgraph", auto = FALSE) %>% 
  toBibtex()

#' ### Import and Symmetric Matrix
dd <- import("figures/SupplementalFigure11_Network/NetworkCyto.csv") %>%
  select(-1) %>% 
  as.matrix()

dd <- Matrix::forceSymmetric(dd,"U")

#' ### Names Construction
nn <- colnames(dd)
nn <- as.data.frame(nn)

nn <- nn  %>%
  separate(nn,c("ca","cb","s","y")) %>%
  mutate(cy=paste(ca,cb,sep="."),
         cy2=paste(cy,s,sep="\n"))

col <- brewer.pal(3, "Set1")
nn$col <- with(nn,ifelse(y==0,col[1],ifelse(y==1,col[2],col[3])))

rownames(dd) <- nn$cy2
colnames(dd) <- nn$cy2

#' ### QGraph
#+ fig_height=7, fig_eidth=10
set.seed(57817)

qgraph(dd,
       graph='assosciation', layout='spring',
       layout.par = list(niter=5000),
       minimum=0.35,maximum=1,
       arrows=F,
       bg='white',label.color='black',
       color=nn$col,
       groups= factor(nn$y, labels=c("Birth","Year 1", "Year 3")),
       labels=as.factor(nn$cy2),
       legend=T, legend.cex=0.80,
       overlay=F,overlaySize=0.70,
       shape='circle',
       curve=0.2,
       vsize=2.5,label.cex=0.5,
       borders=FALSE, vTrans=200)



