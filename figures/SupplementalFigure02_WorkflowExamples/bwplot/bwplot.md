Boxplot
================
A Calatroni & J Wildfire
27 February, 2017

-   [packages](#packages)
-   [create data](#create-data)
-   [Raw Data](#raw-data)
-   [Trasfrom](#trasfrom)
-   [Compare](#compare)
-   [Annotate](#annotate)
-   [Expand](#expand)

### packages

``` r
pacman::p_load(lattice, grid)
pacman::p_load(tidyverse)
pacman::p_load(pixiedust)
```

### create data

``` r
set.seed(2016)
dd1 <- data.frame(x = rnorm(500), g = 1 %>% as.factor())
dd2 <- data.frame(x = rnorm(500, 1.5, 1), g = 2 %>% as.factor())
dd2 <- rbind(dd1,dd2)

dd2$f <- "A"
set.seed(2016)
dd1_b <- data.frame(x = rnorm(500, 0.90, 0.95), g = 1 %>% as.factor())
dd2_b <- data.frame(x = rnorm(500, 1.05, 1.35), g = 2 %>% as.factor())
dd2_b <- rbind(dd1_b,dd2_b)
dd2_b$f <- "B"
dd3 <- rbind(dd2,dd2_b)
```

### Raw Data

``` r
dotplot(x ~ g, data=dd1,
        col='gray70', lwd=3, pch=19, jitter.y = T, amount=0.05,
        horizontal = F,
        scales=list(draw=F), 
        xlab=NULL, ylab=NULL,
        par.settings = list(axis.line = list(col = 0)),
        panel = function(x,y,...){
          panel.points(x,y,...)
        })
```

![](bwplot_files/figure-markdown_github/unnamed-chunk-3-1.png)

### Trasfrom

``` r
bwplot(x~g, data=dd1,
          col='gray70', cex=1, pch='|',
          horizontal = F,
          scales=list(draw=F), 
          xlab=NULL, ylab=NULL,
          par.settings = list(box.rectangle=list(col="black",fill="#DFDFDF",lwd=1,lty=1),
                              box.umbrella=list(col="black",lwd=1,lty=1),
                              plot.symbol=list(pch=151,cex=1,col="black"),
                              axis.line = list(col = 0)),
          panel = function(x,y,...){
            panel.bwplot(x,y,...)
          })
```

![](bwplot_files/figure-markdown_github/unnamed-chunk-4-1.png)

### Compare

``` r
bwplot(x~g, data=dd2,
       col='gray70', cex=1, pch='|',
       horizontal = F,
       scales=list(draw=F), 
       xlab=NULL, ylab=NULL,
       par.settings = list(box.rectangle=list(col="black",fill="#DFDFDF",lwd=1,lty=1),
                           box.umbrella=list(col="black",lwd=1,lty=1),
                           plot.symbol=list(pch=151,cex=1,col="black"),
                           axis.line = list(col = 0)),
       panel = function(x,y,...){
         panel.bwplot(x,y,...)
       })
```

![](bwplot_files/figure-markdown_github/unnamed-chunk-5-1.png)

### Annotate

``` r
pal   <- c("gray35","gray50","gray65","gray80","white")
ind   <- c(1,0.1,0.05,0.01,0.001,0)

bwplot(x~g, data=dd2,
       col='gray70', cex=1, pch='|',
       horizontal = F,
       scales=list(draw=F), 
       xlab=NULL, ylab=NULL,
       par.settings = list(box.rectangle=list(col="black",fill="#DFDFDF",lwd=1,lty=1),
                           box.umbrella=list(col="black",lwd=1,lty=1),
                           plot.symbol=list(pch=151,cex=1,col="black"),
                           axis.line = list(col = 0)),
          legend = list(top=list(fun=grid::textGrob("P-Value", x=1.06)),
                        right = list(fun = draw.colorkey,
                                     args = list(key = list(col = rev(pal), at = seq(0,1,length=6), 
                                                            labels=list(at=seq(0,1,length=6),labels=ind)), 
                                                 draw = FALSE))),
          panel = function(x,y,...){
            
            t <- t.test(y~x)
            m <- tapply(y,x, median)
            e <- format(t$estimate, digits = 1, nsmall = 1)
            i <- format(t$conf.int, digits = 1, nsmall = 1)
            d <- format(diff(t$estimate), digits = 1, nsmall = 1)
            
            col.ind <- cut(t$p.value,breaks=ind,labels=FALSE)               
            panel.fill(col = pal[col.ind])

            panel.bwplot(x,y,...)

            panel.xyplot(c(1,2),m, type=c("a"), col='black')
            
            grid.text(e,x=unit(c(1,2),"native"),y=unit(m,"native"),hjust=0.5,vjust=-0.5,gp=gpar(cex=0.7)) 
            
            grid.text(paste(d," (",i[1]," , ",i[2],") \n p ", pvalString(t$p.value),sep=""),
                      x=0.50,y=0.95,hjust=0.5,gp=gpar(cex=0.7,fontface=ifelse(t$p.value<0.05,2,1)))
            
            
          })
```

![](bwplot_files/figure-markdown_github/unnamed-chunk-6-1.png)

### Expand

``` r
bwplot(x~g |f, data=dd3,
       col='gray70', cex=1, pch='|',
       horizontal = F,
       scales=list(draw=F),
       between=list(x=0.5),
       xlab=NULL, ylab=NULL,
       par.settings = list(box.rectangle=list(col="black",fill="#DFDFDF",lwd=1,lty=1),
                           box.umbrella=list(col="black",lwd=1,lty=1),
                           plot.symbol=list(pch=151,cex=1,col="black"),
                           axis.line = list(col = 0),
                           strip.background=list(col="gray90")),
       legend = list(top=list(fun=grid::textGrob("P-Value", x=1.06)),
                     right = list(fun = draw.colorkey,
                                  args = list(key = list(col = rev(pal), at = seq(0,1,length=6), 
                                                         labels=list(at=seq(0,1,length=6),labels=ind)), 
                                              draw = FALSE))),
       panel = function(x,y,...){
         
         t <- t.test(y~x)
         m <- tapply(y,x, median)
         e <- format(t$estimate, digits = 1, nsmall = 1)
         i <- format(t$conf.int, digits = 1, nsmall = 1)
         d <- format(diff(t$estimate), digits = 1, nsmall = 1)
         
         col.ind <- cut(t$p.value,breaks=ind,labels=FALSE)               
         panel.fill(col = pal[col.ind])
         
         panel.bwplot(x,y,...)
         
         panel.xyplot(c(1,2),m, type=c("a"), col='black')
         
         grid.text(e,x=unit(c(1,2),"native"),y=unit(m,"native"),hjust=0.5,vjust=-0.5,gp=gpar(cex=0.7)) 
         
         grid.text(paste(d," (",i[1]," , ",i[2],") \n p ", pvalString(t$p.value),sep=""),
                   x=0.50,y=0.95,hjust=0.5,gp=gpar(cex=0.7,fontface=ifelse(t$p.value<0.05,2,1)))
         
         
       })
```

![](bwplot_files/figure-markdown_github/unnamed-chunk-7-1.png)
