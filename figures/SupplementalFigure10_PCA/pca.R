#' ---
#' title: PCA
#' author:  A Calatroni & J Wildfire
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'    github_document:
#'      toc: true
#' ---

#' ### setup 
knitr::opts_knit$set(root.dir = '../..')
knitr::opts_chunk$set(warning = FALSE, message = FALSE, comment = NA)

#' ### packages
pacman::p_load(tidyverse, rio)
pacman::p_load(lattice, latticeExtra, grid)
pacman::p_load(colorspace)
pacman::p_load(reshape2)
pacman::p_load(ggfortify)

#' ### citation
citation("ggfortify", auto = FALSE) %>% 
  toBibtex()

#' ### import data
dd <- import("./data/derive/egg_baso_data.rds") %>% 
  dcast(subject_accession + trttrue + visno ~ label)

#' ### subset data
dd_10 <- dd %>% 
  filter(visno=="Month 10") %>% 
  na.omit() %>% 
  select(1:8,10)

names(dd_10)[4:7] <- c("1 mcg/mL \nEgg CD63pos","0.1  mcg/mL \nEgg CD63pos",
                       "0.01  mcg/mL \nEgg CD63pos","0.001  mcg/mL \nEgg CD63pos")

#' ### pca
prcomp(dd_10[,-c(1:3)], scale. = TRUE) %>% summary()

#' ### pca figure
#+ fig.height=6, fig.width=7
autoplot( prcomp(dd_10[,-c(1:3)], scale. = TRUE), data=dd_10 , colour='trttrue',
          frame = TRUE,
          frame.type = 't',
          frame.level = 0.80,
          loadings = TRUE, 
          loadings.colour = 'gray50',
          loadings.label = TRUE,
          loadings.label.colour = 'gray50',
          loadings.label.size = 3,
          loadings.label.hjust = 0, 
          loadings.label.vjust = 0.5) +
  scale_color_brewer(palette="Set1") +
  labs(title = "Biplot Month 10",
       color = "Treatment Group",
       x = "First PC (58% explained var.)",
       y = "Second PC (18%)") +
  guides(fill = FALSE) +
  theme_minimal()

