#' ---
#' title: ColorBrewer
#' author:  A Calatroni & J Wildfire
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'    github_document:
#'      toc: true
#' ---

#' ### packages
pacman::p_load(RColorBrewer)

#' There are 3 types of palettes, sequential, diverging, and qualitative:  
#' * **Sequential palettes** are suited to ordered data that progress from low to high. 
#' Lightness steps dominate the look of these schemes, with light colors for low data 
#' values to dark colors for high data values.   
#' * **Diverging palettes** put equal emphasis on mid-range critical values and extremes 
#' at both ends of the data range. The critical class or break in the middle of the 
#' legend is emphasized with light colors and low and high extremes are emphasized 
#' with dark colors that have contrasting hues.   
#' * **Qualitative palettes** do not imply magnitude differences between legend classes, 
#' and hues are used to create the primary visual differences between classes. 
#' Qualitative schemes are best suited to representing nominal or categorical data.  
#' 
#' [ColorBrewer: Color Advice](http://colorbrewer2.org)
#'
#'
#' ### All
#+ fig.height=15, fig.width=5
display.brewer.all()

#' ### Diverging palettes
#+ fig.height=5, fig.widtht=5
display.brewer.all(type="div")

#' ### Qualitative palettes
#+ fig.height=5, fig.width=5
display.brewer.all(type="qual")

#' ### Sequential palettes
#+ fig.height=5, fig.width=7
display.brewer.all(type="seq")

#' ### test for color blindness
#+ eval = FALSE
pacman::p_load(tmaptools)
palette_explorer()