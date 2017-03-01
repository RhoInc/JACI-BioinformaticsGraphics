## Overview
This repository contains supplemental materials for "Graphic Depiction of Bioinformatics Data" paper in the Journal of Allergy and Clinical Immunology including source code, data and results for many of the figures used in the manuscript. 

## Data
We downloaded clinical and flow cytometry data from the Oral Immunotherapy for Treatment (OIT) of Egg Allergy in Children study ([NEJM Manuscript](http://www.nejm.org/doi/full/10.1056/nejmoa1200435), [NEJM Supplement](http://www.nejm.org/doi/suppl/10.1056/NEJMoa1200435/suppl_file/nejmoa1200435_appendix.pdf)) through the National Institutes for Allergy and Infectious Diseases (NIAID) data sharing site, [ImmPort](http://www.immport.org/immport-open/public/study/study/displayStudyDetail/SDY218) and saved it [here](https://github.com/RhoInc/JACI-BioinformaticsGraphics/tree/master/figures). We then wrote code ([1](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/data/derive/egg_baso_data.R), [2](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/data/derive/egg_flow_data.RDS)) to modify the Immport data to create data sets more suited for certain plots, and saved the output ([1](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/data/derive/egg_baso_data.RDS), [2](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/data/derive/egg_flow_data.RDS)).  

## Figures
Materials for 9 of the 12 figures in the manuscript are included [here](https://github.com/RhoInc/JACI-BioinformaticsGraphics/tree/master/figures). 

### Supplemental Figure 1 - Data table and paneled scatter plot for Anscombe’s quartet

Report combining [code and results](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/figures/SupplementalFigure01_anscombe/anscombe.md) for the figure. 

### Supplemental Figure 2 - Sample workflow for visualizing bioinformatics data

Report combining [code and results](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/figures/SupplementalFigure02_WorkflowExamples/histograms/histograms.md) for the Histogram example in the manuscript. Additional reports combining code and results for examples using:
- [Box and Whisker plots](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/figures/SupplementalFigure02_WorkflowExamples/bwplot/bwplot.md)
- [Density plots](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/figures/SupplementalFigure02_WorkflowExamples/densityplot/densityplot.md)
- [Dot plots](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/figures/SupplementalFigure02_WorkflowExamples/dotplot/dotplot.md)

### Supplemental Figure 4 - Color Brewer palettes for effective color selection

Report combining [code and results](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/figures/SupplementalFigure04_ColorBrewer/ColorBrewer.md) for the figure.
 
### Supplemental Figure 5 - Color Brewer palettes for effective color selection
 
The R Code to initialize the interactive tool is included in the last section of this [file](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/figures/SupplementalFigure04_ColorBrewer/ColorBrewer.md). Simply load the `tmaptools` R library (`pacman::p_load(tmaptools)`) and then call the `palette_explorer()` method. 

### Supplemental Figure 6: Visualization Case Study for Oral Immunotherapy

Report using combining [code and results](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/figures/SupplementalFigure06_ClinicalCaseStudy/clinical.md) for the figure created using Immport OIT Data.
 
### Supplemental Figure 7 - Methods for Addressing Overplotting using Egg Oral Immunotherapy data

Report using combining [code and results](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/figures/SupplementalFigure07_Overplotting/flow.md) for the figure created using Immport OIT Data.

### Supplemental Figure 8 - Sample Scatterplot Matrix with Egg Oral Immunotherapy data

Report using combining [code and results](https://github.com/RhoInc/JACI-BioinformaticsGraphics/tree/master/figures/SupplementalFigure08_ScatterPlotMatrix) for the figure created using Immport OIT Data.

### Supplemental Figure 9 - Sample Heatmap with Egg Oral Immunotherapy data

Report using combining [code and results](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/figures/SupplementalFigure09_Heatmap/levelplot.md) for the figure created using Immport OIT Data.

### Supplemental Figure 10 – Demonstration of Dimension Reduction Techniques with Egg Oral Immunotherapy data

Report using combining [code and results](https://github.com/RhoInc/JACI-BioinformaticsGraphics/blob/master/figures/SupplementalFigure10_PCA/pca.md) for the figure created using Immport OIT Data.
