### Growth Rate codes

These folders contain various scripts I have written to analyze growth-rate data measured from 96-well plates

Analysis is currently done primarily through R-markdown files "gc_analysis-template.Rmd" and "epi_analysis-template.Rmd". "GC-analysis" first analyzes each **g**rowth **c**urve and exracts its features (lag, max growth rate, maxOD), and "Epi-analysis" is run on the output from GC-analysis to identify significant phenotype changes in each strain, and to estimate whether there is any evidence of epistatic interactions among the double-mutants based on the phenotypes of the relevant single-mutants.

Growth curve analysis relies on the [**grofit** package](https://cran.r-project.org/web/packages/grofit/index.html)

Folders 1_gc_analysis_scripts / 2_epi_analysis_scripts hold .R files that mostly hold functions that the .Rmd files call

Folders "cssS_X_X" hold examples of experimental data that have been processed using these scripts.  'cssS' denotes the growth rates are of a set of strains that include the cssS-deletion strain, grown in LB media, DSM, acidified LB (pH5), or LB + 0.008% Triton-X detergent.

Since GitHub does not render html files automatically, here are links to rendered versions of some example files:

###Example Html files:

[LB_pH5 Growth Curve Analysis](http://htmlpreview.github.io/?https://github.com/mkapplebee/Growth_Rate_codes/blob/master/cssS_LBpH5/LB_pH5_gc_analysis.html)    
[LB_pH5 Epistasis Analysis](http://htmlpreview.github.io/?https://github.com/mkapplebee/Growth_Rate_codes/blob/master/cssS_LBpH5/LB_pH5_epi_analysis.html)
